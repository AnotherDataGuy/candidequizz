# Bilan de fin de séance.
#
# Score, temps, thèmes, types, confusions, conseil A-8, et exports F-34.

#' Interface du bilan
#'
#' @param id Identifiant du module.
#'
#' @return Un élément d'interface.
#' @noRd
mod_seance_bilan_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-bilan cq-stats",
		shiny::uiOutput(ns("rapport"))
	)
}

#' Durée lisible
#'
#' @param secondes Un nombre de secondes.
#'
#' @return Une chaîne de caractères.
#' @noRd
seance_bilan_duree <- function(secondes) {
	if (!is.finite(secondes)) {
		return("\u2014")
	}

	minutes <- floor(secondes / 60)
	reste <- round(secondes - minutes * 60)

	if (minutes == 0) {
		return(glue::glue("{reste} s"))
	}

	glue::glue("{minutes} min {reste} s")
}

#' Synthèse par sous-thème
#'
#' @param reponses Tableau des réponses.
#'
#' @return Un tibble trié du sous-thème le moins réussi au mieux réussi.
#' @noRd
seance_bilan_par_theme <- function(reponses) {
	if (!nrow(reponses)) {
		return(tibble::tibble(
			sub_theme = character(0),
			n = integer(0),
			reussite = numeric(0)
		))
	}

	decoupage <- split(reponses, reponses$sub_theme)

	lignes <- lapply(names(decoupage), function(nom) {
		bloc <- decoupage[[nom]]

		tibble::tibble(
			sub_theme = nom,
			n = nrow(bloc),
			reussite = mean(bloc$score)
		)
	})

	tableau <- do.call(rbind, lignes)

	tableau[order(tableau$reussite), , drop = FALSE]
}

#' Portée du bilan : numéro de la séance et comparaison à la moyenne
#'
#' Situe la séance face aux séances précédentes du même compte, pour que le
#' bilan se distingue clairement de la Progression (toutes les séances).
#'
#' @param s Séance.
#' @param liste Résultat de `store_lister()` pour ce compte (ou NULL).
#' @param score_pct Score de la séance, en %.
#' @param total Nombre de réponses de la séance.
#'
#' @return Liste : `detail` (texte du bandeau) et `comparaison` (texte ou NULL).
#' @noRd
bilan_portee <- function(s, liste, score_pct, total) {
	profils <- seance_config_profils()
	profil <- names(profils)[match(as.character(s$parametres$profil %||% "libre"), profils)]
	date <- if (!is.null(s$cree_le)) format(as.POSIXct(s$cree_le), "%d/%m/%Y") else NULL

	numero <- NA_integer_
	precedentes <- NULL
	if (!is.null(liste) && nrow(liste)) {
		liste <- liste[order(liste$cree_le), , drop = FALSE]
		numero <- match(s$session_id, liste$session_id)
		avant <- if (is.na(numero)) liste else liste[seq_len(numero - 1L), , drop = FALSE]
		precedentes <- avant[avant$n_repondues > 0 & is.finite(avant$score), , drop = FALSE]
	}

	detail <- paste(
		c(
			if (!is.na(numero)) paste0("n\u00b0 ", numero),
			if (!is.na(profil)) profil,
			date
		),
		collapse = " \u00b7 "
	)

	comparaison <- if (!total) {
		NULL
	} else if (is.null(precedentes) || !nrow(precedentes)) {
		"Premi\u00e8re s\u00e9ance enregistr\u00e9e : la comparaison viendra avec la suivante."
	} else {
		moyenne <- round(100 * sum(precedentes$score * precedentes$n_repondues) / sum(precedentes$n_repondues))
		ecart <- score_pct - moyenne
		n <- nrow(precedentes)
		position <- if (ecart > 0) {
			glue::glue("{ecart} {if (ecart == 1) 'point' else 'points'} au-dessus de votre moyenne")
		} else if (ecart < 0) {
			glue::glue("{-ecart} {if (ecart == -1) 'point' else 'points'} en dessous de votre moyenne")
		} else {
			"Exactement votre moyenne"
		}
		glue::glue(
			"{position} ({moyenne} % sur {n} s\u00e9ance{if (n > 1) 's' else ''} ",
			"pr\u00e9c\u00e9dente{if (n > 1) 's' else ''})."
		)
	}

	list(detail = detail, comparaison = comparaison)
}

#' Serveur du bilan
#'
#' @param id Identifiant du module.
#' @param seance Réactif renvoyant la séance achevée.
#' @param rejouer Fonction appelée pour repartir sur une nouvelle séance.
#'
#' @return Rien, appelé pour ses effets.
#' @noRd
mod_seance_bilan_server <- function(id, seance, rejouer, utilisateur = shiny::reactive(NULL)) {
	shiny::moduleServer(id, function(input, output, session) {
		shiny::observeEvent(seance(), {
			s <- seance()
			if (is.null(s) || !length(s$reponses) || !seance_closee(s)) {
				return(invisible(NULL))
			}

			if (synthetiseur_a_jour(s)) {
				return(invisible(NULL))
			}

			completee <- tryCatch(
				shiny::withProgress(
					message = "R\u00e9daction de votre bilan",
					value = 0.4,
					synthetiseur_completer(s)
				),
				error = function(e) {
					s$synthese <- synthetiseur_repli(stats_snapshot_seance(s))
					s$synthese$n_reponses <- length(s$reponses)
					s
				}
			)

			seance(completee)
			u <- utilisateur()
			store_sauver(completee, user_id = if (is.null(u)) completee$user_id else u$user_id)
		}, ignoreNULL = TRUE)

		tableau <- shiny::reactive({
			s <- seance()
			shiny::req(s)

			reponses_tibble(s$reponses)
		})

		output$rapport <- shiny::renderUI({
			s <- seance()
			shiny::req(s)
			r <- tableau()
			justes <- if (nrow(r)) sum(r$is_correct) else 0L
			total <- nrow(r)
			analyse <- if (nrow(r)) stats_temps(r) else NULL
			par_sous <- if (nrow(r)) stats_par(r, "sub_theme") else NULL
			par_theme <- if (nrow(r)) stats_par(r, "theme") else NULL
			par_type <- if (nrow(r)) stats_par(r, "type") else NULL
			if (!is.null(par_type) && nrow(par_type)) {
				par_type$cle <- vapply(par_type$cle, function(id) {
					if (id %in% question_type_ids()) question_type(id)$libelle else id
				}, character(1))
			}
			par_diff_graine <- if (nrow(r) && "difficulte_graine" %in% names(r)) {
				rg <- r[!is.na(r$difficulte_graine), , drop = FALSE]
				if (nrow(rg)) {
					rg$bande <- bank_difficulte_bande(rg$difficulte_graine)
					stats_par(rg, "bande")
				} else {
					NULL
				}
			} else {
				NULL
			}
			conf <- stats_confusions(s$reponses)
			syn <- s$synthese
			titre <- if (isTRUE(s$chrono_epuise)) {
				"Temps \u00e9coul\u00e9"
			} else if (identical(s$statut, "abandonnee")) {
				"S\u00e9ance interrompue"
			} else {
				"S\u00e9ance termin\u00e9e"
			}
			chapeau <- if (isTRUE(s$chrono_epuise) && total) {
				glue::glue(
					"Le chronom\u00e8tre est arriv\u00e9 \u00e0 z\u00e9ro. {justes} bonnes r\u00e9ponses sur {total}."
				)
			} else if (isTRUE(s$chrono_epuise)) {
				"Le chronom\u00e8tre est arriv\u00e9 \u00e0 z\u00e9ro avant la premi\u00e8re r\u00e9ponse."
			} else if (total) {
				glue::glue(
					"{justes} {if (justes == 1L) 'bonne r\u00e9ponse' else 'bonnes r\u00e9ponses'} ",
					"sur {total}."
				)
			} else {
				"Aucune question n'a encore \u00e9t\u00e9 valid\u00e9e."
			}
			# Examen blanc : dire clairement si le seuil officiel est atteint.
			if (identical(as.character(s$parametres$profil %||% ""), "examen_blanc") && total) {
				seuil <- as.integer(cq_config("seance", "examen_seuil"))
				chapeau <- paste(
					chapeau,
					if (justes >= seuil) {
						glue::glue("Seuil de r\u00e9ussite atteint ({seuil} bonnes r\u00e9ponses requises).")
					} else {
						glue::glue("Seuil de r\u00e9ussite non atteint : il en faut {seuil}.")
					}
				)
			}
			score_pct <- if (total) as.integer(round(100 * mean(r$score))) else 0L
			meta_temps <- if (!is.null(analyse) && is.finite(analyse$median_s)) {
				glue::glue("Temps m\u00e9dian : {round(analyse$median_s)} s")
			} else {
				NULL
			}
			syn_forts <- if (is.list(syn)) syn$points_forts else NULL
			syn_faibles <- if (is.list(syn)) syn$points_a_travailler else NULL
			syn_plan <- if (is.list(syn)) syn$plan_revision else NULL
			syn_diag <- if (is.list(syn)) syn$diagnostic else NULL

			duree_totale <- if (total && any(is.finite(r$duree_s))) {
				seance_bilan_duree(sum(r$duree_s[is.finite(r$duree_s)]))
			} else {
				"\u2014"
			}

			questions <- explore_revoir_entrees(
				list(s),
				limite = Inf,
				inclure_justes = TRUE,
				ordre = "seance"
			)
			n_ecarts <- sum(vapply(questions, function(e) !identical(e$issue, "juste"), logical(1)))

			essentiel <- shiny::tagList(
				stats_report_essentiel(
					score_pct = score_pct,
					n_reponses = total,
					diagnostic = syn_diag %||% chapeau,
					meta_ligne = meta_temps,
					scope = "bilan",
					suite = NULL
				),
				stats_report_kpis(
					score_pct = score_pct,
					n_justes = justes,
					n_reponses = total,
					n_seances = 1L,
					temps_median_s = if (!is.null(analyse)) analyse$median_s else NA_real_,
					troisieme_valeur = duree_totale,
					troisieme_libelle = "Dur\u00e9e"
				),
				if (length(questions)) {
					shiny::div(
						class = "report-panel report-parcours",
						shiny::div(
							class = "panel-title",
							shiny::div(
								shiny::tags$h2("Question par question"),
								shiny::tags$p(
									if (n_ecarts) {
										glue::glue(
											"{n_ecarts} question(s) \u00e0 reprendre. Touchez-en une pour lire la correction."
										)
									} else {
										"Un sans-faute. Touchez une question pour relire son explication."
									}
								)
							)
						),
						explore_parcours(questions)
					)
				},
				shiny::div(
					class = "report-context",
					shiny::tags$span(
						glue::glue(
							"{total} {if (total == 1L) 'r\u00e9ponse' else 'r\u00e9ponses'} dans cette s\u00e9ance",
							if (!is.null(s$libelle) && nzchar(s$libelle)) {
								glue::glue(" \u00b7 {s$libelle}")
							} else {
								""
							}
						)
					)
				),
				stats_report_next_strip(syn_forts, syn_faibles, syn_plan)
			)

			comprendre <- if (!total) {
				explore_vide("Pas encore de r\u00e9ponses \u00e0 analyser.")
			} else {
				shiny::div(
					class = "explore",
					explore_chapitre(
						"01",
						"Par sujet",
						"Ce que cette s\u00e9ance dit de vos acquis, du plus fragile au plus solide.",
						class = "explore-grid",
						explore_carte(
							"Grands th\u00e8mes",
							"R\u00e9ussite sur cette s\u00e9ance.",
							repere = glue::glue("{nrow(par_theme)} th\u00e8me(s)"),
							explore_rangs(par_theme, limite = 8L)
						),
						explore_carte(
							"Sous-th\u00e8mes",
							"Le d\u00e9tail, sujet par sujet.",
							repere = glue::glue("{nrow(par_sous)} sujet(s)"),
							explore_rangs(par_sous, limite = 8L)
						),
						explore_legende_seuil()
					),
					explore_chapitre(
						"02",
						"Question par question",
						"Le temps pass\u00e9 sur chaque question, et son issue. Une longue h\u00e9sitation signale souvent un doute.",
						explore_carte(
							"Temps de r\u00e9ponse",
							"Dans l'ordre de passation. Le trait pointill\u00e9 marque votre temps m\u00e9dian.",
							explore_temps_questions(questions)
						)
					),
					explore_chapitre(
						"03",
						"Format et r\u00e9flexes",
						"Les formats qui vous r\u00e9ussissent, la difficult\u00e9 rencontr\u00e9e, et les pi\u00e8ges.",
						class = "explore-grid",
						explore_carte(
							"Types de questions",
							"QCM, vrai/faux, ordre, texte \u00e0 trous\u2026",
							explore_rangs(par_type, limite = 6L)
						),
						explore_carte(
							"Difficult\u00e9 des questions",
							"Cote de la banque, sur 10.",
							if (!is.null(par_diff_graine) && nrow(par_diff_graine)) {
								explore_colonnes(
									par_diff_graine,
									niveaux = c("1\u20132", "3\u20134", "5\u20136", "7\u20138", "9\u201310")
								)
							} else {
								explore_vide("Pas encore de difficult\u00e9 de question enregistr\u00e9e.")
							}
						),
						explore_carte(
							"Rythme",
							"R\u00e9ussite de part et d'autre de votre temps m\u00e9dian.",
							explore_rythme(analyse)
						),
						explore_carte(
							"Confusions",
							"Les pi\u00e8ges rencontr\u00e9s dans cette s\u00e9ance.",
							explore_confusions(conf, limite = 5L)
						)
					)
				)
			}

			revoir <- shiny::div(
				class = "explore explore--revoir",
				shiny::tags$section(
					class = "revoir-main",
					shiny::tags$header(
						class = "explore-chapter-head",
						shiny::span(class = "explore-num", "01"),
						shiny::div(
							shiny::tags$h2("Toutes les questions"),
							shiny::tags$p(
								"Les erreurs sont d\u00e9pli\u00e9es. Ouvrez une question juste pour relire son explication."
							)
						)
					),
					explore_revoir_deck(
						questions,
						visibles = max(length(questions), 1L),
						numeroter = TRUE,
						ouvrir = c("fausse", "partielle"),
						vide = "Aucune question n'a \u00e9t\u00e9 valid\u00e9e dans cette s\u00e9ance."
					)
				)
			)

			u <- utilisateur()
			portee <- bilan_portee(
				s,
				liste = tryCatch(
					store_lister(user_id = if (is.null(u)) s$user_id else u$user_id),
					error = function(e) NULL
				),
				score_pct = score_pct,
				total = total
			)

			shiny::tagList(
				stats_report_portee("seance", "Cette s\u00e9ance", portee$detail),
				shiny::div(
					class = "heading report-heading",
					shiny::div(
						shiny::tags$p(class = "eyebrow muted", titre),
						shiny::tags$h1("Ce que vous emportez."),
						shiny::tags$p(
							class = "muted",
							style = "margin-top:12px",
							chapeau
						),
						if (!is.null(portee$comparaison)) {
							shiny::tags$p(
								class = "report-comparaison",
								portee$comparaison,
								" ",
								shiny::tags$button(
									type = "button",
									class = "textbtn",
									onclick = "Shiny.setInputValue('nav_statistiques', Math.random());",
									"Voir ma progression \u2192"
								)
							)
						}
					),
					shiny::div(
						class = "report-heading-tools",
						shiny::actionButton(
							session$ns("nouvelle"),
							label = shiny::tagList("Nouvelle s\u00e9ance", cq_icon("i-arrow")),
							class = "btn primary"
						)
					)
				),
				shiny::div(
					class = "report-toolbar",
					shiny::div(
						class = "source-label",
						shiny::tags$span(class = "dot"),
						"Cette s\u00e9ance \u00b7 enregistr\u00e9e sur ce serveur, pour votre compte"
					),
					shiny::div(
						class = "report-exports",
						shiny::downloadButton(
							session$ns("dl_csv"), "CSV", class = "textbtn", icon = shiny::icon(NULL)
						),
						shiny::downloadButton(
							session$ns("dl_xlsx"), "Excel", class = "textbtn", icon = shiny::icon(NULL)
						),
						shiny::downloadButton(
							session$ns("dl_html"), "HTML", class = "textbtn", icon = shiny::icon(NULL)
						)
					)
				),
				stats_report_shell(
					essentiel = essentiel,
					comprendre = comprendre,
					revoir = revoir,
					onglets = c("R\u00e9sultat", "Mes erreurs", "Corrig\u00e9"),
					footnote = paste(
						"Ce bilan d\u00e9crit uniquement cette s\u00e9ance.",
						"Consultez Progression pour suivre l'\u00e9volution entre plusieurs s\u00e9ances."
					)
				)
			)
		})

		shiny::observeEvent(input$nouvelle, {
			rejouer()
		})

		exporter <- function() {
			s <- seance()
			shiny::req(s)
			seance_export_tableau(s)
		}

		output$dl_csv <- shiny::downloadHandler(
			filename = function() paste0("candidequizz-", seance()$session_id, ".csv"),
			content = function(file) stats_export_csv(exporter(), file)
		)
		output$dl_xlsx <- shiny::downloadHandler(
			filename = function() paste0("candidequizz-", seance()$session_id, ".xlsx"),
			content = function(file) stats_export_xlsx(exporter(), file, seance = TRUE)
		)
		output$dl_html <- shiny::downloadHandler(
			filename = function() paste0("candidequizz-", seance()$session_id, ".html"),
			content = function(file) {
				s <- seance()
				shiny::req(s)
				r <- reponses_tibble(s$reponses)
				stats_export_html(
					titre = s$libelle,
					synthese = stats_synthese(r, 1L),
					par_theme = stats_par(r, "theme"),
					par_sous_theme = stats_par(r, "sub_theme"),
					par_type = stats_par(r, "type"),
					chemin = file
				)
			}
		)

		invisible(NULL)
	})
}
