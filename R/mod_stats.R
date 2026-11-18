# Section « Progression » (statistiques).

#' Statistiques — interface
#'
#' @param id Identifiant du module.
#'
#' @return Un tag HTML.
#' @noRd
mod_stats_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-stats",
		shiny::uiOutput(ns("portee")),
		shiny::div(
			class = "heading report-heading",
			shiny::div(
				shiny::p(class = "eyebrow muted", "Le carnet de progression"),
				shiny::tags$h1("Comprendre ce qui avance."),
				shiny::p(
					class = "muted",
					style = "margin-top:12px",
					"Un regard sur vos acquis. Des pistes pour la suite."
				)
			),
			shiny::div(
				class = "report-heading-tools",
				shiny::downloadButton(
					ns("dl_csv"),
					label = "Exporter CSV \u2193",
					class = "btn",
					icon = shiny::icon(NULL)
				)
			)
		),
		shiny::div(
			class = "report-toolbar",
			shiny::div(
				class = "source-label",
				shiny::tags$span(class = "dot"),
				"Vos donn\u00e9es \u00b7 li\u00e9es \u00e0 votre compte, sur ce serveur"
			),
			shiny::div(
				class = "report-filters",
				shiny::div(
					class = "segments cq-champ",
					shiny::radioButtons(
						ns("perimetre"),
						label = "P\u00e9rim\u00e8tre",
						choices = c(
							"Tout" = "toutes",
							"S\u00e9lection" = "selection"
						),
						selected = "toutes",
						inline = TRUE
					)
				),
				shiny::conditionalPanel(
					condition = sprintf("input['%s'] == 'selection'", ns("perimetre")),
					shiny::selectizeInput(
						ns("seances"),
						label = "S\u00e9ances",
						choices = NULL,
						multiple = TRUE,
						width = "240px",
						options = list(placeholder = "Choisir des s\u00e9ances")
					)
				),
				shiny::actionButton(ns("rafraichir"), "Actualiser", class = "textbtn")
			)
		),
		shiny::uiOutput(ns("rapport")),
		shiny::div(
			class = "report-exports topspace",
			shiny::downloadButton(
				ns("dl_xlsx"),
				"Excel",
				class = "textbtn",
				icon = shiny::icon(NULL)
			),
			shiny::downloadButton(
				ns("dl_html"),
				"HTML",
				class = "textbtn",
				icon = shiny::icon(NULL)
			)
		)
	)
}

#' Choix du s\u00e9lecteur de s\u00e9ances
#' @noRd
stats_choix_seances <- function(liste) {
	if (!nrow(liste)) {
		return(character(0))
	}

	stats::setNames(liste$session_id, liste$libelle)
}

#' Pourcentage lisible
#' @noRd
stats_pct <- function(x) {
	if (!is.finite(x)) {
		return("\u2014")
	}

	glue::glue("{round(100 * x)} %")
}

#' \u00c9cart en points
#' @noRd
stats_delta_txt <- function(x) {
	if (!is.finite(x)) {
		return(NULL)
	}

	signe <- if (x >= 0) "+" else ""
	glue::glue("{signe}{round(100 * x)} pts")
}

#' Piste d'interpr\u00e9tation
#' @noRd
stats_piste <- function(kicker, texte, ton = NULL) {
	shiny::div(
		class = paste(c("cq-piste", if (!is.null(ton)) paste0("cq-piste--", ton)), collapse = " "),
		shiny::span(class = "cq-piste-kicker", kicker),
		shiny::span(class = "cq-piste-texte", texte)
	)
}

#' Bandeau de lecture du tableau de bord
#' @noRd
stats_lecture_ui <- function(lecture) {
	pistes <- list()

	if (!is.null(lecture$faible) && nrow(lecture$faible)) {
		pistes <- c(pistes, list(stats_piste(
			"\u00c0 revoir",
			glue::glue(
				"{lecture$faible$cle[[1L]]} \u00b7 {stats_pct(lecture$faible$reussite[[1L]])}"
			),
			"moins"
		)))
	}

	if (!is.null(lecture$fort) && nrow(lecture$fort) &&
			!identical(lecture$fort$cle[[1L]], lecture$faible$cle[[1L]])) {
		pistes <- c(pistes, list(stats_piste(
			"Solide",
			glue::glue(
				"{lecture$fort$cle[[1L]]} \u00b7 {stats_pct(lecture$fort$reussite[[1L]])}"
			),
			"plus"
		)))
	}

	if (!is.null(lecture$progres) && nrow(lecture$progres)) {
		pistes <- c(pistes, list(stats_piste(
			"En hausse",
			glue::glue(
				"{lecture$progres$cle[[1L]]} \u00b7 {stats_delta_txt(lecture$progres$delta[[1L]])}"
			),
			"plus"
		)))
	}

	if (!is.null(lecture$faible_type) && nrow(lecture$faible_type) &&
			is.finite(lecture$faible_type$reussite[[1L]]) &&
			lecture$faible_type$reussite[[1L]] < 0.7) {
		lib <- lecture$faible_type$libelle %||% lecture$faible_type$cle[[1L]]
		pistes <- c(pistes, list(stats_piste(
			"Type fragile",
			glue::glue("{lib} \u00b7 {stats_pct(lecture$faible_type$reussite[[1L]])}")
		)))
	}

	if (is.finite(lecture$tendance) && abs(lecture$tendance) >= 0.03) {
		pistes <- c(pistes, list(stats_piste(
			"Depuis le d\u00e9but",
			stats_delta_txt(lecture$tendance),
			if (lecture$tendance >= 0) "plus" else "moins"
		)))
	}

	if (lecture$n_passees > 0L) {
		pistes <- c(pistes, list(stats_piste(
			"Pass\u00e9es",
			glue::glue("{lecture$n_passees}")
		)))
	}

	if (!length(pistes)) {
		return(NULL)
	}

	shiny::div(class = "cq-stats-pistes", pistes)
}

#' Story copy enrichie avec les points faibles / forts
#' @noRd
stats_report_diagnostic <- function(lecture, score_pct) {
	faible <- if (!is.null(lecture$faible) && nrow(lecture$faible)) {
		lecture$faible$cle[[1L]]
	} else {
		NULL
	}
	fort <- if (!is.null(lecture$fort) && nrow(lecture$fort)) {
		lecture$fort$cle[[1L]]
	} else {
		NULL
	}

	if (!is.null(faible) && !is.null(fort) && !identical(faible, fort)) {
		return(glue::glue(
			"Vous tenez {fort} ({stats_pct(lecture$fort$reussite[[1L]])}). ",
			"Le point fragile reste {faible} ({stats_pct(lecture$faible$reussite[[1L]])}). ",
			"Une s\u00e9ance cibl\u00e9e sur ce th\u00e8me consolidera le parcours."
		))
	}

	if (!is.null(faible)) {
		return(glue::glue(
			"Le th\u00e8me le plus fragile est {faible} ",
			"({stats_pct(lecture$faible$reussite[[1L]])}). ",
			"Revenez-y avant d'\u00e9largir le programme."
		))
	}

	NULL
}

#' Statistiques — serveur
#' @noRd
mod_stats_server <- function(id, utilisateur = shiny::reactive(NULL), donnees_maj = shiny::reactive(0)) {
	shiny::moduleServer(id, function(input, output, session) {
		catalogue <- shiny::reactive({
			input$rafraichir
			donnees_maj()
			u <- utilisateur()
			shiny::req(u)
			store_lister(user_id = if (is.null(u)) NULL else u$user_id)
		})

		shiny::observe({
			shiny::updateSelectizeInput(
				session,
				"seances",
				choices = stats_choix_seances(catalogue()),
				server = TRUE
			)
		})

		seances <- shiny::reactive({
			liste <- catalogue()
			ids <- if (identical(input$perimetre, "selection")) {
				input$seances
			} else {
				liste$session_id
			}

			u <- utilisateur()
			stats_charger(ids, user_id = if (is.null(u)) NULL else u$user_id)
		})

		reponses <- shiny::reactive({
			stats_reponses(seances())
		})

		# Bandeau de portée : toutes les séances, ou la sélection.
		output$portee <- shiny::renderUI({
			liste <- catalogue()
			liste <- liste[liste$n_repondues > 0, , drop = FALSE]
			n <- nrow(liste)
			pluriel <- function(k) paste0(k, " s\u00e9ance", if (k > 1) "s" else "")

			if (identical(input$perimetre, "selection")) {
				k <- length(intersect(input$seances, liste$session_id))
				return(stats_report_portee("carnet", "S\u00e9lection", glue::glue("{pluriel(k)} sur {n}")))
			}

			detail <- if (n) {
				glue::glue("{pluriel(n)} depuis le {format(min(liste$cree_le), '%d/%m/%Y')}")
			} else {
				"aucune s\u00e9ance pour l'instant"
			}
			stats_report_portee("carnet", "Toutes vos s\u00e9ances", detail)
		})

		output$rapport <- shiny::renderUI({
			r <- reponses()
			s <- seances()
			liste <- catalogue()

			if (!nrow(r)) {
				return(shiny::div(
					class = "report-empty",
					shiny::tags$h2("Pas encore de traces."),
					shiny::tags$p(
						"Terminez une s\u00e9ance : le carnet de progression se remplit ici."
					),
					shiny::tags$button(
						type = "button",
						class = "btn primary",
						onclick = "Shiny.setInputValue('nav_seances', Math.random());",
						"Commencer une s\u00e9ance ",
						cq_icon("i-arrow")
					)
				))
			}

			lecture <- stats_lecture(r, s)
			syn <- lecture$synthese
			par_theme <- stats_par(r, "theme")
			par_sous <- stats_par(r, "sub_theme")
			par_type <- stats_par(r, "type")
			par_type$cle <- vapply(par_type$cle, function(id) {
				if (id %in% question_type_ids()) question_type(id)$libelle else id
			}, character(1))
			par_diff <- stats_par(r, "difficulte")
			par_diff_graine <- if ("difficulte_graine" %in% names(r)) {
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
			analyse <- lecture$temps
			brutes <- unlist(lapply(s, function(x) x$reponses), recursive = FALSE)
			conf <- stats_confusions(brutes)
			chaleur <- stats_chaleur(r)
			evo <- lecture$evolution
			libelles <- stats::setNames(stats_date_courte(evo$cree_le), evo$session_id)
			sessions <- utils::tail(unique(evo$session_id), 6L)
			frequents <- sort(
				table(r$sub_theme[r$session_id %in% sessions]),
				decreasing = TRUE
			)
			themes_carte <- utils::head(names(frequents), 8L)
			themes_carte <- par_sous$cle[par_sous$cle %in% themes_carte]
			reussite_sous <- stats::setNames(par_sous$reussite, par_sous$cle)

			score_pct <- if (is.finite(syn$reussite)) {
				as.integer(round(100 * syn$reussite))
			} else {
				0L
			}

			diag <- stats_report_diagnostic(lecture, score_pct)
			faible_lab <- if (!is.null(lecture$faible) && nrow(lecture$faible)) {
				lecture$faible$cle[[1L]]
			} else {
				NULL
			}
			fort_lab <- if (!is.null(lecture$fort) && nrow(lecture$fort)) {
				lecture$fort$cle[[1L]]
			} else {
				NULL
			}
			pistes_ui <- stats_lecture_ui(lecture)
			readiness <- readiness_estimer(s)

			essentiel <- shiny::tagList(
				stats_report_essentiel(
					score_pct = score_pct,
					n_reponses = nrow(r),
					diagnostic = diag,
					meta_ligne = glue::glue(
						"{syn$n_seances} s\u00e9ance(s) \u00b7 {lecture$n_justes} justes",
						if (is.finite(syn$temps_median_s)) {
							glue::glue(" \u00b7 m\u00e9diane {round(syn$temps_median_s)} s")
						} else {
							""
						}
					),
					scope = "stats",
					suite = NULL
				),
				stats_report_kpis(
					score_pct = score_pct,
					n_justes = lecture$n_justes,
					n_reponses = nrow(r),
					n_seances = syn$n_seances,
					temps_median_s = syn$temps_median_s,
					delta = lecture$delta
				),
				if (!is.null(pistes_ui)) {
					shiny::div(
						class = "report-panel report-pistes-strip",
						shiny::div(
							class = "panel-title",
							shiny::div(
								shiny::tags$h2("Pistes imm\u00e9diates"),
								shiny::tags$p("Ce que le carnet met en avant maintenant.")
							)
						),
						pistes_ui
					)
				},
				shiny::div(
					class = "report-context",
					shiny::tags$span(
						glue::glue(
							"{nrow(r)} r\u00e9ponses sur {syn$n_seances} s\u00e9ance(s)",
							if (nzchar(stats_delta_txt(lecture$delta) %||% "")) {
								glue::glue(" \u00b7 {stats_delta_txt(lecture$delta)} vs la pr\u00e9c\u00e9dente")
							} else {
								""
							}
						)
					)
				),
				readiness_ui(readiness),
				{
					composition_panel <- shiny::div(
						class = "report-panel report-panel--equal",
						shiny::div(
							class = "panel-title",
							shiny::div(
								shiny::tags$h2("Composition des r\u00e9ponses"),
								shiny::tags$p("Justes et fausses.")
							)
						),
						shiny::div(
							class = "report-panel-body report-panel-body--center",
							stats_report_outcome_ribbon(
								n_justes = lecture$n_justes,
								n_partielles = lecture$n_partielles,
								n_fausses = lecture$n_fausses,
								n_passees = lecture$n_passees
							)
						)
					)

					if (nrow(evo) > 0L) {
						shiny::div(
							class = "report-grid report-grid--duo",
							shiny::div(
								class = "report-panel report-panel--equal",
								shiny::div(
									class = "panel-title",
									shiny::div(
										shiny::tags$h2("\u00c9volution du score"),
										shiny::tags$p("Un point par s\u00e9ance, du plus ancien au plus r\u00e9cent.")
									)
								),
								shiny::div(
									class = "report-panel-body",
									stats_graphe_safe(stats_graphe_evolution(evo))
								)
							),
							composition_panel
						)
					} else {
						shiny::div(class = "report-grid report-grid--duo", composition_panel)
					}
				},
				shiny::div(
					class = "report-panel report-panel--peche",
					shiny::div(
						class = "panel-title",
						shiny::div(
							shiny::tags$h2("O\u00f9 \u00e7a p\u00eache"),
							shiny::tags$p("Grands th\u00e8mes les moins r\u00e9ussis, seuil \u00e0 55 %.")
						)
					),
					stats_report_ou_ca_peche(par_theme, limite = 6L, seuil = 0.55)
				),
				stats_report_next_strip(
					points_forts = fort_lab,
					points_faibles = faible_lab,
					plan = if (!is.null(faible_lab)) {
						glue::glue("Pr\u00e9parer une s\u00e9ance sur {faible_lab}.")
					} else {
						"Continuer \u00e0 votre rythme."
					},
					cta_label = "Pr\u00e9parer une s\u00e9ance"
				)
			)

			comprendre <- shiny::div(
				class = "explore",
				explore_chapitre(
					"01",
					"Par sujet",
					"O\u00f9 se situent vos acquis, du plus fragile au plus solide.",
					class = "explore-grid explore-grid--large",
					explore_carte(
						"Grands th\u00e8mes",
						"R\u00e9ussite moyenne sur l'ensemble du p\u00e9rim\u00e8tre.",
						repere = glue::glue("{nrow(par_theme)} th\u00e8me(s)"),
						explore_rangs(par_theme, limite = 8L)
					),
					explore_carte(
						"Types de questions",
						"QCM, vrai/faux, ordre, texte \u00e0 trous\u2026",
						repere = glue::glue("{nrow(par_type)} type(s)"),
						explore_rangs(par_type, limite = 6L)
					),
					explore_legende_seuil()
				),
				explore_chapitre(
					"02",
					"Par difficult\u00e9",
					"Votre r\u00e9ussite quand la barre monte. Un niveau vide n'a pas encore \u00e9t\u00e9 pratiqu\u00e9.",
					class = "explore-grid",
					explore_carte(
						"Niveau de s\u00e9ance",
						"Difficult\u00e9 adaptative, de 1 \u00e0 5.",
						explore_colonnes(par_diff, niveaux = as.character(1:5))
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
					)
				),
				explore_chapitre(
					"03",
					"Dans le temps",
					"Les sujets les plus travaill\u00e9s, s\u00e9ance apr\u00e8s s\u00e9ance.",
					explore_carte(
						"Carte des sous-th\u00e8mes",
						"R\u00e9ussite en % par s\u00e9ance r\u00e9cente. Un point signifie \u00ab non abord\u00e9 \u00bb, pas z\u00e9ro.",
						repere = glue::glue("{length(sessions)} s\u00e9ance(s)"),
						class = "explore-card--flush",
						stats_graphe_safe(explore_chaleur(
							chaleur,
							sessions,
							themes_carte,
							libelles,
							global = reussite_sous
						))
					),
					explore_carte(
						"Toutes vos r\u00e9ponses",
						"Une case par question, regroup\u00e9es par s\u00e9ance.",
						stats_graphe_safe(explore_mosaique(r, n_seances = 8L))
					)
				),
				explore_chapitre(
					"04",
					"Vos r\u00e9flexes",
					"Ce que votre rythme et vos erreurs r\u00e9currentes racontent.",
					class = "explore-grid",
					explore_carte(
						"Rythme",
						"R\u00e9ussite selon le temps pass\u00e9, de part et d'autre de la m\u00e9diane.",
						explore_rythme(analyse)
					),
					explore_carte(
						"Confusions r\u00e9currentes",
						"Les pi\u00e8ges dans lesquels vous tombez le plus souvent.",
						explore_confusions(conf, limite = 5L)
					)
				)
			)

			entrees <- explore_revoir_entrees(s, limite = 60L)
			fragiles <- par_sous[is.finite(par_sous$reussite) & par_sous$reussite < 0.75, , drop = FALSE]

			revoir <- shiny::div(
				class = "explore explore--revoir",
				shiny::div(
					class = "revoir-layout",
					shiny::tags$section(
						class = "revoir-main",
						shiny::tags$header(
							class = "explore-chapter-head",
							shiny::span(class = "explore-num", "01"),
							shiny::div(
								shiny::tags$h2("Questions \u00e0 reprendre"),
								shiny::tags$p(
									"Ouvrez une question pour relire votre r\u00e9ponse, la bonne r\u00e9ponse et l'explication."
								)
							)
						),
						explore_revoir_deck(entrees, visibles = 8L)
					),
					shiny::tags$aside(
						class = "revoir-side",
						explore_carte(
							"Sous-th\u00e8mes \u00e0 consolider",
							"Sous 75 % de r\u00e9ussite, du plus fragile au plus solide.",
							if (nrow(fragiles)) {
								explore_rangs(fragiles, limite = 6L)
							} else {
								explore_vide("Aucun sous-th\u00e8me sous 75 %. Belle r\u00e9gularit\u00e9.")
							}
						),
						explore_carte(
							"Vos s\u00e9ances",
							glue::glue("{length(s)} s\u00e9ance(s) dans le p\u00e9rim\u00e8tre, les plus r\u00e9centes d'abord."),
							explore_seances(evo, limite = 6L)
						)
					)
				),
				stats_report_next_strip(
					points_faibles = faible_lab,
					points_forts = fort_lab,
					cta_label = "Retourner s'entra\u00eener"
				)
			)

			stats_report_shell(
				essentiel = essentiel,
				comprendre = comprendre,
				revoir = revoir,
				onglets = c("Vue d'ensemble", "Par sujet", "\u00c0 reprendre"),
				footnote = shiny::tagList(
					"Les pourcentages sont recalcul\u00e9s sur le p\u00e9rim\u00e8tre s\u00e9lectionn\u00e9. ",
					"Les niveaux de difficult\u00e9 sont indicatifs et n'annoncent pas la r\u00e9ussite \u00e0 l'examen."
				)
			)
		})

		output$dl_csv <- shiny::downloadHandler(
			filename = function() "candidequizz-stats.csv",
			content = function(file) {
				r <- reponses()
				if (!nrow(r)) {
					utils::write.csv(r, file, row.names = FALSE)
					return(invisible(file))
				}

				stats_export_csv(r, file)
			}
		)
		output$dl_xlsx <- shiny::downloadHandler(
			filename = function() "candidequizz-stats.xlsx",
			content = function(file) {
				r <- reponses()
				if (!nrow(r)) {
					writexl::write_xlsx(list(reponses = r), file)
					return(invisible(file))
				}

				stats_export_xlsx(r, file)
			}
		)
		output$dl_html <- shiny::downloadHandler(
			filename = function() "candidequizz-stats.html",
			content = function(file) {
				r <- reponses()
				stats_export_html(
					titre = "CandideQuizz \u2014 statistiques",
					synthese = stats_synthese(r, length(seances())),
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
