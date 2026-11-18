# Onglets « Comprendre » et « Revoir » de la page Progression.
#
# Composants HTML/CSS purs : ils se recomposent selon la largeur de leur carte
# (container queries) plutôt que selon celle de l'écran.

#' Pourcentage entier, ou `NA`
#' @noRd
explore_pct <- function(x) {
	if (length(x) && is.finite(x[[1L]])) as.integer(round(100 * x[[1L]])) else NA_integer_
}

#' Message d'absence de données
#' @noRd
explore_vide <- function(texte) {
	shiny::p(class = "explore-empty", texte)
}

#' Note de lecture sous un graphique
#' @noRd
explore_note <- function(...) {
	shiny::div(class = "explore-note", ...)
}

#' Chapitre numéroté
#'
#' @param numero Numéro affiché (`"01"`…).
#' @param titre,sous Titre et chapeau.
#' @param ... Contenu.
#' @param class Classe de mise en page du corps.
#'
#' @return Tag HTML.
#' @noRd
explore_chapitre <- function(numero, titre, sous = NULL, ..., class = NULL) {
	shiny::tags$section(
		class = "explore-chapter",
		shiny::tags$header(
			class = "explore-chapter-head",
			shiny::span(class = "explore-num", numero),
			shiny::div(
				shiny::tags$h2(titre),
				if (nzchar(sous %||% "")) shiny::tags$p(sous)
			)
		),
		shiny::div(class = paste(c("explore-chapter-body", class), collapse = " "), ...)
	)
}

#' Carte d'un chapitre
#'
#' @param titre,sous Titre et sous-titre.
#' @param ... Contenu.
#' @param repere Petite mention à droite du titre.
#' @param class Classes supplémentaires.
#'
#' @return Tag HTML.
#' @noRd
explore_carte <- function(titre, sous = NULL, ..., repere = NULL, class = NULL) {
	shiny::tags$article(
		class = paste(c("explore-card", class), collapse = " "),
		shiny::tags$header(
			class = "explore-card-head",
			shiny::div(
				shiny::tags$h3(titre),
				if (nzchar(sous %||% "")) shiny::tags$p(sous)
			),
			if (!is.null(repere) && nzchar(repere)) shiny::span(class = "explore-card-repere", repere)
		),
		shiny::div(class = "explore-card-body", ...)
	)
}

#' Classement horizontal (une ligne par catégorie)
#'
#' @param tableau Sortie de [stats_par()].
#' @param limite Nombre de lignes.
#' @param seuil Repère pédagogique (0–1).
#'
#' @return Tag HTML.
#' @noRd
explore_rangs <- function(tableau, limite = 8L, seuil = 0.55) {
	if (is.null(tableau) || !nrow(tableau)) {
		return(explore_vide("Pas encore de donn\u00e9es."))
	}

	tableau <- utils::head(tableau, limite)

	shiny::tags$ol(
		class = "explore-rangs",
		style = sprintf("--seuil:%d%%", as.integer(round(100 * seuil))),
		lapply(seq_len(nrow(tableau)), function(i) {
			pct <- explore_pct(tableau$reussite[[i]])
			nom <- tableau$cle[[i]]

			shiny::tags$li(
				class = "explore-rang",
				`data-ton` = stats_chaleur_ton(tableau$reussite[[i]]),
				shiny::div(
					class = "explore-rang-lab",
					shiny::span(title = nom, nom),
					shiny::tags$small(glue::glue("{tableau$n[[i]]} r\u00e9ponse(s)"))
				),
				shiny::span(
					class = "explore-rang-track",
					`aria-hidden` = "true",
					shiny::tags$b(style = sprintf("width:%d%%", if (is.na(pct)) 0L else pct))
				),
				shiny::tags$strong(
					class = "explore-rang-val",
					if (is.na(pct)) "\u2014" else paste0(pct, " %")
				)
			)
		})
	)
}

#' Colonnes par niveau (tous les niveaux affichés, même vides)
#'
#' @param tableau Sortie de [stats_par()] dont `cle` correspond à `niveaux`.
#' @param niveaux Clés attendues, dans l'ordre.
#' @param libelles Libellés affichés sous les colonnes.
#' @param seuil Repère pédagogique (0–1).
#'
#' @return Tag HTML.
#' @noRd
explore_colonnes <- function(tableau, niveaux, libelles = niveaux, seuil = 0.55) {
	if (is.null(tableau) || !nrow(tableau)) {
		return(explore_vide("Pas encore de donn\u00e9es."))
	}

	shiny::div(
		class = "explore-cols",
		role = "list",
		style = sprintf("--seuil:%d%%", as.integer(round(100 * seuil))),
		lapply(seq_along(niveaux), function(j) {
			ligne <- tableau[tableau$cle == niveaux[[j]], , drop = FALSE]
			mesure <- nrow(ligne) > 0L && is.finite(ligne$reussite[[1L]])
			pct <- if (mesure) explore_pct(ligne$reussite[[1L]]) else NA_integer_

			shiny::div(
				class = "explore-col",
				role = "listitem",
				`data-ton` = if (mesure) stats_chaleur_ton(ligne$reussite[[1L]]) else "vide",
				`aria-label` = if (mesure) {
					glue::glue("Niveau {libelles[[j]]} : {pct} % sur {ligne$n[[1L]]} r\u00e9ponse(s)")
				} else {
					glue::glue("Niveau {libelles[[j]]} : non mesur\u00e9")
				},
				shiny::tags$strong(class = "explore-col-val", if (mesure) paste0(pct, " %") else "\u2014"),
				shiny::span(
					class = "explore-col-track",
					`aria-hidden` = "true",
					shiny::tags$b(style = sprintf("height:%d%%", if (mesure) max(pct, 2L) else 0L))
				),
				shiny::span(class = "explore-col-lab", libelles[[j]]),
				shiny::tags$small(if (mesure) glue::glue("{ligne$n[[1L]]} r\u00e9p.") else "\u2014")
			)
		})
	)
}

#' Légende du repère 55 %
#' @noRd
explore_legende_seuil <- function(texte = "Rep\u00e8re p\u00e9dagogique : 55 %") {
	shiny::div(
		class = "explore-legend",
		shiny::span(class = "explore-key explore-key--seuil", texte),
		shiny::span(class = "explore-key", `data-ton` = "bas", "< 40 %"),
		shiny::span(class = "explore-key", `data-ton` = "fragile", "40\u201354 %"),
		shiny::span(class = "explore-key", `data-ton` = "moyen", "55\u201374 %"),
		shiny::span(class = "explore-key", `data-ton` = "haut", "\u2265 75 %")
	)
}

#' Carte sous-thèmes × séances
#'
#' @param chaleur Tableau de [stats_chaleur()].
#' @param sessions Identifiants de séances (colonnes, de la plus ancienne à la plus récente).
#' @param themes Sous-thèmes (lignes).
#' @param libelles Libellés de colonnes, nommés par `session_id`.
#' @param global Réussite globale par sous-thème (vecteur nommé), ou `NULL`.
#'
#' @return Tag HTML.
#' @noRd
explore_chaleur <- function(chaleur, sessions, themes, libelles = NULL, global = NULL) {
	if (!nrow(chaleur) || !length(sessions) || !length(themes)) {
		return(explore_vide("Pas encore de carte : il faut au moins une s\u00e9ance termin\u00e9e."))
	}

	cellule <- function(reussite, n = NULL) {
		if (!length(reussite) || !is.finite(reussite)) {
			return(shiny::tags$td(shiny::span(class = "explore-heat-cell", `data-ton` = "vide", "\u00b7")))
		}

		shiny::tags$td(shiny::span(
			class = "explore-heat-cell",
			`data-ton` = stats_chaleur_ton(reussite),
			title = if (!is.null(n)) glue::glue("{explore_pct(reussite)} % \u00b7 {n} r\u00e9ponse(s)"),
			explore_pct(reussite)
		))
	}

	shiny::div(
		class = "explore-heat-scroll",
		tabindex = "0",
		shiny::tags$table(
			class = "explore-heat",
			shiny::tags$caption(
				class = "visually-hidden",
				"R\u00e9ussite en pourcentage par sous-th\u00e8me et par s\u00e9ance."
			),
			shiny::tags$thead(shiny::tags$tr(
				shiny::tags$th(scope = "col", "Sous-th\u00e8me"),
				lapply(sessions, function(sid) {
					shiny::tags$th(scope = "col", if (sid %in% names(libelles)) libelles[[sid]] else sid)
				}),
				if (!is.null(global)) shiny::tags$th(scope = "col", class = "explore-heat-total", "Global")
			)),
			shiny::tags$tbody(lapply(themes, function(theme) {
				shiny::tags$tr(
					shiny::tags$th(scope = "row", title = theme, theme),
					lapply(sessions, function(sid) {
						ligne <- chaleur[chaleur$session_id == sid & chaleur$sub_theme == theme, , drop = FALSE]
						if (!nrow(ligne)) cellule(NA_real_) else cellule(ligne$reussite[[1L]], ligne$n[[1L]])
					}),
					if (!is.null(global)) {
						shiny::tags$td(
							class = "explore-heat-total",
							if (theme %in% names(global)) paste0(explore_pct(global[[theme]]), " %") else "\u2014"
						)
					}
				)
			}))
		)
	)
}

#' Mosaïque des réponses, regroupées par séance
#'
#' @param reponses Tableau de [stats_reponses()].
#' @param n_seances Nombre de séances récentes affichées.
#'
#' @return Tag HTML.
#' @noRd
explore_mosaique <- function(reponses, n_seances = 8L) {
	if (!nrow(reponses)) {
		return(explore_vide("Pas encore de traces."))
	}

	reponses <- reponses[order(reponses$cree_le, reponses$indice_seance), , drop = FALSE]
	ids <- utils::tail(unique(reponses$session_id), n_seances)
	reponses <- reponses[reponses$session_id %in% ids, , drop = FALSE]
	issues <- stats_issue(reponses)
	lib <- c(juste = "juste", partielle = "partielle", fausse = "fausse", passee = "pass\u00e9e")
	pluriel <- c(juste = "Justes", partielle = "Partielles", fausse = "Fausses", passee = "Pass\u00e9es")
	decompte <- table(factor(issues, levels = names(lib)))

	groupes <- lapply(ids, function(sid) {
		idx <- which(reponses$session_id == sid)
		shiny::div(
			class = "explore-mosaic-seance",
			shiny::div(
				class = "explore-mosaic-cases",
				lapply(idx, function(i) {
					shiny::span(
						class = "explore-case",
						`data-issue` = issues[[i]],
						title = glue::glue("{reponses$sub_theme[[i]]} \u00b7 {lib[[issues[[i]]]]}")
					)
				})
			),
			shiny::tags$small(stats_date_courte(reponses$cree_le[[idx[[1L]]]]))
		)
	})

	shiny::tagList(
		shiny::div(
			class = "explore-mosaic",
			role = "img",
			`aria-label` = glue::glue(
				"{nrow(reponses)} r\u00e9ponses sur {length(ids)} s\u00e9ance(s) : ",
				"{decompte[['juste']]} justes, {decompte[['partielle']]} partielles, ",
				"{decompte[['fausse']]} fausses, {decompte[['passee']]} pass\u00e9es"
			),
			groupes
		),
		shiny::div(
			class = "explore-legend",
			lapply(names(lib), function(k) {
				if (decompte[[k]] == 0L && k %in% c("partielle", "passee")) {
					return(NULL)
				}
				shiny::span(
					class = "explore-key",
					`data-issue` = k,
					paste0(pluriel[[k]], " "),
					shiny::tags$b(decompte[[k]])
				)
			}),
			shiny::span(class = "explore-legend-axis", "Plus ancienne \u2192 plus r\u00e9cente")
		)
	)
}

#' Rythme : réussite des réponses rapides vs lentes
#'
#' @param analyse Sortie de [stats_temps()].
#'
#' @return Tag HTML.
#' @noRd
explore_rythme <- function(analyse) {
	med <- analyse$median_s
	rap <- analyse$reussite_rapides
	len <- analyse$reussite_lentes

	if (!is.finite(med) || !is.finite(rap) || !is.finite(len)) {
		return(explore_vide("Pas encore assez de temps mesur\u00e9s."))
	}

	ecart <- len - rap
	verdict <- if (ecart <= -0.05) {
		"Les questions qui vous retiennent longtemps sont souvent celles qui r\u00e9sistent : rep\u00e9rez-les, elles signalent un doute."
	} else if (ecart >= 0.05) {
		"Prendre le temps vous r\u00e9ussit : vos r\u00e9ponses pos\u00e9es sont plus justes que vos r\u00e9ponses rapides."
	} else {
		"Votre vitesse n'influe gu\u00e8re sur la r\u00e9ussite : le rythme n'est pas un sujet pour l'instant."
	}

	case <- function(libelle, valeur, detail) {
		shiny::div(
			class = "explore-duel-cell",
			`data-ton` = stats_chaleur_ton(valeur),
			shiny::span(libelle),
			shiny::tags$strong(paste0(explore_pct(valeur), " %")),
			shiny::tags$small(detail)
		)
	}

	lentes <- analyse$plus_lentes
	lentes <- lentes[is.finite(lentes$duree_s), , drop = FALSE]
	lentes <- utils::head(lentes, 3L)
	plafond <- if (nrow(lentes)) max(lentes$duree_s) else 1

	shiny::tagList(
		shiny::div(
			class = "explore-duel",
			case("R\u00e9ponses rapides", rap, glue::glue("moins de {round(med)} s")),
			case("R\u00e9ponses pos\u00e9es", len, glue::glue("{round(med)} s et plus"))
		),
		shiny::p(class = "explore-verdict", verdict),
		if (nrow(lentes)) {
			shiny::div(
				class = "explore-lentes",
				shiny::span(class = "eyebrow", "Les plus longues"),
				shiny::tags$ol(lapply(seq_len(nrow(lentes)), function(i) {
					shiny::tags$li(
						shiny::span(title = lentes$sub_theme[[i]], lentes$sub_theme[[i]]),
						shiny::span(
							class = "explore-lentes-track",
							`aria-hidden` = "true",
							shiny::tags$b(style = sprintf("width:%d%%", as.integer(round(100 * lentes$duree_s[[i]] / plafond))))
						),
						shiny::tags$strong(glue::glue("{round(lentes$duree_s[[i]])} s"))
					)
				}))
			)
		}
	)
}

#' Confusions récurrentes
#'
#' @param conf Sortie de [stats_confusions()].
#' @param limite Nombre de lignes.
#'
#' @return Tag HTML.
#' @noRd
explore_confusions <- function(conf, limite = 5L) {
	if (is.null(conf) || !nrow(conf)) {
		return(explore_vide("Aucune confusion r\u00e9currente pour l'instant. C'est bon signe."))
	}

	conf <- utils::head(conf[order(-conf$n), , drop = FALSE], limite)
	plafond <- max(conf$n, 1L)

	shiny::tagList(
		shiny::tags$ol(
			class = "explore-conf",
			lapply(seq_len(nrow(conf)), function(i) {
				shiny::tags$li(
					shiny::tags$strong(class = "explore-conf-n", paste0(conf$n[[i]], "\u00d7")),
					shiny::div(
						class = "explore-conf-body",
						shiny::span(conf$libelle[[i]] %||% conf$diagnostic_tag[[i]]),
						if ("statut" %in% names(conf)) {
							shiny::tags$small(conf$statut[[i]])
						},
						shiny::span(
							class = "explore-conf-track",
							`aria-hidden` = "true",
							shiny::tags$b(style = sprintf("width:%d%%", as.integer(round(100 * conf$n[[i]] / plafond))))
						)
					)
				)
			})
		),
		explore_note(paste(
			"Une observation isol\u00e9e reste \u00e0 confirmer.",
			"Seule une confusion r\u00e9p\u00e9t\u00e9e influence les prochaines s\u00e9ances d'apprentissage."
		))
	)
}

#' Réponses à revoir, avec énoncé et correction
#'
#' @param seances Liste de séances chargées.
#' @param limite Nombre maximal d'entrées conservées.
#' @param inclure_justes Garder aussi les réponses justes.
#' @param ordre `"priorite"` (fausses, partielles, passées ; récentes d'abord)
#'   ou `"seance"` (ordre de passation).
#'
#' @return Une liste d'entrées.
#' @noRd
explore_revoir_entrees <- function(seances,
																	 limite = 60L,
																	 inclure_justes = FALSE,
																	 ordre = c("priorite", "seance")) {
	ordre <- match.arg(ordre)
	entrees <- list()

	for (s in seances) {
		items <- s$items %||% list()
		if (length(items)) {
			names(items) <- vapply(items, function(i) as.character(i$item_id %||% ""), character(1))
		}

		for (k in seq_along(s$reponses %||% list())) {
			reponse <- s$reponses[[k]]
			score <- as.numeric(reponse$score %||% NA_real_)[[1L]]
			issue <- if (isTRUE(reponse$passee)) {
				"passee"
			} else if (isTRUE(reponse$is_correct)) {
				"juste"
			} else if (is.finite(score) && score > 0) {
				"partielle"
			} else {
				"fausse"
			}

			if (identical(issue, "juste") && !inclure_justes) {
				next
			}

			item <- items[[as.character(reponse$item_id %||% "")]]
			entrees[[length(entrees) + 1L]] <- list(
				issue = issue,
				numero = k,
				session_id = as.character(s$session_id %||% ""),
				cree_le = as.POSIXct(s$cree_le),
				indice = as.integer(reponse$indice_seance %||% k),
				sub_theme = as.character(reponse$sub_theme %||% item$sub_theme %||% ""),
				theme = as.character(reponse$theme %||% item$theme %||% ""),
				duree_s = as.numeric(reponse$duree_ms %||% NA_real_)[[1L]] / 1000,
				enonce = if (!is.null(item)) as.character(item$enonce %||% "") else "",
				votre = reponse_libelle(reponse, item),
				bonne = if (!is.null(item)) tryCatch(question_type_solution(item), error = function(e) "") else "",
				explication = if (!is.null(item)) as.character(item$explication %||% "") else ""
			)
		}
	}

	if (!length(entrees) || identical(ordre, "seance")) {
		return(utils::head(entrees, limite))
	}

	prio <- match(vapply(entrees, `[[`, character(1), "issue"), c("fausse", "partielle", "passee", "juste"))
	quand <- vapply(entrees, function(e) as.numeric(e$cree_le) * 1000 + e$indice, numeric(1))
	utils::head(entrees[order(prio, -quand)], limite)
}

#' Ancre d'une carte de révision
#' @noRd
explore_revoir_ancre <- function(e) {
	paste0("revoir-", gsub("[^A-Za-z0-9]", "", e$session_id), "-", e$numero)
}

#' Parcours d'une séance : une pastille par question, cliquable vers Revoir
#'
#' @param entrees Sortie de [explore_revoir_entrees()] avec `inclure_justes = TRUE`
#'   et `ordre = "seance"`.
#'
#' @return Tag HTML.
#' @noRd
explore_parcours <- function(entrees) {
	if (!length(entrees)) {
		return(NULL)
	}

	marque <- c(fausse = "\u2717", partielle = "~", passee = "\u2013", juste = "\u2713")
	mention <- c(fausse = "fausse", partielle = "partielle", passee = "pass\u00e9e", juste = "juste")
	issues <- vapply(entrees, `[[`, character(1), "issue")
	pluriel <- c(juste = "Justes", partielle = "Partielles", fausse = "Fausses", passee = "Pass\u00e9es")

	shiny::tagList(
		shiny::tags$ol(
			class = "explore-parcours",
			lapply(entrees, function(e) {
				shiny::tags$li(shiny::tags$button(
					type = "button",
					class = "explore-parcours-q",
					`data-issue` = e$issue,
					`data-report-tab` = "review",
					`data-revoir-cible` = explore_revoir_ancre(e),
					title = paste0("Q", e$numero, " \u00b7 ", e$sub_theme, " \u00b7 ", mention[[e$issue]]),
					`aria-label` = paste0("Question ", e$numero, ", ", mention[[e$issue]], " : revoir"),
					shiny::tags$b(e$numero),
					shiny::span(`aria-hidden` = "true", marque[[e$issue]])
				))
			})
		),
		shiny::div(
			class = "explore-legend",
			lapply(names(pluriel), function(k) {
				n <- sum(issues == k)
				if (n > 0L) shiny::span(class = "explore-key", `data-issue` = k, paste0(pluriel[[k]], " "), shiny::tags$b(n))
			}),
			shiny::span(class = "explore-legend-axis", "Touchez une question pour la revoir")
		)
	)
}

#' Temps passé sur chaque question, coloré selon l'issue
#'
#' @param entrees Entrées dans l'ordre de passation.
#'
#' @return Tag HTML.
#' @noRd
explore_temps_questions <- function(entrees) {
	durees <- vapply(entrees, function(e) e$duree_s %||% NA_real_, numeric(1))

	if (!length(entrees) || !any(is.finite(durees))) {
		return(explore_vide("Pas encore de temps mesur\u00e9s."))
	}

	mediane <- stats::median(durees, na.rm = TRUE)
	plafond <- max(stats::quantile(durees, 0.9, na.rm = TRUE, names = FALSE), mediane * 1.5, 1)
	mention <- c(fausse = "Fausse", partielle = "Partielle", passee = "Pass\u00e9e", juste = "Juste")

	shiny::tagList(
		shiny::tags$ol(
			class = "explore-temps",
			style = sprintf("--seuil:%.1f%%", 100 * min(mediane / plafond, 1)),
			lapply(seq_along(entrees), function(i) {
				e <- entrees[[i]]
				d <- durees[[i]]
				largeur <- if (is.finite(d)) 100 * min(d / plafond, 1) else 0
				shiny::tags$li(
					class = "explore-temps-row",
					`data-issue` = e$issue,
					shiny::span(class = "explore-temps-num", e$numero),
					shiny::span(class = "explore-temps-lab", title = e$sub_theme, e$sub_theme),
					shiny::span(
						class = "explore-temps-track",
						`aria-hidden` = "true",
						shiny::tags$b(
							class = if (is.finite(d) && d > plafond) "explore-temps-deborde",
							style = sprintf("width:%.1f%%", largeur)
						)
					),
					shiny::tags$strong(
						title = mention[[e$issue]],
						if (is.finite(d)) paste0(round(d), " s") else "\u2014"
					)
				)
			})
		),
		shiny::div(
			class = "explore-legend",
			shiny::span(class = "explore-key explore-key--seuil", glue::glue("M\u00e9diane : {round(mediane)} s")),
			shiny::span(class = "explore-key", `data-issue` = "juste", "Juste"),
			shiny::span(class = "explore-key", `data-issue` = "partielle", "Partielle"),
			shiny::span(class = "explore-key", `data-issue` = "fausse", "Fausse"),
			shiny::span(class = "explore-key", `data-issue` = "passee", "Pass\u00e9e")
		)
	)
}

#' Pile de révision filtrable
#'
#' @param entrees Sortie de [explore_revoir_entrees()].
#' @param visibles Nombre d'entrées affichées avant « Afficher plus ».
#' @param numeroter Afficher le numéro de la question dans la pastille.
#' @param ouvrir Issues dont la carte est dépliée d'emblée.
#' @param vide Message quand il n'y a rien à revoir.
#'
#' @return Tag HTML.
#' @noRd
explore_revoir_deck <- function(entrees,
																visibles = 8L,
																numeroter = FALSE,
																ouvrir = character(0),
																vide = "Toutes les r\u00e9ponses du p\u00e9rim\u00e8tre sont justes.") {
	if (!length(entrees)) {
		return(shiny::div(
			class = "explore-empty explore-empty--fete",
			shiny::tags$strong("Rien \u00e0 revoir."),
			shiny::span(vide)
		))
	}

	issues <- vapply(entrees, `[[`, character(1), "issue")
	lib <- c(fausse = "Fausses", partielle = "Partielles", passee = "Pass\u00e9es", juste = "Justes")
	marque <- c(fausse = "\u2717", partielle = "~", passee = "\u2013", juste = "\u2713")
	mention <- c(fausse = "Fausse", partielle = "Partielle", passee = "Pass\u00e9e", juste = "Juste")

	filtre <- function(cle, texte, n, actif = FALSE) {
		shiny::tags$button(
			type = "button",
			class = "revoir-filtre",
			`data-revoir-filtre` = cle,
			`aria-pressed` = if (actif) "true" else "false",
			`data-issue` = if (!identical(cle, "tout")) cle,
			shiny::span(texte),
			shiny::tags$b(n)
		)
	}

	carte <- function(e) {
		parties <- Filter(nzchar, c(e$sub_theme, e$theme))
		parties <- parties[!duplicated(tolower(iconv(parties, to = "ASCII//TRANSLIT")))]
		sujet <- paste(parties, collapse = " \u00b7 ")
		repere <- if (numeroter) {
			if (is.finite(e$duree_s %||% NA_real_)) glue::glue("{round(e$duree_s)} s") else NULL
		} else {
			stats_date_courte(e$cree_le)
		}

		shiny::tags$details(
			class = "revoir-item",
			id = explore_revoir_ancre(e),
			`data-issue` = e$issue,
			open = if (e$issue %in% ouvrir) NA else NULL,
			shiny::tags$summary(
				shiny::span(
					class = paste("revoir-mark", if (numeroter) "revoir-mark--num"),
					`aria-label` = if (numeroter) paste("Question", e$numero),
					if (numeroter) e$numero else marque[[e$issue]]
				),
				shiny::span(
					class = "revoir-sum",
					shiny::span(class = "revoir-sujet", sujet),
					shiny::span(
						class = "revoir-enonce",
						if (nzchar(e$enonce)) e$enonce else "\u00c9nonc\u00e9 indisponible"
					)
				),
				shiny::span(
					class = "revoir-meta",
					shiny::span(class = "revoir-pill", mention[[e$issue]]),
					if (!is.null(repere)) shiny::tags$small(repere)
				),
				shiny::span(class = "revoir-chevron", `aria-hidden` = "true")
			),
			shiny::div(
				class = "revoir-corps",
				shiny::div(
					class = "revoir-compare",
					shiny::div(
						class = "revoir-compare-votre",
						shiny::span("Votre r\u00e9ponse"),
						shiny::tags$strong(e$votre)
					),
					shiny::div(
						class = "revoir-compare-bonne",
						shiny::span("Bonne r\u00e9ponse"),
						shiny::tags$strong(if (nzchar(e$bonne)) e$bonne else "\u2014")
					)
				),
				if (nzchar(e$explication)) {
					shiny::div(
						class = "revoir-explication",
						shiny::span(class = "eyebrow", "Pourquoi"),
						cq_explication_ui(e$explication, class = "revoir-explication-texte")
					)
				}
			)
		)
	}

	cartes <- lapply(entrees, carte)
	tete <- utils::head(cartes, visibles)
	reste <- if (length(cartes) > visibles) cartes[-seq_len(visibles)] else list()

	shiny::div(
		class = "revoir-deck",
		`data-filtre` = "tout",
		shiny::div(
			class = "revoir-filtres",
			role = "group",
			`aria-label` = "Filtrer les r\u00e9ponses",
			filtre("tout", "Tout", length(entrees), actif = TRUE),
			lapply(names(lib), function(k) {
				n <- sum(issues == k)
				if (n > 0L) filtre(k, lib[[k]], n)
			})
		),
		shiny::div(class = "revoir-liste", tete),
		if (length(reste)) {
			shiny::tags$details(
				class = "revoir-plus",
				shiny::tags$summary(glue::glue("Afficher les {length(reste)} autres")),
				shiny::div(class = "revoir-liste", reste)
			)
		}
	)
}

#' Liste des séances du périmètre, avec leur score
#'
#' @param evo Sortie de [stats_evolution()].
#' @param limite Nombre de séances.
#'
#' @return Tag HTML.
#' @noRd
explore_seances <- function(evo, limite = 6L) {
	if (is.null(evo) || !nrow(evo)) {
		return(explore_vide("Aucune s\u00e9ance."))
	}

	evo <- evo[evo$n > 0L, , drop = FALSE]
	if (!nrow(evo)) {
		return(explore_vide("Aucune s\u00e9ance avec des r\u00e9ponses."))
	}

	evo <- utils::head(evo[order(evo$cree_le, decreasing = TRUE), , drop = FALSE], limite)
	noms <- sub("\\s*\u2014\\s*\\d{2}/\\d{2}/\\d{4}.*$", "", evo$libelle)

	shiny::tags$ol(
		class = "explore-seances",
		lapply(seq_len(nrow(evo)), function(i) {
			pct <- explore_pct(evo$reussite[[i]])
			shiny::tags$li(
				shiny::div(
					class = "explore-seances-copy",
					shiny::span(title = evo$libelle[[i]], noms[[i]]),
					shiny::tags$small(glue::glue(
						"{stats_date_courte(evo$cree_le[[i]])} \u00b7 {evo$n[[i]]} question(s)"
					))
				),
				shiny::tags$strong(
					class = "explore-score-pill",
					`data-ton` = stats_chaleur_ton(evo$reussite[[i]]),
					if (is.na(pct)) "\u2014" else paste0(pct, " %")
				)
			)
		})
	)
}
