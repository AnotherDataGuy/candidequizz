# Rapports d'exploration (bilan séance + page Progression).

#' Titre narratif selon le score
#'
#' @param score_pct Score 0–100.
#' @param scope `"bilan"` ou `"stats"`.
#'
#' @return Liste `title`, `body`.
#' @noRd
stats_report_story_copy <- function(score_pct, scope = "bilan") {
	if (scope == "stats") {
		if (score_pct >= 80) {
			return(list(
				title = "Le fil s'\u00e9paissit.",
				body = "Vos s\u00e9ances dessinent une progression nette. Gardez le rythme sur les th\u00e8mes encore fragiles."
			))
		}
		if (score_pct >= 55) {
			return(list(
				title = "Vous avancez, point par point.",
				body = "Les acquis se consolident. Les donn\u00e9es ci-dessous montrent o\u00f9 concentrer la prochaine s\u00e9ance."
			))
		}
		return(list(
			title = "Le parcours commence ici.",
			body = "Chaque r\u00e9ponse laisse une trace. Explorez les th\u00e8mes fragiles, puis revenez sur les erreurs."
		))
	}

	if (score_pct >= 80) {
		return(list(
			title = "Une s\u00e9ance solide.",
			body = "Vous emportez l'essentiel. Relisez les \u00e9carts restants pour verrouiller les rep\u00e8res."
		))
	}
	if (score_pct >= 55) {
		return(list(
			title = "Des acquis, et des pistes.",
			body = "Le score raconte une s\u00e9ance utile. Les erreurs nomm\u00e9es valent plus qu'un simple total."
		))
	}
	return(list(
		title = "Une base \u00e0 consolider.",
		body = "Pas de jugement : des rep\u00e8res \u00e0 revoir. Passez par l'onglet Revoir, puis relancez une s\u00e9ance cibl\u00e9e."
	))
}

#' Panneau Essentiel (story + score-orbit)
#'
#' @param score_pct Score.
#' @param n_reponses Nombre de réponses.
#' @param diagnostic Texte de synthèse (optionnel).
#' @param meta_ligne Ligne secondaire sous l'orbit.
#' @param scope `"bilan"` ou `"stats"`.
#' @param suite Contenu additionnel (points forts, next-strip…).
#'
#' @return Tag HTML.
#' @noRd
stats_report_essentiel <- function(score_pct,
																	 n_reponses,
																	 diagnostic = NULL,
																	 meta_ligne = NULL,
																	 scope = "bilan",
																	 suite = NULL) {
	copy <- stats_report_story_copy(score_pct, scope)
	eyebrow <- if (identical(scope, "bilan")) {
		"Votre s\u00e9ance, en perspective"
	} else {
		"Le fil de votre progression"
	}

	shiny::tagList(
		shiny::div(
			class = "report-story",
			shiny::div(
				class = "story-copy",
				shiny::tags$span(class = "eyebrow", eyebrow),
				shiny::tags$h2(copy$title),
				shiny::tags$p(if (nzchar(diagnostic %||% "")) diagnostic else copy$body),
				shiny::div(
					class = "story-actions",
					shiny::tags$button(
						type = "button",
						class = "btn primary",
						`data-report-tab` = "review",
						if (score_pct == 100) "Revoir mes r\u00e9ponses" else "Comprendre mes erreurs",
						" ",
						cq_icon("i-arrow")
					),
					shiny::tags$button(
						type = "button",
						class = "textbtn",
						`data-report-tab` = "understand",
						"Explorer les donn\u00e9es \u2197"
					)
				)
			),
			shiny::div(
				class = "score-island",
				shiny::div(
					class = "score-orbit",
					style = paste0("--score:", score_pct),
					role = "img",
					`aria-label` = paste0("Score de ", score_pct, " %"),
					shiny::div(
						shiny::tags$strong(paste0(score_pct, " %")),
						shiny::tags$span(if (identical(scope, "bilan")) "score de la s\u00e9ance" else "score de la s\u00e9lection")
					)
				),
				shiny::tags$p(
					paste(
						n_reponses,
						if (identical(as.integer(n_reponses), 1L)) "r\u00e9ponse" else "r\u00e9ponses"
					),
					if (!is.null(meta_ligne) && nzchar(meta_ligne)) {
						shiny::tagList(shiny::tags$br(), meta_ligne)
					}
				)
			)
		),
		suite
	)
}

#' Coquille d'analytics à onglets
#'
#' @param essentiel Contenu onglet 1.
#' @param comprendre Contenu onglet 2.
#' @param revoir Contenu onglet 3.
#' @param toolbar Contenu optionnel au-dessus des onglets.
#' @param footnote Note de bas de page.
#' @param onglets Libellés des trois onglets. Le bilan et la Progression
#'   utilisent des libellés différents pour qu'on sache où l'on est.
#'
#' @return Tag HTML.
#' @noRd
stats_report_shell <- function(essentiel,
															 comprendre,
															 revoir,
															 toolbar = NULL,
															 footnote = NULL,
															 onglets = c("Essentiel", "Comprendre", "Revoir")) {
	shiny::div(
		class = "analytics",
		toolbar,
		shiny::div(
			class = "report-tabs",
			role = "tablist",
			shiny::tags$button(
				type = "button",
				role = "tab",
				`aria-selected` = "true",
				`data-report-panel` = "story",
				shiny::tags$span("01"),
				paste0(" ", onglets[[1]])
			),
			shiny::tags$button(
				type = "button",
				role = "tab",
				`aria-selected` = "false",
				`data-report-panel` = "understand",
				shiny::tags$span("02"),
				paste0(" ", onglets[[2]])
			),
			shiny::tags$button(
				type = "button",
				role = "tab",
				`aria-selected` = "false",
				`data-report-panel` = "review",
				shiny::tags$span("03"),
				paste0(" ", onglets[[3]])
			)
		),
		shiny::div(
			class = "report-content",
			shiny::div(
				class = "report-panel-pane",
				`data-panel` = "story",
				essentiel
			),
			shiny::div(
				class = "report-panel-pane hidden",
				`data-panel` = "understand",
				comprendre
			),
			shiny::div(
				class = "report-panel-pane hidden",
				`data-panel` = "review",
				revoir
			)
		),
		if (!is.null(footnote)) {
			shiny::div(class = "report-footnote", footnote)
		}
	)
}

#' Bandeau de portée : de quoi parle la page
#'
#' Une bande en tête du bilan (« Cette séance ») et de la Progression
#' (« Toutes vos séances »), pour ne pas confondre les deux pages.
#'
#' @param portee `"seance"` ou `"carnet"`.
#' @param titre Libellé court, en capitales à l'écran.
#' @param detail Précision (numéro, nombre de séances…).
#'
#' @return Tag HTML.
#' @noRd
stats_report_portee <- function(portee = c("seance", "carnet"), titre, detail = NULL) {
	portee <- match.arg(portee)

	shiny::div(
		class = paste0("report-portee report-portee--", portee),
		shiny::tags$strong(titre),
		if (!is.null(detail) && nzchar(detail)) shiny::tags$span(detail)
	)
}

#' Bandeau « suite » sous l'essentiel
#'
#' @param points_forts Vecteur.
#' @param points_faibles Vecteur.
#' @param plan Texte de révision.
#'
#' @return Tag ou NULL.
#' @noRd
stats_report_next_strip <- function(points_forts = NULL,
																		points_faibles = NULL,
																		plan = NULL,
																		cta_label = NULL,
																		cta_onclick = NULL) {
	if (!length(points_forts) && !length(points_faibles) && !nzchar(plan %||% "")) {
		return(NULL)
	}

	shiny::div(
		class = "next-strip",
		shiny::div(
			shiny::tags$span(class = "eyebrow", "Pour la suite"),
			shiny::tags$h3(
				if (nzchar(plan %||% "")) plan else "Revenir sur ce qui r\u00e9siste."
			),
			if (length(points_faibles)) {
				shiny::tags$p(paste("\u00c0 travailler :", paste(points_faibles, collapse = ", ")))
			},
			if (length(points_forts)) {
				shiny::tags$p(class = "muted", paste("Points forts :", paste(points_forts, collapse = ", ")))
			}
		),
		if (!is.null(cta_label) && nzchar(cta_label)) {
			shiny::tags$button(
				type = "button",
				class = "btn darkbtn",
				onclick = cta_onclick %||% "Shiny.setInputValue('nav_seances', Math.random());",
				cta_label,
				" ",
				cq_icon("i-arrow")
			)
		}
	)
}

#' Ruban de composition des réponses (Justes / Fausses)
#'
#' Les réponses non justes (partielles, fausses, passées) sont regroupées
#' sous « Fausses » pour une lecture binaire.
#'
#' @param n_justes,n_partielles,n_fausses,n_passees Compteurs.
#'
#' @return Tag HTML.
#' @noRd
stats_report_outcome_ribbon <- function(n_justes = 0L,
																				n_partielles = 0L,
																				n_fausses = 0L,
																				n_passees = 0L) {
	n_justes <- as.integer(n_justes)
	n_autres <- as.integer(n_partielles + n_fausses + n_passees)
	total <- n_justes + n_autres

	if (!total) {
		return(NULL)
	}

	parts <- list(
		list(cls = "right", n = n_justes, lab = "Justes", sym = "\u2713"),
		list(cls = "partial", n = n_partielles, lab = "Partielles", sym = "~"),
		list(cls = "wrong", n = n_fausses, lab = "Fausses", sym = "\u2717"),
		list(cls = "skip", n = n_passees, lab = "Pass\u00e9es", sym = "\u2013")
	)
	parts <- Filter(function(p) p$n > 0L, parts)

	shiny::tagList(
		shiny::div(
			class = "outcome-ribbon",
			role = "img",
			`aria-label` = glue::glue("{n_justes} justes, {n_partielles} partielles, {n_fausses} fausses, {n_passees} pass\u00e9es"),
			lapply(parts, function(p) {
				shiny::tags$button(
					type = "button",
					class = p$cls,
					style = paste0("flex:", p$n),
					title = paste(p$n, p$lab),
					p$sym
				)
			})
		),
		shiny::div(
			class = "outcome-legend",
			lapply(parts, function(p) {
				shiny::tags$button(
					type = "button",
					shiny::tags$i(class = paste("swatch", p$cls), p$sym),
					shiny::tags$span(p$lab),
					shiny::tags$strong(p$n)
				)
			})
		),
		stats_note(glue::glue("Sur {total} r\u00e9ponses, {n_justes} sont enti\u00e8rement justes ({round(100 * n_justes / total)} %). Les {n_partielles} r\u00e9ponses partielles contribuent au score ; {n_fausses} sont fausses et {n_passees} ont \u00e9t\u00e9 pass\u00e9es."))
	)
}

#' Bandeau d'indicateurs (KPI) sous l'Essentiel
#'
#' @param score_pct Score 0–100.
#' @param n_justes,n_reponses,n_seances Compteurs.
#' @param temps_median_s Médiane en secondes, ou `NA`.
#' @param delta Écart vs séance précédente (0–1), ou `NULL`.
#' @param troisieme_valeur,troisieme_libelle Valeur et libellé optionnels du
#'   troisième indicateur. Par défaut, le nombre de séances.
#'
#' @return Tag HTML.
#' @noRd
stats_report_kpis <- function(score_pct,
															n_justes,
															n_reponses,
															n_seances,
															temps_median_s = NA_real_,
															delta = NULL,
															troisieme_valeur = n_seances,
															troisieme_libelle = "S\u00e9ances") {
	cellule <- function(valeur, libelle, note = NULL) {
		shiny::div(
			class = "report-kpi",
			shiny::tags$strong(valeur),
			shiny::tags$span(libelle),
			if (!is.null(note) && nzchar(note)) {
				shiny::tags$small(note)
			}
		)
	}

	delta_txt <- if (!is.null(delta) && is.finite(delta)) {
		stats_delta_txt(delta)
	} else {
		NULL
	}

	shiny::div(
		class = "report-kpis",
		role = "group",
		`aria-label` = "Indicateurs cl\u00e9s",
		cellule(paste0(as.integer(score_pct), " %"), "Score", delta_txt),
		cellule(
			glue::glue("{as.integer(n_justes)}/{as.integer(n_reponses)}"),
			"Justes",
			if (n_reponses > 0L) {
				glue::glue("{round(100 * n_justes / n_reponses)} % des r\u00e9ponses")
			}
		),
		cellule(as.character(troisieme_valeur), troisieme_libelle),
		cellule(
			if (is.finite(temps_median_s)) {
				paste0(round(temps_median_s), " s")
			} else {
				"\u2014"
			},
			"Temps m\u00e9dian"
		)
	)
}
