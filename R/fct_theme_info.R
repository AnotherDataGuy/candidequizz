# Fenêtre d'information d'un grand thème, ouverte depuis le carrousel.
#
# Les textes éditoriaux viennent de `inst/extdata/themes.yml` ; les
# sous-thèmes et le nombre de questions viennent de la banque.

#' Textes éditoriaux des thèmes
#'
#' @return Une liste nommée par `theme_id`, vide si le fichier est absent.
#' @noRd
cq_theme_infos <- function() {
	chemin <- app_sys("extdata", "themes.yml")

	if (!nzchar(chemin) || !file.exists(chemin)) {
		return(list())
	}

	tryCatch(yaml::read_yaml(chemin)$themes %||% list(), error = function(e) list())
}

#' Fenêtre d'un thème
#'
#' @param theme_id Identifiant court du thème (`REHF`, …).
#' @param bank Banque de graines.
#'
#' @return Un `shiny::modalDialog()`, ou `NULL` si le thème est inconnu.
#' @noRd
cq_theme_modal <- function(theme_id, bank = bank_load()) {
	theme_id <- as.character(theme_id %||% "")[[1L]]
	themes <- bank_themes(bank)
	ligne <- match(theme_id, themes$theme_id)

	if (is.na(ligne)) {
		return(NULL)
	}

	info <- cq_theme_infos()[[theme_id]] %||% list()
	ref <- bank$referentiel[bank$referentiel$theme_id == theme_id, , drop = FALSE]
	src <- cq_theme_art_src(theme_id = theme_id, bank = bank)

	shiny::modalDialog(
		title = NULL,
		size = "l",
		easyClose = TRUE,
		class = "cq-theme-modal", # ajoutée au corps de la fenêtre
		if (!is.null(src)) {
			shiny::tags$img(class = "cq-theme-modal-art", src = src, alt = "")
		},
		shiny::tags$p(class = "eyebrow muted", "Th\u00e8me de l'examen civique"),
		shiny::tags$h2(class = "cq-theme-modal-title", themes$theme[[ligne]]),
		if (!is.null(info$accroche)) {
			shiny::tags$p(class = "cq-theme-modal-lead", trimws(info$accroche))
		},
		if (length(info$a_retenir)) {
			shiny::div(
				class = "cq-texte-retenir",
				shiny::span(class = "cq-texte-retenir-titre", "\u00c0 retenir"),
				shiny::tags$ul(lapply(info$a_retenir, shiny::tags$li))
			)
		},
		shiny::tags$h3(
			class = "cq-theme-modal-sub",
			glue::glue(
				"{nrow(ref)} sous-th\u00e8mes \u00b7 {themes$n_graines[[ligne]]} questions de r\u00e9f\u00e9rence"
			)
		),
		shiny::tags$ul(
			class = "cq-theme-modal-list",
			lapply(seq_len(nrow(ref)), function(i) {
				shiny::tags$li(
					shiny::tags$strong(ref$sub_theme[[i]]),
					if (!is.na(ref$cadre[[i]]) && nzchar(ref$cadre[[i]])) {
						shiny::tags$span(class = "muted", gsub(";", ",", ref$cadre[[i]]))
					}
				)
			})
		),
		footer = shiny::tagList(
			shiny::modalButton("Fermer"),
			shiny::tags$button(
				type = "button",
				class = "btn btn-primary primary",
				onclick = sprintf(
					"Shiny.setInputValue('theme_entrainer', '%s', {priority: 'event'})",
					theme_id
				),
				"M'entra\u00eener sur ce th\u00e8me"
			)
		)
	)
}
