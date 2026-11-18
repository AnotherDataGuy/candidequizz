# Illustrations des grands thèmes (hors home, hors passation quiz).

#' Fichiers d'illustration indexés par `theme_id`
#'
#' @return Vecteur nommé de chemins relatifs à `www/`.
#' @noRd
cq_theme_art_map <- function() {
	# Illustrations vectorielles (charte de l'app). Les anciennes PNG ont \u00e9t\u00e9
	# retir\u00e9es (2026-11) ; elles restent dans l'historique git si besoin.
	c(
		REHF = "illustrations/v2/theme_1_histoire.svg",
		PRVS = "illustrations/v2/theme_2_principes.svg",
		INPO = "illustrations/v2/theme_3_institutions.svg",
		CIDD = "illustrations/v2/theme_4_citoyennete.svg",
		SOCF = "illustrations/v2/theme_5_societe.svg",
		GEOG = "illustrations/v2/theme_6_geographie.svg",
		ECON = "illustrations/v2/theme_7_economie.svg",
		FUEM = "illustrations/v2/theme_8_europe.svg"
	)
}

#' Normaliser un libellé de thème pour comparaison
#' @noRd
cq_theme_art_norm <- function(x) {
	x <- as.character(x %||% "")
	x <- tolower(trimws(x))
	x <- gsub("[\u2019\u2018`']", "'", x, perl = TRUE)
	x <- gsub("\\s+", " ", x)
	x
}

#' Résoudre un `theme_id` à partir d'un id ou d'un libellé
#'
#' @param theme_id Identifiant court (`REHF`, …).
#' @param theme Libellé long du thème.
#' @param bank Banque optionnelle.
#'
#' @return Une chaîne `theme_id`, ou `NULL`.
#' @noRd
cq_theme_art_resolve_id <- function(theme_id = NULL, theme = NULL, bank = NULL) {
	carte <- cq_theme_art_map()

	if (!is.null(theme_id) && nzchar(theme_id) && theme_id %in% names(carte)) {
		return(theme_id)
	}

	if (is.null(theme) || !nzchar(theme)) {
		return(NULL)
	}

	if (is.null(bank)) {
		bank <- tryCatch(bank_load(), error = function(e) NULL)
	}

	if (!is.null(bank)) {
		themes <- bank_themes(bank)
		idx <- match(cq_theme_art_norm(theme), cq_theme_art_norm(themes$theme))
		if (!is.na(idx) && themes$theme_id[[idx]] %in% names(carte)) {
			return(themes$theme_id[[idx]])
		}
	}

	# Repli : préfixe du libellé
	norm <- cq_theme_art_norm(theme)
	aliases <- c(
		REHF = "reperes d'histoire",
		PRVS = "principes, valeurs",
		INPO = "institutions politiques",
		CIDD = "citoyennete, droits",
		SOCF = "societe et culture",
		GEOG = "geographie",
		ECON = "economie",
		FUEM = "france, union europeenne"
	)
	norm_ascii <- iconv(norm, to = "ASCII//TRANSLIT")
	for (id in names(aliases)) {
		if (grepl(aliases[[id]], norm_ascii, fixed = TRUE)) {
			return(id)
		}
	}

	NULL
}

#' URL relative de l'illustration d'un thème
#'
#' @return Chemin `www/...` ou `NULL`.
#' @noRd
cq_theme_art_src <- function(theme_id = NULL, theme = NULL, bank = NULL) {
	id <- cq_theme_art_resolve_id(theme_id, theme, bank)
	if (is.null(id)) {
		return(NULL)
	}

	paste0("www/", cq_theme_art_map()[[id]])
}

#' Figure / image d'illustration d'un thème
#'
#' @param theme_id,theme Identifiants du thème.
#' @param class Classe CSS du conteneur.
#' @param show_caption Afficher le libellé sous l'image.
#'
#' @return Tag HTML, ou `NULL`.
#' @noRd
cq_theme_art_ui <- function(theme_id = NULL,
														theme = NULL,
														class = "theme-art-figure",
														show_caption = FALSE,
														bank = NULL) {
	src <- cq_theme_art_src(theme_id, theme, bank)
	if (is.null(src)) {
		return(NULL)
	}

	libelle <- theme
	if (is.null(libelle) || !nzchar(libelle)) {
		id <- cq_theme_art_resolve_id(theme_id, theme, bank)
		if (!is.null(bank) || !is.null(id)) {
			bank <- bank %||% tryCatch(bank_load(), error = function(e) NULL)
			if (!is.null(bank) && !is.null(id)) {
				themes <- bank_themes(bank)
				libelle <- themes$theme[match(id, themes$theme_id)]
			}
		}
		libelle <- libelle %||% "Th\u00e8me"
	}

	shiny::tags$figure(
		class = class,
		shiny::tags$img(
			src = src,
			alt = paste("Illustration \u2014", libelle),
			loading = "lazy",
			decoding = "async"
		),
		if (isTRUE(show_caption)) {
			shiny::tags$figcaption(libelle)
		}
	)
}

#' Panneau « Où ça pêche » — thèmes fragiles illustrés
#'
#' Affiche les grands thèmes les moins réussis avec leur illustration,
#' un score et une barre de progression. Style rapport, pas une galerie décorative.
#'
#' @param par_theme Tableau de [stats_par()] sur `theme` (`cle`, `reussite`, `n`).
#' @param bank Banque optionnelle.
#' @param limite Nombre max de lignes.
#' @param seuil Seuil de fragilité (0–1) ; les thèmes au-dessus restent listés
#'   s'il n'y a pas assez de thèmes fragiles, mais sans pastille d'alerte.
#'
#' @return Tag HTML.
#' @noRd
stats_report_ou_ca_peche <- function(par_theme,
																		 bank = NULL,
																		 limite = 6L,
																		 seuil = 0.55) {
	if (is.null(par_theme) || !nrow(par_theme)) {
		return(shiny::p(
			class = "chart-note",
			"Pas encore assez de r\u00e9ponses pour rep\u00e9rer les th\u00e8mes fragiles."
		))
	}

	bank <- bank %||% tryCatch(bank_load(), error = function(e) NULL)
	ok <- par_theme[is.finite(par_theme$reussite) & par_theme$n > 0, , drop = FALSE]
	if (!nrow(ok)) {
		return(shiny::p(
			class = "chart-note",
			"Pas encore assez de r\u00e9ponses pour rep\u00e9rer les th\u00e8mes fragiles."
		))
	}

	ok <- ok[order(ok$reussite, -ok$n), , drop = FALSE]
	lignes <- utils::head(ok, limite)

	rangees <- lapply(seq_len(nrow(lignes)), function(i) {
		nom <- lignes$cle[[i]]
		pct <- as.integer(round(100 * lignes$reussite[[i]]))
		fragile <- lignes$reussite[[i]] < seuil
		ton <- stats_chaleur_ton(lignes$reussite[[i]])
		art <- cq_theme_art_ui(
			theme = nom,
			bank = bank,
			class = "theme-art-figure peche-art"
		)

		shiny::tags$article(
			class = paste(
				"peche-row",
				if (fragile) "peche-row--fragile" else "peche-row--ok"
			),
			if (!is.null(art)) {
				art
			} else {
				shiny::tags$div(
					class = "peche-art-fallback",
					`aria-hidden` = "true",
					substr(nom, 1L, 1L)
				)
			},
			shiny::tags$div(
				class = "peche-body",
				shiny::tags$div(
					class = "peche-head",
					shiny::tags$h3(nom),
					shiny::tags$span(
						class = paste("peche-score", paste0("cq-ton--", ton)),
						paste0(pct, " %")
					)
				),
				shiny::tags$div(
					class = "peche-track",
					role = "meter",
					`aria-valuenow` = pct,
					`aria-valuemin` = 0,
					`aria-valuemax` = 100,
					`aria-label` = glue::glue("R\u00e9ussite {pct} % sur {nom}"),
					shiny::tags$span(
						class = paste("peche-fill", paste0("cq-ton--", ton)),
						style = paste0("width:", pct, "%")
					)
				),
				shiny::tags$p(
					class = "peche-meta",
					glue::glue(
						"{lignes$n[[i]]} r\u00e9ponse(s)",
						if (fragile) {
							glue::glue(" \u00b7 sous le seuil ({as.integer(round(100 * seuil))} %)")
						} else {
							" \u00b7 au-dessus du seuil"
						}
					)
				)
			)
		)
	})

	n_fragiles <- sum(lignes$reussite < seuil, na.rm = TRUE)
	intro <- if (n_fragiles > 0L) {
		glue::glue(
			"{n_fragiles} th\u00e8me(s) sous {as.integer(round(100 * seuil))} % ",
			"parmi les {nrow(lignes)} affich\u00e9(s)."
		)
	} else {
		glue::glue(
			"Aucun th\u00e8me sous {as.integer(round(100 * seuil))} % ",
			"pour l'instant \u2014 voici les moins r\u00e9ussis."
		)
	}

	shiny::div(
		class = "peche-panel",
		shiny::tags$p(class = "peche-intro", intro),
		shiny::div(class = "peche-list", rangees)
	)
}
