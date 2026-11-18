# Graphiques SVG de la page Statistiques.
#
# Dessinés ici, sans {ggplot2} ni widget : le rendu suit la charte, reste hors
# ligne et n'ajoute aucune dépendance.

#' Projeter un domaine numérique sur une image
#'
#' @param x Valeurs.
#' @param domaine `c(min, max)` des données.
#' @param image `c(min, max)` en pixels.
#'
#' @return Un vecteur numérique de la même longueur que `x`.
#' @noRd
stats_echelle <- function(x, domaine, image) {
	span <- domaine[[2L]] - domaine[[1L]]

	if (!is.finite(span) || span == 0) {
		return(rep(mean(image), length(x)))
	}

	image[[1L]] + (x - domaine[[1L]]) / span * (image[[2L]] - image[[1L]])
}

#' Couleur d'un taux de r\u00e9ussite
#'
#' @param reussite Taux entre 0 et 1, ou `NA`.
#'
#' @return Un hex.
#' @noRd
stats_ton_couleur <- function(reussite) {
	switch(
		stats_chaleur_ton(reussite),
		vide = "#d7d9c9",
		bas = "#e9bcab",
		fragile = "#e7d4a2",
		moyen = "#c7dca7",
		haut = "#718c54",
		"#29352a"
	)
}

#' Classe de ton pour un taux
#'
#' @param reussite Taux entre 0 et 1, ou `NA`.
#'
#' @return Une classe CSS.
#' @noRd
stats_ton_classe <- function(reussite) {
	paste0("cq-ton--", stats_chaleur_ton(reussite))
}

#' Tronquer un libellé de graphique
#'
#' @param texte Chaîne.
#' @param n Longueur maximale.
#'
#' @return Une chaîne.
#' @noRd
stats_tronque <- function(texte, n = 22L) {
	texte <- as.character(texte[[1L]] %||% "")

	if (nchar(texte) <= n) {
		return(texte)
	}

	paste0(substr(texte, 1L, n - 1L), "\u2026")
}

#' Cadre SVG
#'
#' @param largeur,hauteur ViewBox.
#' @param libelle `aria-label`.
#' @param ... Enfants SVG.
#'
#' @return Un tag `<svg>`.
#' @noRd
stats_svg <- function(largeur, hauteur, libelle, ...) {
	shiny::tags$svg(
		xmlns = "http://www.w3.org/2000/svg",
		role = "img",
		class = "cq-graphe report-chart",
		width = "100%",
		height = "100%",
		viewBox = glue::glue("0 0 {largeur} {hauteur}"),
		preserveAspectRatio = "xMidYMid meet",
		style = glue::glue(
			"aspect-ratio: {largeur} / {hauteur}; width: 100%; height: auto; display: block;"
		),
		"aria-label" = libelle,
		...
	)
}

#' Date courte, sans casser `format()` si la colonne n'est pas un POSIXct
#'
#' @param x Date, date-heure ou chaîne.
#'
#' @return Une chaîne `jj/mm`.
#' @noRd
stats_date_courte <- function(x) {
	x <- as.POSIXct(x, tz = "Europe/Paris")
	sortie <- format(x, "%d/%m")
	sortie[!is.finite(as.numeric(x))] <- "\u2014"
	sortie
}

#' Afficher un graphique ou le motif d'\u00e9chec, jamais une page blanche
#'
#' @param expr Expression qui produit un tag.
#'
#' @return Un tag.
#' @noRd
stats_graphe_safe <- function(expr) {
	tryCatch(
		force(expr),
		error = function(e) {
			shiny::p(class = "cq-aide", "Ce graphique n'a pas pu \u00eatre dessin\u00e9.")
		}
	)
}

#' Points `x,y` pour un attribut SVG
#'
#' @param x,y Coordonnées.
#'
#' @return Une chaîne.
#' @noRd
stats_svg_points <- function(x, y) {
	paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
}

#' Légende compacte sous un graphique
#'
#' @param ... Paires nom / texte, ou tags.
#'
#' @return Un tag.
#' @noRd
stats_graphe_legende <- function(...) {
	shiny::div(class = "cq-graphe-legende", ...)
}

#' Pastille de légende
#'
#' @param classe Modificateur (`juste`, `fausse`, …).
#' @param texte Libellé.
#'
#' @return Un tag.
#' @noRd
stats_graphe_cle <- function(classe, texte) {
	shiny::span(
		class = paste("cq-graphe-cle", paste0("cq-graphe-cle--", classe)),
		texte
	)
}

#' Courbe d'évolution des séances
#'
#' @param evo Tableau de [stats_evolution()].
#'
#' @return Un tag.
#' @noRd
stats_graphe_evolution <- function(evo) {
	if (!nrow(evo)) {
		return(shiny::p(class = "cq-aide", "Pas encore d'\u00e9volution."))
	}

	largeur <- 720
	hauteur <- 240
	gauche <- 48
	droite <- 42
	haut <- 32
	bas <- 30
	xs <- if (nrow(evo) == 1L) {
		(gauche + largeur - droite) / 2
	} else {
		stats_echelle(seq_len(nrow(evo)), c(1, nrow(evo)), c(gauche, largeur - droite))
	}
	ys <- stats_echelle(
		pmin(1, pmax(0, ifelse(is.finite(evo$reussite), evo$reussite, 0))),
		c(0, 1),
		c(hauteur - bas, haut)
	)
	y_seuil <- stats_echelle(0.55, c(0, 1), c(hauteur - bas, haut))
	y0 <- hauteur - bas
	dernier <- nrow(evo)
	premier <- if (is.finite(evo$reussite[[1L]])) round(100 * evo$reussite[[1L]]) else NA
	fin <- if (is.finite(evo$reussite[[dernier]])) round(100 * evo$reussite[[dernier]]) else NA

	axes <- lapply(c(0, 0.55, 1), function(tick) {
		y <- stats_echelle(tick, c(0, 1), c(hauteur - bas, haut))
		shiny::tagList(
			shiny::tags$line(
				x1 = gauche,
				x2 = largeur - droite,
				y1 = y,
				y2 = y,
				class = if (identical(tick, 0.55)) "cq-graphe-seuil" else "cq-graphe-grille"
			),
			shiny::tags$text(
				x = gauche - 6,
				y = y + 3,
				class = "cq-graphe-axe cq-graphe-axe--y",
				if (identical(tick, 0.55)) "55" else as.character(round(100 * tick))
			)
		)
	})

	points <- lapply(seq_len(nrow(evo)), function(i) {
		part <- if (is.finite(evo$reussite[[i]])) round(100 * evo$reussite[[i]]) else NA
		rayon <- if (i == dernier) 5.2 else 3.6
		shiny::tags$circle(
			cx = xs[[i]],
			cy = ys[[i]],
			r = rayon,
			class = paste(
				"cq-graphe-point",
				stats_ton_classe(evo$reussite[[i]]),
				if (i == dernier) "cq-graphe-point--dernier" else ""
			),
			shiny::tags$title(glue::glue(
				"{stats_date_courte(evo$cree_le[[i]])} \u00b7 ",
				"{if (is.finite(part)) paste0(part, ' %') else '\u2014'} \u00b7 ",
				"{evo$n[[i]]} question(s)"
			))
		)
	})

	dates <- lapply(unique(round(seq(1, nrow(evo), length.out = min(6L, nrow(evo))))), function(i) {
		shiny::tags$text(
			x = xs[[i]],
			y = hauteur - 8,
			class = "cq-graphe-axe cq-graphe-axe--x",
			stats_date_courte(evo$cree_le[[i]])
		)
	})

	shiny::div(
		class = "cq-graphe-bloc",
		stats_svg(
			largeur,
			hauteur,
			glue::glue(
				"\u00c9volution du score sur {nrow(evo)} s\u00e9ance(s)",
				if (is.finite(premier) && is.finite(fin)) {
					glue::glue(" : de {premier} \u00e0 {fin} %")
				} else {
					""
				}
			),
			shiny::tags$rect(
				x = gauche,
				y = haut,
				width = largeur - gauche - droite,
				height = hauteur - haut - bas,
				class = "cq-graphe-fond"
			),
			axes,
			if (nrow(evo) >= 2L) {
				shiny::tagList(
					shiny::tags$path(
						d = glue::glue(
							"M {sprintf('%.1f,%.1f', xs[[1L]], y0)} ",
							"L {paste(sprintf('%.1f,%.1f', xs, ys), collapse = ' L ')} ",
							"L {sprintf('%.1f,%.1f', xs[[dernier]], y0)} Z"
						),
						class = "cq-graphe-aire"
					),
					shiny::tags$polyline(
						points = stats_svg_points(xs, ys),
						class = "cq-graphe-ligne"
					)
				)
			},
			points,
			if (is.finite(fin)) {
				shiny::tags$text(
					x = xs[[dernier]],
					`text-anchor` = "middle",
					y = ys[[dernier]] - 10,
					class = "cq-graphe-valeur",
					glue::glue("{fin} %")
				)
			},
			dates
		),
		stats_graphe_legende(
			shiny::span("Chaque point est une s\u00e9ance."),
			stats_graphe_cle("seuil", "Rep\u00e8re p\u00e9dagogique : 55 %")
		),
		stats_note(stats_note_evolution(evo))
	)
}

#' Lollipops de r\u00e9ussite
#'
#' @param tableau Tableau de [stats_par()].
#' @param limite Nombre de lignes.
#' @param domaine_fixe Échelle 0–100, même si les valeurs sont groupées.
#'
#' @return Un tag.
#' @noRd
stats_graphe_lollipop <- function(tableau, limite = 6L, domaine_fixe = TRUE) {
	if (!nrow(tableau)) {
		return(shiny::p(class = "cq-aide", "Pas encore de donn\u00e9es."))
	}

	# Le texte est en HTML : il se replie sur mobile sans sortir du viewBox.
	# L'échelle reste commune (0–100 %) pour comparer toutes les catégories.
	tableau <- utils::head(tableau, limite)
	shiny::div(class = "cq-graphe-bloc cq-categories",
		lapply(seq_len(nrow(tableau)), function(i) {
			part <- tableau$reussite[[i]]
			mesure <- is.finite(part)
			pct <- if (mesure) round(100 * part) else NA_real_
			shiny::div(class = "cq-category",
				shiny::div(class = "cq-category-heading",
					shiny::span(tableau$cle[[i]]),
					shiny::strong(if (mesure) paste0(pct, " %") else "Non mesur\u00e9")),
				shiny::div(class = "cq-category-track", `aria-hidden` = "true",
					shiny::span(class = paste("cq-category-fill", stats_ton_classe(part)),
						style = paste0("width:", if (mesure) max(0, min(100, pct)) else 0, "%;")),
					shiny::tags$i(class = "cq-category-threshold")),
				shiny::div(class = "cq-category-detail",
					shiny::span(glue::glue("{tableau$n[[i]]} r\u00e9ponse(s)")),
					if ("n_justes" %in% names(tableau)) shiny::span(glue::glue("{tableau$n_justes[[i]]} enti\u00e8rement juste(s)")),
					if ("temps_median_s" %in% names(tableau) && is.finite(tableau$temps_median_s[[i]]))
						shiny::span(glue::glue("M\u00e9diane : {round(tableau$temps_median_s[[i]])} s"))))
		}),
		stats_graphe_legende(shiny::span("\u00c9chelle de 0 \u00e0 100 %"),
			stats_graphe_cle("seuil", "Rep\u00e8re p\u00e9dagogique : 55 %")),
		stats_note(stats_note_categories(tableau)))
}

