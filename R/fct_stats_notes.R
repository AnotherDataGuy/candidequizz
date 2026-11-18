# Notes de lecture déterministes : mêmes données et même périmètre que le
# graphique. Aucun appel LLM, et aucune extrapolation à partir d'un petit lot.

stats_note <- function(texte) {
	shiny::div(class = "cq-lecture", shiny::span(class = "cq-lecture-titre", "Note de lecture"),
		shiny::p(texte))
}

stats_note_categories <- function(tableau) {
	ok <- tableau[is.finite(tableau$reussite) & tableau$n > 0, , drop = FALSE]
	if (!nrow(ok)) return("Aucun score mesurable sur les cat\u00e9gories affich\u00e9es.")
	faible <- which.min(ok$reussite)
	fort <- which.max(ok$reussite)
	texte <- if (nrow(ok) == 1L) {
		glue::glue("\u00ab {ok$cle[[1L]]} \u00bb : {round(100 * ok$reussite[[1L]])} % sur {ok$n[[1L]]} r\u00e9ponse(s).")
	} else if (all(ok$reussite == ok$reussite[[1L]])) {
		glue::glue("Les {nrow(ok)} cat\u00e9gories affich\u00e9es ont le m\u00eame score : {round(100 * ok$reussite[[1L]])} %.")
	} else {
		glue::glue("Parmi les cat\u00e9gories affich\u00e9es, \u00ab {ok$cle[[fort]]} \u00bb obtient le score le plus \u00e9lev\u00e9 ({round(100 * ok$reussite[[fort]])} %, {ok$n[[fort]]} r\u00e9ponse(s)). \u00ab {ok$cle[[faible]]} \u00bb est la moins r\u00e9ussie ({round(100 * ok$reussite[[faible]])} %, {ok$n[[faible]]} r\u00e9ponse(s)).")
	}
	paste(texte, "Le score inclut les cr\u00e9dits partiels.",
		if (any(ok$n < 5L)) "Moins de 5 r\u00e9ponses dans certaines cat\u00e9gories : ces rep\u00e8res restent \u00e0 confirmer." else "Les cat\u00e9gories peuvent avoir des effectifs diff\u00e9rents.")
}

stats_note_issues <- function(reponses) {
	n <- nrow(reponses)
	if (!n) return("Aucune r\u00e9ponse enregistr\u00e9e.")
	comptes <- table(factor(stats_issue(reponses), levels = c("juste", "partielle", "fausse", "passee")))
	glue::glue("Sur les {n} r\u00e9ponses affich\u00e9es : {comptes[['juste']]} justes ({round(100 * comptes[['juste']] / n)} %), {comptes[['partielle']]} partielles, {comptes[['fausse']]} fausses et {comptes[['passee']]} pass\u00e9es. Une r\u00e9ponse partielle contribue au score, sans compter comme une r\u00e9ponse enti\u00e8rement juste.")
}

stats_note_parcours <- function(reponses) {
	justes <- stats_issue(reponses) == "juste"
	series <- rle(justes)
	serie <- max(c(0L, series$lengths[series$values]))
	score <- reponses$score[is.finite(reponses$score)]
	paste(if (length(score)) glue::glue("Score moyen : {round(100 * mean(score))} % sur {length(score)} r\u00e9ponse(s) not\u00e9e(s).") else "Aucun score mesurable.",
		glue::glue("Votre plus longue s\u00e9rie compte {serie} r\u00e9ponse(s) enti\u00e8rement juste(s)."),
		"L'axe horizontal suit l'ordre des questions ; la hauteur indique le cr\u00e9dit obtenu, de 0 \u00e0 100 %.")
}

stats_note_evolution <- function(evo) {
	ok <- evo[is.finite(evo$reussite), , drop = FALSE]
	if (!nrow(ok)) return("Aucun score disponible pour les s\u00e9ances affich\u00e9es.")
	fin <- round(100 * utils::tail(ok$reussite, 1L))
	if (nrow(ok) == 1L) return(glue::glue("Une seule s\u00e9ance mesur\u00e9e, \u00e0 {fin} %. Une autre s\u00e9ance permettra une premi\u00e8re comparaison."))
	delta <- round(100 * (utils::tail(ok$reussite, 1L) - utils::head(ok$reussite, 1L)), 1)
	glue::glue("Sur {nrow(ok)} s\u00e9ances mesur\u00e9es, le score passe de {round(100 * ok$reussite[[1L]])} % \u00e0 {fin} %, soit {if (delta > 0) '+' else ''}{delta} point(s). Les th\u00e8mes, les difficult\u00e9s et les nombres de questions peuvent varier d'une s\u00e9ance \u00e0 l'autre.")
}
