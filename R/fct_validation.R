# Outils de validation partagés par les types de questions.
#
# Ces contrôles sont volontairement déterministes et gratuits : c'est la règle
# de conception de la chaîne d'agents, on n'appelle le LLM que pour ce qu'il est
# seul à savoir faire. Compter des options ou repérer un doublon est du ressort
# du code R.

#' Normaliser un texte pour comparaison
#'
#' Minuscules, accents retirés, ponctuation remplacée par des espaces, espaces
#' réduits. Sert à repérer les doublons et les réponses divulguées malgré une
#' différence de casse, d'accent ou de ponctuation.
#'
#' @param x Un vecteur de caractères.
#'
#' @return Un vecteur de caractères normalisé.
#' @noRd
vld_normaliser <- function(x) {
	x <- tolower(as.character(x))

	# chartr plutôt qu'iconv : la translittération ASCII est peu fiable selon
	# les plateformes, alors qu'une table de correspondance explicite ne varie pas.
	x <- chartr(
		"\u00e0\u00e2\u00e4\u00e1\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00ec\u00f4\u00f6\u00f2\u00f9\u00fb\u00fc\u00e7",
		"aaaaeeeeiiiooouuuc",
		x
	)

	x <- gsub("[^a-z0-9 ]+", " ", x)
	x <- gsub("\\s+", " ", x)

	trimws(x)
}

#' Le champ est-il un texte utilisable
#'
#' @param x Valeur à contrôler.
#' @param mini Longueur minimale, en caractères.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
vld_texte_ok <- function(x, mini = 1L) {
	is.character(x) &&
		length(x) == 1L &&
		!is.na(x) &&
		nchar(trimws(x)) >= mini
}

#' La bonne réponse est-elle divulguée dans l'énoncé
#'
#' Un énoncé qui contient mot pour mot sa propre réponse ne teste rien. Le
#' contrôle ne s'applique qu'aux réponses d'au moins quatre caractères, pour ne
#' pas déclencher de faux positifs sur des réponses très courtes.
#'
#' @param enonce L'énoncé de la question.
#' @param reponse Le texte de la bonne réponse.
#'
#' @return `TRUE` si la réponse apparaît dans l'énoncé.
#' @noRd
vld_fuite_reponse <- function(enonce, reponse) {
	reponse_n <- vld_normaliser(reponse)

	if (nchar(reponse_n) < 4L) {
		return(FALSE)
	}

	grepl(reponse_n, vld_normaliser(enonce), fixed = TRUE)
}

#' Une des bonnes r\u00e9ponses fuit-elle dans l'\u00e9nonc\u00e9
#'
#' @param enonce L'\u00e9nonc\u00e9.
#' @param reponses Textes des bonnes r\u00e9ponses.
#'
#' @return `TRUE` si au moins une r\u00e9ponse appara\u00eet dans l'\u00e9nonc\u00e9.
#' @noRd
vld_fuites_reponses <- function(enonce, reponses) {
	any(vapply(reponses, function(r) vld_fuite_reponse(enonce, r), logical(1)))
}

#' Un libell\u00e9 de classement contient-il sa propre date
#'
#' Classer « Prise de la Bastille (1789) » ne teste plus la chronologie : la
#' date est d\u00e9j\u00e0 \u00e9crite. On refuse les ann\u00e9es entre parenth\u00e8ses, apr\u00e8s un
#' tiret, ou apr\u00e8s une virgule en fin de libell\u00e9.
#'
#' @param texte Libell\u00e9 d'un \u00e9l\u00e9ment \u00e0 classer.
#'
#' @return `TRUE` si une date annot\u00e9e est visible.
#' @noRd
vld_fuite_date <- function(texte) {
	texte <- as.character(texte[[1L]] %||% "")

	grepl("\\([^)]*\\d{3,4}[^)]*\\)", texte) ||
		grepl("[-\u2013\u2014]\\s*\\d{3,4}\\s*$", texte) ||
		grepl(",\\s*\\d{3,4}\\s*$", texte)
}

#' Formulations d'options à proscrire
#'
#' « Aucune des réponses », « toutes les réponses », « A et B » : ces options
#' relèvent du test d'aptitude et non de la connaissance visée, et se prêtent
#' mal à un diagnostic d'erreur.
#'
#' @param textes Les libellés des options.
#'
#' @return Les libellés fautifs, ou un vecteur vide.
#' @noRd
vld_formulations_interdites <- function(textes) {
	motifs <- c(
		"aucune de",
		"aucune des",
		"toutes les reponses",
		"toutes ces reponses",
		"les deux reponses",
		"reponses a et b",
		"je ne sais pas",
		# Options qui s'auto-disqualifient. Observé en conditions réelles :
		# « 1950, annee sans lien direct avec la naissance de la Ve Republique ».
		"sans lien",
		"sans rapport",
		"hors sujet",
		"aucun rapport"
	)

	normalises <- vld_normaliser(textes)
	fautifs <- vapply(
		normalises,
		function(texte) any(vapply(motifs, function(m) grepl(m, texte, fixed = TRUE), logical(1))),
		logical(1)
	)

	textes[fautifs]
}

#' Contrôles communs à toute liste d'options
#'
#' @param options Liste d'options, chacune avec `texte` et `is_correct`.
#'
#' @return Un vecteur de motifs de rejet, vide si tout est conforme.
#' @noRd
vld_options_communes <- function(options) {
	motifs <- character(0)

	if (!is.list(options) || !length(options)) {
		return("Aucune option de r\u00e9ponse fournie.")
	}

	textes <- vapply(
		options,
		function(o) if (vld_texte_ok(o$texte)) o$texte else NA_character_,
		character(1)
	)

	if (anyNA(textes)) {
		motifs <- c(motifs, "Au moins une option a un libell\u00e9 vide.")
		return(motifs)
	}

	if (any(duplicated(vld_normaliser(textes)))) {
		motifs <- c(motifs, "Deux options ont le m\u00eame libell\u00e9.")
	}

	interdites <- vld_formulations_interdites(textes)

	if (length(interdites)) {
		motifs <- c(
			motifs,
			glue::glue(
				"Formulation d'option proscrite : {paste(sprintf('\u00ab %s \u00bb', interdites), collapse = ', ')}."
			)
		)
	}

	corrects <- vapply(
		options,
		function(o) isTRUE(o$is_correct),
		logical(1)
	)

	if (!length(corrects) || anyNA(corrects)) {
		motifs <- c(motifs, "Le marqueur `is_correct` est absent ou non bool\u00e9en.")
	}

	motifs
}

#' Assembler un verdict de validation
#'
#' @param motifs Vecteur des motifs de rejet.
#'
#' @return `list(ok, motifs)`.
#' @noRd
vld_verdict <- function(motifs = character(0)) {
	motifs <- motifs[nzchar(motifs)]

	list(ok = length(motifs) == 0L, motifs = as.character(motifs))
}

#' Options correctes d'un item
#'
#' @param options Liste d'options.
#'
#' @return Un vecteur logique.
#' @noRd
vld_corrects <- function(options) {
	vapply(options, function(o) isTRUE(o$is_correct), logical(1))
}

#' Libellés des options d'un item
#'
#' @param options Liste d'options.
#'
#' @return Un vecteur de caractères.
#' @noRd
vld_textes <- function(options) {
	vapply(options, function(o) as.character(o$texte %||% ""), character(1))
}

#' Valeur par défaut si NULL
#'
#' @param x Valeur éventuellement `NULL`.
#' @param y Valeur de remplacement.
#'
#' @return `x` si non `NULL`, sinon `y`.
#' @noRd
`%||%` <- function(x, y) {
	if (is.null(x)) y else x
}
