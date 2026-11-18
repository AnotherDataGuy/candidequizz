# Messages utilisateur.
#
# Ce que l'écran affiche est toujours une phrase en français, jamais
# une trace R. Les conditions de la chaîne sont déjà rédigées ; on les
# nettoie. Le reste tombe sur un repli actionnable.

#' Message affichable à partir d'une condition
#'
#' @param condition Une condition, un texte, ou `NULL`.
#'
#' @return Une chaîne d'une ou deux phrases, en français.
#' @noRd
cq_message_utilisateur <- function(condition) {
	if (is.null(condition)) {
		return(cq_message_repli())
	}

	if (is.character(condition)) {
		return(cq_message_nettoyer(condition))
	}

	classes <- class(condition)

	if ("candidequizz_llm_cle_absente" %in% classes) {
		return(paste(
			"Aucune cl\u00e9 d'API n'est disponible.",
			"La s\u00e9ance reste jouable en mode d\u00e9grad\u00e9."
		))
	}

	if (any(grepl("^candidequizz_llm_", classes))) {
		return(paste(
			"Le service de g\u00e9n\u00e9ration n'a pas r\u00e9pondu.",
			"R\u00e9essayez dans un instant : vos r\u00e9ponses d\u00e9j\u00e0 donn\u00e9es sont conserv\u00e9es."
		))
	}

	if (any(grepl("^candidequizz_pipeline_", classes))) {
		return(paste(
			"La pr\u00e9paration des questions a \u00e9chou\u00e9.",
			"R\u00e9essayez : ce qui est d\u00e9j\u00e0 fait n'est pas perdu."
		))
	}

	if ("candidequizz_parametres_invalides" %in% classes) {
		return(cq_message_nettoyer(conditionMessage(condition)))
	}

	cq_message_nettoyer(conditionMessage(condition))
}

#' Repli quand le message d'origine n'est pas montrable
#'
#' @return Une chaîne.
#' @noRd
cq_message_repli <- function() {
	paste(
		"Une erreur inattendue s'est produite.",
		"R\u00e9essayez, ou revenez plus tard : vos r\u00e9ponses d\u00e9j\u00e0 donn\u00e9es sont conserv\u00e9es."
	)
}

#' Nettoyer un texte de condition
#'
#' @param texte Texte brut, éventuellement multiligne.
#'
#' @return Une phrase montrable, ou le repli.
#' @noRd
cq_message_nettoyer <- function(texte) {
	texte <- paste(as.character(texte), collapse = "\n")
	texte <- gsub("\033\\[[0-9;]*m", "", texte)
	lignes <- trimws(unlist(strsplit(texte, "\n", fixed = TRUE), use.names = FALSE))
	lignes <- lignes[nzchar(lignes)]
	lignes <- sub("^[\u2716\u2139*!iIxX]\\s+", "", lignes)
	lignes <- lignes[!grepl(
		"^(Error in |Error: |Caused by|Call:|Backtrace|Traceback)",
		lignes
	)]

	if (!length(lignes)) {
		return(cq_message_repli())
	}

	premier <- lignes[[1L]]

	if (grepl(
		"object ['`].*['`] not found|unexpected|cannot open the connection|subscript out of bounds|argument is of length zero",
		premier,
		ignore.case = TRUE
	)) {
		return(cq_message_repli())
	}

	if (grepl("\\.(R|r)\\b", premier) && grepl(":[0-9]+", premier)) {
		return(cq_message_repli())
	}

	if (!nzchar(premier) || nchar(premier) > 280L) {
		return(cq_message_repli())
	}

	premier
}
