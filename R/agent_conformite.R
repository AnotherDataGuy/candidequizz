# Agent A-4 : contrôleur de conformité.
#
# Agent déterministe, et c'est tout l'intérêt : compter des options, repérer un
# doublon ou vérifier qu'un marqueur de trou existe ne demande pas de modèle.
# Cet agent absorbe la grande majorité des rejets sans dépenser un token, et
# ne laisse au vérificateur factuel que ce qu'il est seul à pouvoir juger.

#' Longueur maximale d'un énoncé, en caractères
#'
#' Un énoncé trop long ne tient pas sur un écran de téléphone et n'a pas sa
#' place dans un entretien oral.
#'
#' @return Un entier.
#' @noRd
conformite_enonce_max <- function() {
	300L
}

#' Contrôles communs à tous les types
#'
#' @param item L'item à contrôler.
#'
#' @return Un vecteur de motifs de rejet.
#' @noRd
conformite_motifs_communs <- function(item) {
	motifs <- character(0)

	if (!vld_texte_ok(item$enonce, mini = 10L)) {
		motifs <- c(motifs, "\u00c9nonc\u00e9 absent ou trop court.")
	} else if (nchar(item$enonce) > conformite_enonce_max()) {
		motifs <- c(
			motifs,
			glue::glue(
				"\u00c9nonc\u00e9 de {nchar(item$enonce)} caract\u00e8res, ",
				"maximum {conformite_enonce_max()}."
			)
		)
	}

	if (!vld_texte_ok(item$explication, mini = 15L)) {
		motifs <- c(motifs, "Explication absente ou trop courte.")
	}

	motifs
}

#' Valider un item
#'
#' Enchaîne les contrôles communs puis ceux du type. Fonction et non agent :
#' le réparateur la réutilise pour vérifier son propre travail, sans avoir à
#' relancer une étape de chaîne.
#'
#' @param item L'item à valider.
#' @param consigne La consigne d'origine.
#'
#' @return `list(ok, motifs)`.
#' @noRd
conformite_valider <- function(item, consigne) {
	motifs <- conformite_motifs_communs(item)
	verdict_type <- question_type(item$type)$valider(item, consigne)

	vld_verdict(c(motifs, verdict_type$motifs))
}

#' Retrouver la consigne d'un item
#'
#' @param consignes Liste des consignes du lot.
#' @param numero Numéro recherché.
#'
#' @return La consigne.
#' @noRd
conformite_consigne <- function(consignes, numero) {
	numeros <- vapply(consignes, function(c) as.integer(c$numero), integer(1))
	position <- which(numeros == as.integer(numero))

	if (!length(position)) {
		rlang::abort(
			glue::glue("Aucune consigne ne porte le num\u00e9ro {numero}."),
			class = "candidequizz_pipeline_consigne_introuvable"
		)
	}

	consignes[[position[[1L]]]]
}

#' Décrire un rejet
#'
#' @param item L'item rejeté.
#' @param consigne La consigne d'origine.
#' @param motifs Motifs du rejet.
#' @param origine Agent à l'origine du rejet.
#'
#' @return Un rejet.
#' @noRd
conformite_rejet <- function(item, consigne, motifs, origine) {
	list(
		item = item,
		consigne = consigne,
		motifs = as.character(motifs),
		origine = origine
	)
}

#' Contrôler la conformité des items d'un lot
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `items` réduit aux items conformes et `rejets`
#'   renseigné.
#' @noRd
agent_conformite <- function(ctx) {
	conformes <- list()
	rejets <- list()

	for (item in ctx$items) {
		consigne <- conformite_consigne(ctx$consignes, item$numero)
		verdict <- conformite_valider(item, consigne)

		if (verdict$ok) {
			conformes <- c(conformes, list(item))
		} else {
			rejets <- c(rejets, list(conformite_rejet(item, consigne, verdict$motifs, "conformite")))
		}
	}

	ctx$items <- conformes
	ctx$rejets <- rejets

	ctx_detail(ctx, n_items = length(conformes), n_rejets = length(rejets))
}
