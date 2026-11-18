# Révision ciblée (P-01).
#
# À partir de l'historique, on retient les sous-thèmes les plus fragiles et
# on en fait les poids du premier tirage. Le régleur de séance peut ensuite
# les ajuster avec les réponses du lot en cours.

#' Préparer une séance de révision ciblée
#'
#' @param n Nombre de sous-thèmes à retenir. `NULL` pour la valeur de
#'   configuration.
#' @param seances Liste de séances. `NULL` pour tout le store.
#' @param bank Banque, pour les libellés.
#'
#' @return Une liste `ok`, `full_theme_ids`, `poids_themes`, `apercu`.
#' @noRd
revision_preparer <- function(n = NULL,
															seances = NULL,
															bank = bank_load()) {
	n <- as.integer(n %||% cq_config("seance", "revision_n_sous_themes"))
	seuil <- as.numeric(cq_config("seance", "revision_seuil"))
	seances <- seances %||% stats_charger()
	par <- stats_par(stats_reponses(seances), "full_theme_id")

	vide <- list(
		ok = FALSE,
		full_theme_ids = character(0),
		poids_themes = NULL,
		apercu = tibble::tibble(
			full_theme_id = character(0),
			sub_theme = character(0),
			reussite = numeric(0),
			n = integer(0)
		)
	)

	if (!nrow(par)) {
		return(vide)
	}

	fragiles <- par[is.finite(par$reussite) & par$reussite < seuil, , drop = FALSE]

	if (!nrow(fragiles)) {
		fragiles <- par
	}

	fragiles <- utils::head(fragiles, n)
	poids <- pmax(0.5, 3 - 2 * fragiles$reussite)
	names(poids) <- fragiles$cle

	correspondance <- match(fragiles$cle, bank$referentiel$full_theme_id)
	libelles <- as.character(bank$referentiel$sub_theme[correspondance])
	libelles[is.na(libelles)] <- as.character(fragiles$cle[is.na(libelles)])

	list(
		ok = TRUE,
		full_theme_ids = as.character(fragiles$cle),
		poids_themes = poids,
		apercu = tibble::tibble(
			full_theme_id = as.character(fragiles$cle),
			sub_theme = libelles,
			reussite = fragiles$reussite,
			n = fragiles$n
		)
	)
}

#' Fusionner les poids du profil et ceux du régleur
#'
#' Les poids de la séance en cours l'emportent sur une clé déjà vue. Les
#' sous-thèmes de révision non encore croisés gardent leur surpondération.
#'
#' @param base Poids du profil (`revision_ciblee`), ou `NULL`.
#' @param session Poids calculés sur les réponses de la séance, ou `NULL`.
#'
#' @return Un vecteur nommé, ou `NULL`.
#' @noRd
revision_fusionner_poids <- function(base, session) {
	if (is.null(base) && is.null(session)) {
		return(NULL)
	}

	if (is.null(session)) {
		return(base)
	}

	if (is.null(base)) {
		return(session)
	}

	fusion <- base
	fusion[names(session)] <- session
	fusion
}
