# Agent A-7 : régleur adaptatif.
#
# Agent déterministe. Il referme la boucle entre deux lots : les réponses pas
# encore intégrées avancent l'état, puis une directive dit au sélecteur comment
# tirer le lot suivant (difficulté, poids thématiques, types). Aucun appel
# modèle : une règle arithmétique se teste et s'explique.

#' Clé d'une réponse pour le suivi de l'intégration
#'
#' @param reponse Une réponse.
#' @param rang Rang de la réponse dans la liste, en dernier recours.
#'
#' @return Une chaîne de caractères.
#' @noRd
regleur_cle_reponse <- function(reponse, rang) {
	id <- as.character(reponse$item_id %||% "")

	if (length(id) == 1L && !is.na(id) && nzchar(id)) {
		return(id)
	}

	paste0("lot", reponse$lot %||% "?", "#", rang)
}

#' État adaptatif à appliquer au lot en cours
#'
#' L'état est relu dans le plan, puis avancé de toutes les réponses de la
#' séance qu'il n'a pas encore vues. Le lot suivant se prépare pendant que les
#' dernières questions du lot courant sont encore à venir : filtrer sur « le
#' lot précédent » perdait donc ces réponses-là pour de bon. Le suivi se fait
#' par identifiant d'item, ce qui rend aussi l'intégration idempotente : une
#' génération relancée après un échec réseau n'applique pas deux fois les
#' mêmes réponses.
#'
#' @param ctx Contexte de génération.
#'
#' @return L'état adaptatif.
#' @noRd
regleur_etat_adaptatif <- function(ctx) {
	etat <- ctx$plan$etat_adaptatif

	if (ctx$lot <= 1L) {
		return(etat)
	}

	# La difficulté avance uniquement avec les réponses de la séance en
	# cours. Les réponses de séances antérieures servent au ciblage thématique.
	reponses_session <- ctx$historique$reponses_session %||%
		ctx$historique$reponses %||%
		list()
	cles <- vapply(
		seq_along(reponses_session),
		function(i) regleur_cle_reponse(reponses_session[[i]], i),
		character(1)
	)
	integrees <- etat$reponses_integrees

	# Séance enregistrée avant le suivi par item : seul le numéro du dernier
	# lot intégré est connu.
	if (is.null(integrees) && as.integer(etat$dernier_lot_integre %||% 0L) > 0L) {
		lots <- vapply(
			reponses_session,
			function(r) as.integer(r$lot %||% NA_integer_)[[1L]],
			integer(1)
		)
		integrees <- cles[!is.na(lots) & lots <= as.integer(etat$dernier_lot_integre)]
	}

	nouvelles <- !cles %in% (integrees %||% character(0))

	if (!any(nouvelles)) {
		return(etat)
	}

	reponses <- reponses_session[nouvelles]
	rangs <- vapply(
		reponses,
		function(r) suppressWarnings(as.integer(r$indice_seance %||% NA_integer_))[[1L]],
		integer(1)
	)

	if (!anyNA(rangs)) {
		reponses <- reponses[order(rangs)]
	}

	etat <- adapt_maj(etat, reponses)
	etat$reponses_integrees <- c(integrees %||% character(0), cles[nouvelles])
	etat$dernier_lot_integre <- max(
		as.integer(etat$dernier_lot_integre %||% 0L),
		suppressWarnings(max(vapply(reponses, function(r) as.integer(r$lot %||% 0L)[[1L]], integer(1))))
	)
	etat$historique_difficulte <- c(etat$historique_difficulte, etat$difficulte)

	etat
}

#' Alias conservé : l'état était calculé dans le sélecteur avant le Lot 5
#' @noRd
selecteur_etat_adaptatif <- regleur_etat_adaptatif

#' Régler le lot à venir d'après les réponses déjà données
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `plan` enrichi de l'état et de la directive.
#' @noRd
agent_regleur_adaptatif <- function(ctx) {
	# Un examen blanc doit rester comparable du début à la fin : les réponses
	# déjà données ne changent ni les thèmes, ni les types, ni la difficulté des
	# lots suivants. Le découpage en lots reste un détail de chargement.
	if (!parametres_adaptatif_actif(ctx$parametres)) {
		indices <- ctx$plan$indices_lots[[ctx$lot]]
		ctx$plan$adaptatif_actif <- FALSE
		ctx$plan$directive <- list(
			lot = ctx$lot,
			adaptatif = FALSE,
			difficulte = ctx$plan$etat_adaptatif$difficulte,
			confiance_restantes = 0L,
			poids_themes = NULL,
			types = ctx$plan$types[indices]
		)

		return(ctx_detail(
			ctx,
			difficulte = ctx$plan$etat_adaptatif$difficulte,
			adaptatif = FALSE,
			n_reponses = 0L
		))
	}

	etat <- regleur_etat_adaptatif(ctx)
	ctx$plan$etat_adaptatif <- etat

	indices <- ctx$plan$indices_lots[[ctx$lot]]
	types <- adapt_ajuster_types(
		types = ctx$plan$types[indices],
		etat = etat,
		disponibles = ctx$parametres$types
	)

	if (!identical(types, ctx$plan$types[indices])) {
		ctx$plan$types[indices] <- types
		ctx$plan$n_options[indices] <- vapply(
			types,
			function(type) parametres_n_options(type, ctx$parametres$n_options),
			integer(1),
			USE.NAMES = FALSE
		)
	}

	poids <- revision_fusionner_poids(
		ctx$parametres$poids_themes,
		adapt_poids_themes(ctx$historique$reponses %||% list())
	)

	ctx$plan$directive <- list(
		lot = ctx$lot,
		adaptatif = TRUE,
		difficulte = etat$difficulte,
		confiance_restantes = etat$confiance_restantes,
		poids_themes = poids,
		types = types
	)

	ctx_detail(
		ctx,
		difficulte = etat$difficulte,
		confiance = etat$confiance_restantes,
		n_reponses = length(ctx$historique$reponses %||% list())
	)
}
