# Agent A-2b : cache de questions vérifiées.
#
# Agent déterministe, placé entre le sélecteur et le rédacteur. Pour chaque
# consigne, il cherche une question déjà écrite et relue sur la même graine ;
# le rédacteur n'écrit que ce qui manque. Une séance coûte ainsi de moins en
# moins d'appels à mesure que l'application sert, et une question en cache
# est prête sans attendre. Sans clé d'API, le cache sert encore des questions
# reformulées là où le mode dégradé n'aurait que les graines.

#' Servir depuis le cache ce qui peut l'être
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `items` renseigné des questions trouvées (liste
#'   éventuellement vide).
#' @noRd
agent_cache_questions <- function(ctx) {
	ctx$items <- list()

	if (is.null(ctx$cache) || !cache_actif()) {
		return(ctx_detail(ctx, n_items = 0L, n_depuis_cache = 0L, ignore = TRUE))
	}

	entrees <- cache_lire(ctx$cache$chemin)
	exclure <- as.character(ctx$cache$exclure_ids %||% character(0))

	for (consigne in ctx$consignes) {
		entree <- cache_choisir(entrees, consigne, exclure_ids = exclure)

		if (is.null(entree)) {
			next
		}

		ctx$items <- c(ctx$items, list(item_depuis_cache(entree, consigne)))
		exclure <- c(exclure, entree$cache_id)
	}

	ctx_detail(
		ctx,
		n_items = length(ctx$items),
		n_depuis_cache = length(ctx$items),
		n_entrees = length(entrees)
	)
}
