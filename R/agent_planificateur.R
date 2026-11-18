# Agent A-1 : planificateur de séance.
#
# Agent déterministe, appelé une seule fois par séance. Il traduit les
# paramètres saisis en un plan : découpage en lots, type et nombre d'options de
# chaque question, état initial du régleur adaptatif.
#
# Ce travail ne demande aucun modèle : c'est de l'arithmétique et de la
# répartition. Le confier au LLM coûterait un appel par séance pour un résultat
# moins prévisible et non testable.

#' Découper une séance en lots
#'
#' Le premier lot est plus court, pour que la séance démarre vite. Les suivants
#' sont plus longs, ce qui amortit le coût fixe d'un appel au modèle. Un reliquat
#' trop petit est absorbé par le lot précédent plutôt que servi seul.
#'
#' @param n Nombre total de questions.
#' @param premier Taille du premier lot.
#' @param courant Taille des lots suivants.
#' @param minimum Taille en dessous de laquelle un reliquat est absorbé.
#'
#' @return Un vecteur d'entiers dont la somme vaut `n`.
#' @noRd
plan_tailles_lots <- function(n,
															premier = cq_config("seance", "taille_premier_lot"),
															courant = cq_config("seance", "taille_lot"),
															minimum = cq_config("seance", "taille_lot_min")) {
	n <- as.integer(n)
	premier <- as.integer(premier)
	courant <- as.integer(courant)
	minimum <- as.integer(minimum)

	if (n <= premier) {
		return(n)
	}

	tailles <- premier
	reste <- n - premier

	while (reste > 0L) {
		if (reste < minimum) {
			tailles[[length(tailles)]] <- tailles[[length(tailles)]] + reste

			break
		}

		prise <- min(courant, reste)
		tailles <- c(tailles, prise)
		reste <- reste - prise
	}

	as.integer(tailles)
}

#' Répartir les types de questions sur une séance
#'
#' Répartition par rotation puis mélange : la rotation garantit que chaque type
#' demandé apparaît en proportion égale, y compris sur une séance courte où un
#' tirage aléatoire pourrait en oublier un. Le mélange évite que les types
#' arrivent toujours dans le même ordre.
#'
#' @param n Nombre de questions.
#' @param types Types demandés.
#'
#' @return Un vecteur de caractères de longueur `n`.
#' @noRd
plan_repartir_types <- function(n, types) {
	n <- as.integer(n)

	if (length(types) == 1L) {
		return(rep(types, n))
	}

	rotation <- rep(types, length.out = n)

	rotation[sample.int(n)]
}

#' Planifier une séance
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `plan` renseigné.
#' @noRd
agent_planificateur <- function(ctx) {
	parametres <- ctx$parametres
	tailles <- plan_tailles_lots(parametres$nb_questions)

	fins <- cumsum(tailles)
	debuts <- fins - tailles + 1L

	types <- plan_repartir_types(parametres$nb_questions, parametres$types)

	n_options <- vapply(
		types,
		function(type) parametres_n_options(type, parametres$n_options),
		integer(1),
		USE.NAMES = FALSE
	)

	ctx$plan <- list(
		nb_questions = parametres$nb_questions,
		tailles_lots = tailles,
		n_lots = length(tailles),
		indices_lots = lapply(seq_along(tailles), function(i) seq(debuts[[i]], fins[[i]])),
		types = types,
		n_options = n_options,
		adaptatif_actif = parametres_adaptatif_actif(parametres),
		etat_adaptatif = adapt_etat_initial()
	)

	ctx_detail(ctx, n_lots = length(tailles), tailles = paste(tailles, collapse = "+"))
}
