# Agent A-5 : vérificateur factuel.
#
# Agent LLM, un seul appel par lot, sur un modèle volontairement modeste : la
# tâche est un jugement court sur un contenu déjà rédigé, pas une rédaction.
# Il n'examine que les items déjà conformes, puisqu'un item mal formé sera de
# toute façon réécrit.

#' Schéma de sortie du vérificateur
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
verificateur_schema <- function() {
	ellmer::type_object(
		verdicts = ellmer::type_array(
			ellmer::type_object(
				numero = ellmer::type_integer("Le numero de l'item juge."),
				valide = ellmer::type_boolean(
					"Vrai si l'item est exact et dans le perimetre, faux sinon."
				),
				motif = ellmer::type_string(
					"En cas de rejet, la correction attendue en une phrase. Sinon, chaine vide."
				)
			),
			"Un verdict par item soumis."
		)
	)
}

#' Juger un ensemble d'items en un appel
#'
#' Cœur du vérificateur, partagé avec le réparateur qui fait relire les items
#' qu'il a corrigés.
#'
#' @param items Items à juger, hors items de repli.
#'
#' @return `list(ok, journal, motifs, valides, n_sans_verdict)` : `motifs` est
#'   une liste nommée par numéro des seuls items rejetés, `valides` les numéros
#'   explicitement jugés exacts.
#' @noRd
verificateur_juger <- function(items) {
	appel <- llm_appel_structure(
		agent = "verificateur_factuel",
		role = "verificateur",
		systeme = prompt_rendre("verificateur_systeme", socle = prompt_socle()),
		invite = prompt_rendre(
			"verificateur_invite",
			nb_items = length(items),
			bloc_items = paste(vapply(items, prompt_bloc_item, character(1)), collapse = "\n\n")
		),
		schema = verificateur_schema()
	)

	if (!appel$ok) {
		return(list(
			ok = FALSE,
			journal = appel$journal,
			motifs = list(),
			valides = integer(0),
			n_sans_verdict = length(items)
		))
	}

	verdicts <- appel$donnees$verdicts %||% list()
	motifs <- list()

	for (verdict in verdicts) {
		numero <- suppressWarnings(as.integer(verdict$numero %||% NA_integer_))[[1L]]

		if (is.na(numero) || isTRUE(verdict$valide)) {
			next
		}

		motif <- as.character(verdict$motif %||% "")
		motifs[[as.character(numero)]] <- if (nzchar(motif)) {
			motif
		} else {
			"Item jug\u00e9 inexact, sans motif pr\u00e9cis\u00e9."
		}
	}

	# Un item soumis sans verdict en retour relève d'une réponse incomplète du
	# modèle, pas d'un doute sur l'item : il est conservé, et compté.
	numeros_soumis <- vapply(items, function(i) as.integer(i$numero), integer(1))
	numeros_juges <- vapply(
		verdicts,
		function(v) suppressWarnings(as.integer(v$numero %||% NA_integer_))[[1L]],
		integer(1)
	)

	list(
		ok = TRUE,
		journal = appel$journal,
		motifs = motifs,
		valides = setdiff(intersect(numeros_soumis, numeros_juges), as.integer(names(motifs))),
		n_sans_verdict = length(setdiff(numeros_soumis, numeros_juges))
	)
}

#' Remplacer par leur graine les items rédigés que personne n'a relus
#'
#' Quand le coupe-circuit a sauté, le vérificateur ne peut plus être appelé.
#' Plutôt que de servir sans relecture des items écrits par le modèle, on
#' sert leur graine, exacte par construction. Aucun appel, aucune attente.
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, avec le nombre d'items remplacés dans `n_remplaces`.
#' @noRd
verificateur_remplacer_non_relus <- function(ctx) {
	n <- 0L

	ctx$items <- lapply(ctx$items, function(item) {
		if (!identical(item$source, "llm")) {
			return(item)
		}

		n <<- n + 1L
		item_depuis_graine(
			conformite_consigne(ctx$consignes, item$numero),
			motif = "mod\u00e8le indisponible : relecture factuelle impossible"
		)
	})

	ctx$n_remplaces <- n

	ctx
}

#' Vérifier l'exactitude factuelle des items d'un lot
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, les items jugés inexacts étant déplacés vers `rejets`.
#' @noRd
agent_verificateur_factuel <- function(ctx) {
	if (!is.null(ctx$coupure_llm) && isTRUE(ctx$parametres$verifier_faits)) {
		ctx <- verificateur_remplacer_non_relus(ctx)

		return(ctx_detail(
			ctx,
			n_items = length(ctx$items),
			n_rejets = length(ctx$rejets),
			n_remplaces = ctx$n_remplaces,
			verification_indisponible = TRUE
		))
	}

	if (ctx$degrade || !isTRUE(ctx$parametres$verifier_faits) || !length(ctx$items)) {
		return(ctx_detail(
			ctx,
			n_items = length(ctx$items),
			n_rejets = length(ctx$rejets),
			ignore = TRUE
		))
	}

	# Les items de repli reprennent mot pour mot une graine déjà relue, et ceux
	# du cache ont déjà passé ce contrôle : seuls les items rédigés à l'instant
	# sont soumis.
	a_verifier <- ctx$items[
		vapply(ctx$items, function(i) identical(i$source, "llm"), logical(1))
	]

	if (!length(a_verifier)) {
		return(ctx_detail(ctx, n_items = length(ctx$items), n_rejets = length(ctx$rejets)))
	}

	jugement <- verificateur_juger(a_verifier)
	ctx <- ctx_journaliser(ctx, jugement$journal)

	if (!jugement$ok) {
		# Un vérificateur indisponible ne bloque pas la séance, mais ne laisse
		# pas non plus passer du contenu non relu : les graines prennent le relais.
		ctx <- verificateur_remplacer_non_relus(ctx)

		return(ctx_detail(
			ctx,
			n_items = length(ctx$items),
			n_rejets = length(ctx$rejets),
			n_remplaces = ctx$n_remplaces,
			verification_indisponible = TRUE
		))
	}

	conserves <- list()

	for (item in ctx$items) {
		redige <- identical(item$source, "llm")
		motif <- if (redige) jugement$motifs[[as.character(item$numero)]] else NULL

		if (is.null(motif)) {
			if (redige && as.integer(item$numero) %in% jugement$valides) {
				item$verifie <- TRUE
			}

			conserves <- c(conserves, list(item))

			next
		}

		ctx$rejets <- c(ctx$rejets, list(conformite_rejet(
			item = item,
			consigne = conformite_consigne(ctx$consignes, item$numero),
			motifs = motif,
			origine = "verification_factuelle"
		)))
	}

	ctx$items <- conserves

	ctx_detail(
		ctx,
		n_items = length(ctx$items),
		n_rejets = length(ctx$rejets),
		n_sans_verdict = jugement$n_sans_verdict
	)
}
