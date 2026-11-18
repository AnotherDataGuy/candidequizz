# Agent A-6 : réparateur.
#
# Agent LLM, appelé seulement s'il y a des rejets. Il tente jusqu'à deux passes
# de correction, puis se rabat sur la graine. Tout item réparé est relu une
# fois par le vérificateur. La boucle de reprise vit dans cet
# agent et non dans le moteur : la chaîne reste une liste linéaire d'étapes, ce
# qui la garde lisible et réordonnable.

#' Nombre maximal de passes de réparation
#'
#' Au-delà de deux passes, l'expérience des chaînes de ce genre est qu'un
#' modèle qui n'a pas corrigé son erreur en deux fois ne la corrigera pas à la
#' troisième : mieux vaut servir la graine et rendre la main.
#'
#' @return Un entier.
#' @noRd
reparateur_passes_max <- function() {
	2L
}

#' Mettre en forme un rejet pour l'invite de réparation
#'
#' @param rejet Le rejet.
#'
#' @return Une chaîne de caractères.
#' @noRd
prompt_bloc_rejet <- function(rejet) {
	item <- rejet$item
	consigne <- rejet$consigne
	type <- question_type(consigne$type)

	paste(
		c(
			glue::glue("### Item num\u00e9ro {item$numero} \u2014 \u00e0 r\u00e9parer"),
			glue::glue("Motif du rejet : {paste(rejet$motifs, collapse = ' ')}"),
			glue::glue("Difficult\u00e9 adaptative vis\u00e9e : {consigne$difficulte} sur 5."),
			type$consigne(consigne),
			"Version rejet\u00e9e :",
			glue::glue("\u00c9nonc\u00e9 : {item$enonce}"),
			jsonlite::toJSON(item$payload, auto_unbox = TRUE, pretty = TRUE),
			glue::glue("Explication : {item$explication}"),
			"",
			prompt_bloc_graine(consigne$graine)
		),
		collapse = "\n"
	)
}

#' Réparer un groupe de rejets de même type
#'
#' @param rejets Rejets du groupe.
#' @param ctx Contexte.
#'
#' @return `list(repares, restants, journal)`.
#' @noRd
reparateur_groupe <- function(rejets, ctx) {
	consigne_type <- rejets[[1L]]$consigne
	type <- question_type(consigne_type$type)

	appel <- llm_appel_structure(
		agent = "reparateur",
		role = "reparateur",
		systeme = prompt_rendre("reparateur_systeme", socle = prompt_socle()),
		invite = prompt_rendre(
			"reparateur_invite",
			nb_items = length(rejets),
			type_libelle = type$libelle,
			bloc_rejets = paste(vapply(rejets, prompt_bloc_rejet, character(1)), collapse = "\n\n")
		),
		schema = redacteur_schema(type$id, consigne_type$n_options)
	)

	if (!appel$ok) {
		return(list(repares = list(), restants = rejets, journal = appel$journal))
	}

	consignes <- lapply(rejets, function(r) r$consigne)
	apparies <- redacteur_apparier(appel$donnees$items %||% list(), consignes)

	repares <- list()
	restants <- list()

	for (i in seq_along(rejets)) {
		if (is.null(apparies[[i]])) {
			restants <- c(restants, list(rejets[[i]]))

			next
		}

		candidat <- item_depuis_llm(apparies[[i]], consignes[[i]], modele = appel$journal$modele)
		verdict <- conformite_valider(candidat, consignes[[i]])

		if (verdict$ok) {
			repares <- c(repares, list(candidat))
		} else {
			restants <- c(restants, list(conformite_rejet(
				item = candidat,
				consigne = consignes[[i]],
				motifs = verdict$motifs,
				origine = "reparation"
			)))
		}
	}

	list(repares = repares, restants = restants, journal = appel$journal)
}

#' Réparer les items rejetés d'un lot
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `items` complété et `rejets` vidé des cas traités.
#' @noRd
agent_reparateur <- function(ctx) {
	if (!length(ctx$rejets)) {
		return(ctx_detail(ctx, n_items = length(ctx$items), n_rejets = 0L, n_passes = 0L))
	}

	restants <- ctx$rejets
	passes <- 0L
	n_repares <- 0L
	repares <- list()

	while (length(restants) && passes < reparateur_passes_max() && !ctx$degrade) {
		passes <- passes + 1L
		types <- vapply(restants, function(r) r$consigne$type, character(1))
		groupes <- split(restants, factor(types, levels = unique(types)))

		restants <- list()

		for (groupe in groupes) {
			resultat <- reparateur_groupe(groupe, ctx)
			ctx <- ctx_journaliser(ctx, resultat$journal)
			repares <- c(repares, resultat$repares)
			n_repares <- n_repares + length(resultat$repares)
			restants <- c(restants, resultat$restants)
		}
	}

	# Le vérificateur passe avant le réparateur : un item réécrit n'a été relu
	# que par la conformité, qui ne juge que la forme. Tous les items réparés
	# repassent donc au vérificateur, en un seul appel, avant d'être servis.
	a_relire <- if (isTRUE(ctx$parametres$verifier_faits)) repares else list()
	n_relus <- length(a_relire)

	if (n_relus) {
		jugement <- if (!ctx$degrade) {
			verificateur_juger(a_relire)
		} else {
			NULL
		}

		if (!is.null(jugement)) {
			ctx <- ctx_journaliser(ctx, jugement$journal)
		}

		for (item in a_relire) {
			motif <- if (is.null(jugement) || !jugement$ok) {
				"relecture factuelle impossible apr\u00e8s r\u00e9paration"
			} else {
				jugement$motifs[[as.character(item$numero)]]
			}

			if (is.null(motif)) {
				if (as.integer(item$numero) %in% jugement$valides) {
					repares <- lapply(repares, function(i) {
						if (identical(i$item_id, item$item_id)) i$verifie <- TRUE
						i
					})
				}

				next
			}

			repares <- Filter(function(i) !identical(i$item_id, item$item_id), repares)
			n_repares <- n_repares - 1L
			restants <- c(restants, list(conformite_rejet(
				item = item,
				consigne = conformite_consigne(ctx$consignes, item$numero),
				motifs = motif,
				origine = "verification_factuelle"
			)))
		}
	}

	ctx$items <- c(ctx$items, repares)

	# Dernier recours : la graine elle-même, exacte par construction.
	abandons <- lapply(restants, function(rejet) {
		item_depuis_graine(
			rejet$consigne,
			motif = glue::glue(
				"non r\u00e9parable apr\u00e8s {passes} passe(s) : ",
				"{paste(rejet$motifs, collapse = ' ')}"
			)
		)
	})

	ctx$items <- c(ctx$items, abandons)

	numeros <- vapply(ctx$items, function(i) as.integer(i$numero), integer(1))
	ctx$items <- ctx$items[order(numeros)]
	ctx$rejets <- list()
	ctx$abandons <- restants

	ctx_detail(
		ctx,
		n_items = length(ctx$items),
		n_passes = passes,
		n_repares = n_repares,
		n_relus = n_relus,
		n_abandons = length(abandons)
	)
}
