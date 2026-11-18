# Contexte et moteur de chaîne d'agents.
#
# Une chaîne est une simple liste ordonnée d'identifiants d'agents. Réordonner,
# retirer ou insérer une étape ne demande aucune modification du moteur ni des
# agents : c'est ce qui permet, par exemple, de désactiver la vérification
# factuelle sur les séances courtes sans toucher au code de génération.

#' Créer un contexte de génération
#'
#' @param parametres Paramètres de séance validés.
#' @param bank Banque de graines.
#' @param lot Numéro du lot en cours.
#' @param exclure_seed_ids Graines déjà servies dans la séance, à éviter.
#' @param historique Résumé des réponses passées, pour l'adaptatif.
#' @param cache Paramètres du cache de questions ([cache_parametres()]), ou
#'   `NULL` pour tout rédiger.
#'
#' @return Un contexte, simple liste.
#' @noRd
pipeline_ctx <- function(parametres,
												 bank = bank_load(),
												 lot = 1L,
												 exclure_seed_ids = character(0),
												 historique = NULL,
												 cache = NULL) {
	list(
		parametres = parametres,
		bank = bank,
		lot = as.integer(lot),
		exclure_seed_ids = as.character(exclure_seed_ids),
		historique = historique,
		cache = cache,
		plan = NULL,
		consignes = NULL,
		items = NULL,
		rejets = NULL,
		journal_llm = list(),
		trace = list(),
		degrade = generation_degradee(parametres),
		coupure_llm = NULL
	)
}

#' Le lot doit-il se passer du modèle
#'
#' Sans IA, ou après une coupure, sauf si l'utilisateur relance explicitement
#' le lot. Une clé absente dégrade aussi une séance prévue avec IA.
#'
#' @param parametres Paramètres de séance.
#'
#' @return `TRUE` si les agents LLM doivent se replier.
#' @noRd
generation_degradee <- function(parametres) {
	if (identical(parametres$generation, "ia_off")) {
		return(TRUE)
	}

	if (identical(parametres$generation_etat, "ia_coupure") && !isTRUE(parametres$generation_retenter)) {
		return(TRUE)
	}

	!llm_cle_disponible()
}

#' Chaîne exécutée une fois par séance
#'
#' @return Un vecteur d'identifiants d'agents.
#' @noRd
pipeline_seance <- function() {
	c("planificateur")
}

#' Chaîne exécutée pour chaque lot de questions
#'
#' @return Un vecteur d'identifiants d'agents.
#' @noRd
pipeline_lot <- function() {
	c(
		"regleur_adaptatif",
		"selecteur_graines",
		"cache_questions",
		"redacteur",
		"conformite",
		"verificateur_factuel",
		"reparateur"
	)
}

#' Journaliser un appel LLM dans le contexte
#'
#' Coupe-circuit : un appel qui échoue après toutes ses reprises fait passer
#' le reste du lot en mode dégradé. Sans cela, une panne réseau se paierait
#' une fois par groupe de rédaction, puis au vérificateur, puis à chaque passe
#' du réparateur, avant de finir de toute façon sur les graines.
#'
#' @param ctx Le contexte.
#' @param journal Une ligne de journal, ou `NULL`.
#'
#' @return Le contexte enrichi.
#' @noRd
ctx_journaliser <- function(ctx, journal) {
	if (is.null(journal)) {
		return(ctx)
	}

	ctx$journal_llm <- c(ctx$journal_llm, list(journal))

	if (identical(journal$statut, "echec") && !isTRUE(ctx$degrade)) {
		ctx$degrade <- TRUE
		ctx$coupure_llm <- list(agent = journal$agent, message = journal$message)
	}

	ctx
}

#' Consigner une étape dans la trace du contexte
#'
#' @param ctx Le contexte.
#' @param agent Identifiant de l'agent.
#' @param details Champs à consigner.
#'
#' @return Le contexte enrichi.
#' @noRd
ctx_tracer <- function(ctx, agent, details = list()) {
	ctx$trace <- c(ctx$trace, list(c(list(agent = agent, lot = ctx$lot), details)))

	ctx
}

#' Déposer des détails destinés à la trace
#'
#' Un agent n'écrit pas directement dans la trace : il dépose des détails que
#' le moteur joint à l'entrée qu'il consigne lui-même, avec la durée mesurée.
#' La trace compte ainsi exactement une ligne par agent exécuté.
#'
#' @param ctx Le contexte.
#' @param ... Champs à consigner.
#'
#' @return Le contexte enrichi.
#' @noRd
ctx_detail <- function(ctx, ...) {
	ctx$details_trace <- c(ctx$details_trace %||% list(), list(...))

	ctx
}

#' Exécuter un agent sur un contexte
#'
#' Vérifie le contrat d'entrée, exécute, mesure la durée, puis vérifie le
#' contrat de sortie.
#'
#' @param id Identifiant de l'agent.
#' @param ctx Le contexte.
#'
#' @return Le contexte modifié.
#' @noRd
run_agent <- function(id, ctx) {
	a <- agent(id)

	manquants <- a$requiert[vapply(a$requiert, function(champ) is.null(ctx[[champ]]), logical(1))]

	if (length(manquants)) {
		rlang::abort(
			c(
				glue::glue("Contrat d'entr\u00e9e non satisfait pour l'agent `{id}`."),
				x = glue::glue("Champs absents du contexte : {paste(manquants, collapse = ', ')}."),
				i = "V\u00e9rifiez l'ordre des agents dans la cha\u00eene."
			),
			class = "candidequizz_pipeline_contrat_non_satisfait"
		)
	}

	debut <- Sys.time()
	ctx$details_trace <- NULL
	ctx <- a$fn(ctx)
	duree <- as.numeric(difftime(Sys.time(), debut, units = "secs"))

	if (!is.list(ctx)) {
		rlang::abort(
			glue::glue("L'agent `{id}` n'a pas renvoy\u00e9 de contexte."),
			class = "candidequizz_pipeline_sortie_invalide"
		)
	}

	non_produits <- a$produit[vapply(a$produit, function(champ) is.null(ctx[[champ]]), logical(1))]

	if (length(non_produits)) {
		rlang::abort(
			c(
				glue::glue("Contrat de sortie non satisfait par l'agent `{id}`."),
				x = glue::glue("Champs non renseign\u00e9s : {paste(non_produits, collapse = ', ')}.")
			),
			class = "candidequizz_pipeline_sortie_invalide"
		)
	}

	details <- ctx$details_trace %||% list()
	ctx$details_trace <- NULL

	ctx_tracer(ctx, id, c(list(duree_s = duree, nature = a$nature), details))
}

#' Exécuter une chaîne d'agents
#'
#' @param ctx Le contexte initial.
#' @param agents Identifiants des agents, dans l'ordre.
#'
#' @return Le contexte final.
#' @noRd
run_pipeline <- function(ctx, agents = pipeline_lot()) {
	for (id in agents) {
		ctx <- run_agent(id, ctx)
	}

	ctx
}

#' Trace d'exécution sous forme de tableau
#'
#' @param ctx Le contexte.
#'
#' @return Un tibble.
#' @noRd
pipeline_trace <- function(ctx) {
	if (!length(ctx$trace)) {
		return(tibble::tibble(agent = character(0), lot = integer(0), duree_s = numeric(0)))
	}

	champ <- function(nom, defaut) {
		vapply(ctx$trace, function(t) (t[[nom]] %||% defaut)[[1L]], defaut)
	}

	tibble::tibble(
		agent = champ("agent", NA_character_),
		lot = as.integer(champ("lot", NA_real_)),
		nature = champ("nature", NA_character_),
		duree_s = champ("duree_s", NA_real_),
		n_items = as.integer(champ("n_items", NA_real_)),
		n_rejets = as.integer(champ("n_rejets", NA_real_))
	)
}

#' Générer un lot de questions
#'
#' Point d'entrée unique de la chaîne de génération, utilisé par la console
#' comme par l'interface.
#'
#' @param parametres Paramètres de séance.
#' @param plan Plan de séance produit par l'agent planificateur.
#' @param lot Numéro du lot.
#' @param exclure_seed_ids Graines déjà servies.
#' @param historique Résumé des réponses passées.
#' @param cache Paramètres du cache de questions, ou `NULL`.
#' @param bank Banque de graines.
#'
#' @return Le contexte final, dont `items` porte le lot produit.
#' @noRd
generer_lot <- function(parametres,
												plan = NULL,
												lot = 1L,
												exclure_seed_ids = character(0),
												historique = NULL,
												cache = NULL,
												bank = bank_load()) {
	ctx <- pipeline_ctx(
		parametres = parametres,
		bank = bank,
		lot = lot,
		exclure_seed_ids = exclure_seed_ids,
		historique = historique,
		cache = cache
	)

	if (is.null(plan)) {
		ctx <- run_pipeline(ctx, pipeline_seance())
	} else {
		ctx$plan <- plan
	}

	run_pipeline(ctx, pipeline_lot())
}
