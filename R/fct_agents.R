# Registre des agents.
#
# Un agent est une étape nommée de la chaîne de génération. Il déclare ce qu'il
# attend du contexte et ce qu'il y dépose, et se réduit à une fonction
# `function(ctx) ctx`. Le moteur vérifie les contrats avant exécution, de sorte
# qu'une chaîne mal ordonnée échoue immédiatement avec un message explicite,
# plutôt que par un `NULL` inattendu trois étapes plus loin.
#
# Ajouter un agent = créer `R/agent_<id>.R`, appeler `register_agent()` depuis
# `.onLoad()`, et l'insérer dans une chaîne. Aucun agent existant n'est touché.

.agent_registre <- new.env(parent = emptyenv())

#' Enregistrer un agent
#'
#' @param id Identifiant technique, par exemple `"redacteur"`.
#' @param libelle Libellé lisible, affiché dans les traces.
#' @param nature `"deterministe"` pour du code R pur, `"llm"` pour un agent qui
#'   appelle un modèle. Sert à distinguer dans les traces ce qui coûte de ce
#'   qui est gratuit.
#' @param fn Fonction `function(ctx)` renvoyant le contexte modifié.
#' @param requiert Champs du contexte qui doivent être présents et non `NULL`.
#' @param produit Champs du contexte que l'agent renseigne.
#' @param role Rôle de modèle utilisé, ou `NA` pour un agent déterministe.
#' @param description Rôle de l'agent, en une phrase.
#'
#' @return L'agent enregistré, invisiblement.
#' @noRd
register_agent <- function(id,
													 libelle,
													 nature,
													 fn,
													 requiert = character(0),
													 produit = character(0),
													 role = NA_character_,
													 description = "") {
	if (!nature %in% c("deterministe", "llm")) {
		rlang::abort(
			glue::glue("Nature d'agent inconnue pour `{id}` : `{nature}`."),
			class = "candidequizz_agent_contrat_invalide"
		)
	}

	if (!is.function(fn) || !"ctx" %in% names(formals(fn))) {
		rlang::abort(
			glue::glue("L'agent `{id}` doit \u00eatre une fonction acceptant `ctx`."),
			class = "candidequizz_agent_contrat_invalide"
		)
	}

	if (identical(nature, "llm") && !is.na(role) && !role %in% cq_llm_roles()) {
		rlang::abort(
			c(
				glue::glue("R\u00f4le de mod\u00e8le inconnu pour l'agent `{id}` : `{role}`."),
				i = glue::glue("R\u00f4les connus : {paste(cq_llm_roles(), collapse = ', ')}.")
			),
			class = "candidequizz_agent_contrat_invalide"
		)
	}

	agent <- list(
		id = id,
		libelle = libelle,
		nature = nature,
		fn = fn,
		requiert = as.character(requiert),
		produit = as.character(produit),
		role = as.character(role),
		description = description
	)

	assign(id, agent, envir = .agent_registre)

	invisible(agent)
}

#' Registre complet des agents
#'
#' @return Une liste nommée par identifiant d'agent.
#' @noRd
agent_registry <- function() {
	ids <- sort(ls(envir = .agent_registre))
	stats::setNames(lapply(ids, function(id) get(id, envir = .agent_registre)), ids)
}

#' Identifiants des agents enregistrés
#'
#' @return Un vecteur de caractères.
#' @noRd
agent_ids <- function() {
	sort(ls(envir = .agent_registre))
}

#' Un agent
#'
#' @param id Identifiant de l'agent.
#'
#' @return La description de l'agent.
#' @noRd
agent <- function(id) {
	if (!exists(id, envir = .agent_registre, inherits = FALSE)) {
		rlang::abort(
			c(
				glue::glue("Agent inconnu : `{id}`."),
				i = glue::glue("Agents enregistr\u00e9s : {paste(agent_ids(), collapse = ', ')}.")
			),
			class = "candidequizz_agent_inconnu"
		)
	}

	get(id, envir = .agent_registre)
}

#' Tableau de synthèse des agents
#'
#' Reflet exécutable de la chaîne d'agents : la
#' documentation est dérivée du registre, elle ne peut donc pas divorcer du
#' code.
#'
#' @return Un tibble.
#' @noRd
agent_synthese <- function() {
	agents <- agent_registry()

	if (!length(agents)) {
		return(tibble::tibble(
			id = character(0),
			libelle = character(0),
			nature = character(0),
			role = character(0),
			modele = character(0),
			description = character(0)
		))
	}

	champ <- function(nom) {
		vapply(agents, function(a) as.character(a[[nom]] %||% NA_character_)[[1L]], character(1))
	}

	roles <- champ("role")

	tibble::tibble(
		id = champ("id"),
		libelle = champ("libelle"),
		nature = champ("nature"),
		role = roles,
		modele = vapply(
			roles,
			function(r) if (is.na(r)) NA_character_ else cq_llm_model(r),
			character(1)
		),
		description = champ("description")
	)
}

#' Vider le registre des agents
#'
#' Réservé aux tests.
#'
#' @return `NULL`, invisiblement.
#' @noRd
agent_reset <- function() {
	rm(list = ls(envir = .agent_registre), envir = .agent_registre)
	invisible(NULL)
}
