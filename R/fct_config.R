#' Accès à la configuration applicative
#'
#' Lecture d'une valeur de `inst/golem-config.yml` par chemin de clés.
#' Toute la configuration de l'application passe par cette fonction : aucun
#' seuil, nom de modèle ou taille de lot ne doit être écrit en dur ailleurs.
#'
#' @param ... Chemin de la clé, du plus général au plus précis.
#'   `cq_config("llm", "timeout_s")` renvoie `llm$timeout_s`.
#'
#' @return La valeur de configuration demandée.
#' @noRd
#'
#' @examples
#' \dontrun{
#' cq_config("seance", "taille_lot")
#' cq_config("llm", "presets", "equilibre", "redacteur")
#' }
cq_config <- function(...) {
	chemin <- as.character(c(...))

	if (!length(chemin)) {
		rlang::abort(
			"cq_config() attend au moins une cl\u00e9 de configuration.",
			class = "candidequizz_config_appel_invalide"
		)
	}

	valeur <- get_golem_config(chemin[[1]])

	if (is.null(valeur)) {
		rlang::abort(
			c(
				glue::glue("Cl\u00e9 de configuration absente : `{chemin[[1]]}`."),
				i = "V\u00e9rifiez `inst/golem-config.yml`."
			),
			class = "candidequizz_config_cle_absente"
		)
	}

	for (i in seq_along(chemin)[-1]) {
		cle <- chemin[[i]]

		if (!is.list(valeur) || !cle %in% names(valeur)) {
			rlang::abort(
				c(
					glue::glue(
						"Cl\u00e9 de configuration absente : `{paste(chemin[seq_len(i)], collapse = '$')}`."
					),
					i = "V\u00e9rifiez `inst/golem-config.yml`."
				),
				class = "candidequizz_config_cle_absente"
			)
		}

		valeur <- valeur[[cle]]
	}

	valeur
}

#' Titre affiché de l'application
#'
#' @return Une chaîne de caractères.
#' @noRd
cq_app_title <- function() {
	cq_config("app_title")
}

#' Preset de modèles LLM actif
#'
#' L'ordre de priorité est la variable d'environnement `CANDIDEQUIZZ_PRESET`,
#' puis la valeur `llm$preset` du fichier de configuration. Cela permet de
#' basculer sur le preset économique pendant le développement sans modifier
#' le dépôt.
#'
#' @return Le nom du preset actif : `"economique"`, `"equilibre"` ou `"qualite"`.
#' @noRd
cq_llm_preset <- function() {
	preset <- Sys.getenv("CANDIDEQUIZZ_PRESET", "")

	if (!nzchar(preset)) {
		preset <- cq_config("llm", "preset")
	}

	connus <- names(cq_config("llm", "presets"))

	if (!preset %in% connus) {
		rlang::abort(
			c(
				glue::glue("Preset LLM inconnu : `{preset}`."),
				i = glue::glue("Presets disponibles : {paste(connus, collapse = ', ')}.")
			),
			class = "candidequizz_preset_inconnu"
		)
	}

	preset
}

#' Rôles d'agent susceptibles d'appeler un modèle
#'
#' @return Un vecteur de caractères.
#' @noRd
cq_llm_roles <- function() {
	c("redacteur", "verificateur", "reparateur", "synthetiseur")
}

#' Modèle affecté à un rôle d'agent
#'
#' @param role Un rôle parmi ceux de [cq_llm_roles()].
#' @param preset Nom du preset. Par défaut le preset actif.
#'
#' @return L'identifiant du modèle OpenAI à utiliser.
#' @noRd
cq_llm_model <- function(role, preset = cq_llm_preset()) {
	role <- match.arg(role, cq_llm_roles())
	cq_config("llm", "presets", preset, "roles", role)
}

#' Le modèle raisonne-t-il avant de répondre
#'
#' Les modèles de la famille gpt-5 produisent des tokens de raisonnement,
#' facturés en sortie et coûteux en latence. Ils acceptent un réglage d'effort
#' que les autres modèles rejettent, d'où ce test.
#'
#' @param modele Identifiant du modèle.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
cq_llm_raisonne <- function(modele) {
	modele %in% cq_config("llm", "modeles_raisonnement")
}

#' Effort de raisonnement du preset
#'
#' @param preset Nom du preset. Par défaut le preset actif.
#'
#' @return Une chaîne de caractères, par exemple `"minimal"`.
#' @noRd
cq_llm_effort <- function(preset = cq_llm_preset()) {
	cq_config("llm", "presets", preset, "effort_raisonnement")
}

#' Paramètres d'appel pour un rôle d'agent
#'
#' @param role Un rôle parmi ceux de [cq_llm_roles()].
#' @param preset Nom du preset. Par défaut le preset actif.
#'
#' @return Un objet `ellmer::params()`, avec l'effort de raisonnement
#'   uniquement si le modèle du rôle le prend en charge.
#' @noRd
cq_llm_params <- function(role, preset = cq_llm_preset()) {
	modele <- cq_llm_model(role, preset = preset)

	if (cq_llm_raisonne(modele)) {
		return(ellmer::params(reasoning_effort = cq_llm_effort(preset)))
	}

	ellmer::params()
}

#' Tarif d'un modèle
#'
#' @param modele Identifiant du modèle.
#'
#' @return Une liste `entree` / `sortie`, en dollars par million de tokens.
#'   `NULL` si le modèle n'est pas tarifé dans la configuration.
#' @noRd
cq_llm_prix <- function(modele) {
	prix <- cq_config("llm", "prix_usd_par_million")
	prix[[modele]]
}

#' Coût d'un appel LLM
#'
#' @param modele Identifiant du modèle.
#' @param tokens_entree,tokens_sortie Nombres de tokens consommés.
#'
#' @return Le coût en dollars, ou `NA_real_` si le modèle n'est pas tarifé.
#' @noRd
cq_llm_cout <- function(modele, tokens_entree, tokens_sortie) {
	prix <- cq_llm_prix(modele)

	if (is.null(prix)) {
		return(NA_real_)
	}

	(tokens_entree * prix$entree + tokens_sortie * prix$sortie) / 1e6
}
