# Registre des types de questions.
#
# C'est la pièce centrale de l'extensibilité. Rien d'autre dans l'application ne
# connaît la liste des types : le paramétrage, le prompt du rédacteur, le
# contrôleur de conformité, la passation, la notation et les statistiques lisent
# tous ce registre.
#
# Ajouter un type de question = créer `R/qtype_<id>.R`, créer
# `inst/prompts/qtype_<id>.md`, et appeler `register_question_type()` depuis
# `.onLoad()`. Aucun `switch()` ni `if` à modifier ailleurs.

.qtype_registre <- new.env(parent = emptyenv())

#' Modes de séance connus
#'
#' @return Un vecteur de caractères.
#' @noRd
cq_modes <- function() {
	c("qcm_classique", "autres_types")
}

#' Enregistrer un type de question
#'
#' @param id Identifiant technique, par exemple `"qcm_single"`.
#' @param libelle Libellé affiché à l'utilisateur.
#' @param modes Modes de séance dans lesquels le type est proposé.
#' @param n_options Nombres admissibles d'options, d'éléments ou de trous selon
#'   le type, ou `NULL` si la notion ne s'applique pas.
#' @param n_options_defaut Valeur retenue quand le paramètre de séance ne
#'   pilote pas ce type. Par défaut, la valeur médiane des admissibles.
#' @param options_pilotees Le paramètre d'options saisi par l'utilisateur
#'   s'applique-t-il à ce type ? Vrai pour les QCM, où il désigne bien un
#'   nombre d'options. Faux pour les autres, où il désigne autre chose : quatre
#'   options de QCM ne veulent pas dire quatre trous à remplir.
#' @param schema Fonction `function(n_options)` renvoyant le schéma `{ellmer}`
#'   d'un item de ce type.
#' @param consigne Fonction `function(consigne)` renvoyant les consignes de
#'   rédaction propres au type, en texte, injectées dans le prompt.
#' @param valider Fonction `function(item, consigne)` renvoyant
#'   `list(ok, motifs)`.
#' @param noter Fonction `function(item, reponse)` renvoyant
#'   `list(score, is_correct, detail)`.
#' @param ui,server Fonctions de module Shiny. Peuvent rester `NULL` : le type
#'   existe alors côté génération, validation et notation, mais n'est pas encore
#'   jouable à l'écran. C'est ce qui permet de développer un type par étapes.
#' @param solution Fonction `function(item)` renvoyant le texte de la bonne
#'   réponse, pour le bilan. `NULL` si le type n'a pas encore de formulation.
#' @param temps_ref_s Temps de réponse de référence, en secondes, utilisé comme
#'   étalon dans les statistiques.
#'
#' @return Le type enregistré, invisiblement.
#' @noRd
register_question_type <- function(id,
																	 libelle,
																	 modes,
																	 schema,
																	 consigne,
																	 valider,
																	 noter,
													 n_options = NULL,
													 n_options_defaut = NULL,
													 options_pilotees = TRUE,
													 ui = NULL,
													 server = NULL,
													 solution = NULL,
													 temps_ref_s = 30) {
	if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
		rlang::abort(
			"L'identifiant d'un type de question doit \u00eatre une cha\u00eene non vide.",
			class = "candidequizz_qtype_contrat_invalide"
		)
	}

	modes_inconnus <- setdiff(modes, cq_modes())

	if (length(modes_inconnus)) {
		rlang::abort(
			c(
				glue::glue("Mode inconnu pour le type `{id}`."),
				x = glue::glue("Inconnus : {paste(modes_inconnus, collapse = ', ')}."),
				i = glue::glue("Modes connus : {paste(cq_modes(), collapse = ', ')}.")
			),
			class = "candidequizz_qtype_contrat_invalide"
		)
	}

	# Contrôle du contrat à l'enregistrement : une signature erronée doit se
	# voir au chargement du package, pas au milieu d'une séance.
	attendus <- list(
		schema = "n_options",
		consigne = "consigne",
		valider = c("item", "consigne"),
		noter = c("item", "reponse")
	)

	fonctions <- list(schema = schema, consigne = consigne, valider = valider, noter = noter)

	for (nom in names(attendus)) {
		fn <- fonctions[[nom]]

		if (!is.function(fn)) {
			rlang::abort(
				glue::glue("`{nom}` doit \u00eatre une fonction pour le type `{id}`."),
				class = "candidequizz_qtype_contrat_invalide"
			)
		}

		arguments <- names(formals(fn))
		manquants <- setdiff(attendus[[nom]], arguments)

		if (length(manquants)) {
			rlang::abort(
				c(
					glue::glue("Signature incorrecte de `{nom}` pour le type `{id}`."),
					x = glue::glue("Arguments manquants : {paste(manquants, collapse = ', ')}.")
				),
				class = "candidequizz_qtype_contrat_invalide"
			)
		}
	}

	if (!is.null(n_options) && is.null(n_options_defaut)) {
		n_options_defaut <- stats::median(n_options)
	}

	type <- list(
		id = id,
		libelle = libelle,
		modes = modes,
		n_options = n_options,
		n_options_defaut = if (is.null(n_options_defaut)) NULL else as.integer(n_options_defaut),
		options_pilotees = isTRUE(options_pilotees),
		schema = schema,
		consigne = consigne,
		valider = valider,
		noter = noter,
		ui = ui,
		server = server,
		solution = solution,
		temps_ref_s = temps_ref_s,
		jouable = !is.null(ui) && !is.null(server)
	)

	assign(id, type, envir = .qtype_registre)

	invisible(type)
}

#' Registre complet des types de questions
#'
#' @return Une liste nommée par identifiant de type.
#' @noRd
question_type_registry <- function() {
	ids <- sort(ls(envir = .qtype_registre))
	stats::setNames(lapply(ids, function(id) get(id, envir = .qtype_registre)), ids)
}

#' Identifiants des types de questions
#'
#' @param mode Restreindre aux types proposés dans ce mode. `NULL` pour tous.
#' @param jouable Ne garder que les types dotés d'une interface. `NULL` pour
#'   ne pas filtrer.
#'
#' @return Un vecteur de caractères.
#' @noRd
question_type_ids <- function(mode = NULL, jouable = NULL) {
	types <- question_type_registry()

	if (!is.null(mode)) {
		types <- types[vapply(types, function(t) mode %in% t$modes, logical(1))]
	}

	if (!is.null(jouable)) {
		types <- types[vapply(types, function(t) identical(t$jouable, jouable), logical(1))]
	}

	names(types)
}

#' Un type de question
#'
#' @param id Identifiant du type.
#'
#' @return La description du type.
#' @noRd
question_type <- function(id) {
	if (!exists(id, envir = .qtype_registre, inherits = FALSE)) {
		rlang::abort(
			c(
				glue::glue("Type de question inconnu : `{id}`."),
				i = glue::glue("Types enregistr\u00e9s : {paste(question_type_ids(), collapse = ', ')}.")
			),
			class = "candidequizz_qtype_inconnu"
		)
	}

	get(id, envir = .qtype_registre)
}

#' Texte de la bonne réponse d'un item
#'
#' Délégué au type : le bilan et les exports ne connaissent pas la forme du
#' `payload`. Sans fonction `solution`, on renvoie une chaîne vide plutôt que
#' de deviner.
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
question_type_solution <- function(item) {
	fn <- question_type(item$type)$solution

	if (!is.function(fn)) {
		return("")
	}

	as.character(fn(item)[[1L]] %||% "")
}

#' Libellés des types, pour l'interface de paramétrage
#'
#' @param mode Restreindre à un mode. `NULL` pour tous.
#'
#' @return Un vecteur de caractères nommé par libellé, valant l'identifiant,
#'   directement exploitable par les fonctions de saisie de Shiny.
#' @noRd
question_type_choix <- function(mode = NULL) {
	ids <- question_type_ids(mode = mode)
	libelles <- vapply(ids, function(id) question_type(id)$libelle, character(1))

	stats::setNames(ids, libelles)
}

#' Vider le registre
#'
#' Réservé aux tests, qui enregistrent des types factices.
#'
#' @return `NULL`, invisiblement.
#' @noRd
question_type_reset <- function() {
	rm(list = ls(envir = .qtype_registre), envir = .qtype_registre)
	invisible(NULL)
}
