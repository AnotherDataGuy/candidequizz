# Paramètres de séance.
#
# Un seul constructeur valide les paramètres, quelle qu'en soit l'origine :
# formulaire Shiny, appel en console, ou séance rechargée. L'interface du Lot 3
# n'aura donc pas à dupliquer ces contrôles, et une séance rechargée depuis un
# fichier ne peut pas réintroduire des paramètres devenus invalides.

#' Profils de séance (intention pédagogique)
#'
#' Distincts des modes de questions [cq_modes()] : un profil impose un
#' périmètre (révision, examen), pas un type d'item.
#'
#' @return Un vecteur de caractères.
#' @noRd
cq_profils <- function() {
	c("libre", "revision_ciblee", "examen_blanc")
}

#' L'adaptation est-elle autorisée pour ce profil ?
#'
#' L'examen blanc mesure une performance dans un cadre stable. Ses questions
#' suivantes ne doivent donc jamais dépendre des réponses précédentes.
#'
#' @param parametres Paramètres d'une séance.
#'
#' @return `TRUE` pour les profils d'apprentissage, `FALSE` pour l'examen.
#' @noRd
parametres_adaptatif_actif <- function(parametres) {
	!identical(as.character(parametres$profil %||% "libre"), "examen_blanc")
}

#' Construire et valider des paramètres de séance
#'
#' @param nb_questions Nombre de questions, parmi les valeurs admises en
#'   configuration.
#' @param mode Mode de séance, parmi [cq_modes()].
#' @param n_options Nombre d'options pour les QCM : un entier de 2 à 4, ou
#'   `"varie"` pour laisser varier d'une question à l'autre.
#' @param types Types de questions autorisés. `NULL` pour tous ceux du mode. En
#'   mode `qcm_classique`, la valeur est imposée.
#' @param full_theme_ids Sous-thèmes à couvrir. `NULL` pour tout le référentiel.
#' @param verifier_faits Activer l'agent de vérification factuelle.
#' @param profil `"libre"`, `"revision_ciblee"` ou `"examen_blanc"`.
#' @param poids_themes Poids nommés par `full_theme_id`, ou `NULL`.
#' @param retour_differe Reporter les corrections en fin de séance.
#' @param avis_pendant Demander aussi la note de difficulté pendant la
#'   séance (avant la correction), en plus de l'écran « Vos avis » final.
#' @param chronometre `"aucun"` ou `"global"`.
#' @param limite_s Durée maximale en secondes si le chronomètre est global.
#' @param generation `"auto"`, `"ia_on"` ou `"ia_off"`. `auto` suit la clé API.
#' @param langue `"courant"`, `"simple"` ou `"administratif"`.
#' @param libelle Libellé libre, ou `NULL` pour le libellé automatique.
#' @param bank Banque de graines, pour contrôler les sous-thèmes demandés.
#'
#' @return Une liste de paramètres validés.
#' @noRd
parametres_seance <- function(nb_questions = 10L,
															mode = "qcm_classique",
															n_options = 4L,
															types = NULL,
															full_theme_ids = NULL,
															verifier_faits = TRUE,
															profil = "libre",
															poids_themes = NULL,
															retour_differe = FALSE,
															avis_pendant = FALSE,
															chronometre = "aucun",
															limite_s = NA_real_,
															generation = "auto",
															langue = "courant",
															libelle = NULL,
															bank = bank_load()) {
	if (length(profil) != 1L || !profil %in% cq_profils()) {
		rlang::abort(
			c(
				"Profil de s\u00e9ance inconnu.",
				i = glue::glue("Profils connus : {paste(cq_profils(), collapse = ', ')}.")
			),
			class = "candidequizz_parametres_invalides"
		)
	}

	if (identical(profil, "examen_blanc")) {
		nb_questions <- as.integer(cq_config("seance", "examen_nb_questions"))
		mode <- "qcm_classique"
		n_options <- 4L
		types <- NULL
		full_theme_ids <- NULL
		poids_themes <- NULL
		retour_differe <- TRUE
		chronometre <- "global"
		limite_s <- as.numeric(cq_config("seance", "examen_limite_s"))
	}

	admis <- as.integer(cq_config("seance", "nb_questions_possibles"))
	nb_questions <- suppressWarnings(as.integer(nb_questions))

	if (length(nb_questions) != 1L || is.na(nb_questions) || !nb_questions %in% admis) {
		rlang::abort(
			c(
				"Nombre de questions non admis.",
				x = glue::glue("Re\u00e7u : {paste(nb_questions, collapse = ', ')}."),
				i = glue::glue("Valeurs admises : {paste(admis, collapse = ', ')}.")
			),
			class = "candidequizz_parametres_invalides"
		)
	}

	if (length(mode) != 1L || !mode %in% cq_modes()) {
		rlang::abort(
			c(
				"Mode de s\u00e9ance inconnu.",
				x = glue::glue("Re\u00e7u : {paste(mode, collapse = ', ')}."),
				i = glue::glue("Modes connus : {paste(cq_modes(), collapse = ', ')}.")
			),
			class = "candidequizz_parametres_invalides"
		)
	}

	disponibles <- question_type_ids(mode = mode)

	if (identical(mode, "qcm_classique")) {
		# Le mode classique est défini par son type : ne pas laisser l'appelant
		# le contredire évite une séance « classique » remplie de textes à trous.
		types <- "qcm_single"
	} else {
		types <- types %||% disponibles
		inconnus <- setdiff(types, disponibles)

		if (length(inconnus)) {
			rlang::abort(
				c(
					glue::glue("Type de question non disponible en mode `{mode}`."),
					x = glue::glue("Inconnus : {paste(inconnus, collapse = ', ')}."),
					i = glue::glue("Disponibles : {paste(disponibles, collapse = ', ')}.")
				),
				class = "candidequizz_parametres_invalides"
			)
		}
	}

	if (!length(types)) {
		rlang::abort(
			"Aucun type de question s\u00e9lectionn\u00e9.",
			class = "candidequizz_parametres_invalides"
		)
	}

	if (!identical(n_options, "varie")) {
		n_options <- suppressWarnings(as.integer(n_options))

		if (length(n_options) != 1L || is.na(n_options) || n_options < 2L || n_options > 4L) {
			rlang::abort(
				c(
					"Nombre d'options non admis.",
					i = "Attendu : un entier de 2 \u00e0 4, ou la cha\u00eene `varie`."
				),
				class = "candidequizz_parametres_invalides"
			)
		}
	}

	if (!is.null(full_theme_ids) && length(full_theme_ids)) {
		inconnus <- setdiff(full_theme_ids, bank$referentiel$full_theme_id)

		if (length(inconnus)) {
			rlang::abort(
				c(
					"Sous-th\u00e8me inconnu dans le r\u00e9f\u00e9rentiel.",
					x = glue::glue("Inconnus : {paste(inconnus, collapse = ', ')}.")
				),
				class = "candidequizz_parametres_invalides"
			)
		}

		full_theme_ids <- as.character(full_theme_ids)
	} else {
		full_theme_ids <- NULL
	}

	if (!is.null(poids_themes)) {
		if (is.null(names(poids_themes)) || any(!nzchar(names(poids_themes)))) {
			rlang::abort(
				"Les poids th\u00e9matiques doivent \u00eatre un vecteur nomm\u00e9.",
				class = "candidequizz_parametres_invalides"
			)
		}

		inconnus <- setdiff(names(poids_themes), bank$referentiel$full_theme_id)

		if (length(inconnus)) {
			rlang::abort(
				c(
					"Poids th\u00e9matique hors r\u00e9f\u00e9rentiel.",
					x = glue::glue("Inconnus : {paste(inconnus, collapse = ', ')}.")
				),
				class = "candidequizz_parametres_invalides"
			)
		}

		poids_themes <- as.numeric(poids_themes)
	}

	if (!chronometre %in% c("aucun", "global")) {
		rlang::abort(
			"Chronom\u00e8tre inconnu. Attendu : `aucun` ou `global`.",
			class = "candidequizz_parametres_invalides"
		)
	}

	if (identical(chronometre, "global")) {
		limite_s <- suppressWarnings(as.numeric(limite_s))

		if (length(limite_s) != 1L || !is.finite(limite_s) || limite_s <= 0) {
			rlang::abort(
				"Une limite de temps positive est requise pour le chronom\u00e8tre global.",
				class = "candidequizz_parametres_invalides"
			)
		}
	} else {
		limite_s <- NA_real_
	}

	if (!generation %in% c("auto", "ia_on", "ia_off")) {
		rlang::abort(
			"Mode de g\u00e9n\u00e9ration inconnu. Attendu : `auto`, `ia_on` ou `ia_off`.",
			class = "candidequizz_parametres_invalides"
		)
	}

	if (identical(generation, "auto") || (identical(generation, "ia_on") && !llm_cle_disponible())) {
		generation <- if (identical(generation, "auto") && llm_par_defaut()) "ia_on" else "ia_off"
	}

	if (!langue %in% c("courant", "simple", "administratif")) {
		rlang::abort(
			"Niveau de langue inconnu.",
			class = "candidequizz_parametres_invalides"
		)
	}

	libelle <- trimws(as.character(libelle %||% ""))

	if (!nzchar(libelle)) {
		libelle <- NULL
	} else if (nchar(libelle) > 80L) {
		libelle <- substr(libelle, 1L, 80L)
	}

	list(
		nb_questions = nb_questions,
		mode = mode,
		n_options = n_options,
		types = as.character(types),
		full_theme_ids = full_theme_ids,
		verifier_faits = isTRUE(verifier_faits) && !identical(generation, "ia_off"),
		profil = profil,
		adaptatif = !identical(profil, "examen_blanc"),
		poids_themes = poids_themes,
		retour_differe = isTRUE(retour_differe),
		avis_pendant = isTRUE(avis_pendant),
		chronometre = chronometre,
		limite_s = limite_s,
		generation = generation,
		langue = langue,
		libelle = libelle
	)
}

#' Nombre d'options, d'éléments ou de trous à viser pour un type donné
#'
#' Le paramètre saisi par l'utilisateur porte sur les options de QCM. Il ne se
#' transpose pas aux autres types : demander quatre options ne veut pas dire
#' quatre trous à remplir, et un texte de deux phrases troué quatre fois
#' devient illisible. Seuls les types qui se déclarent pilotés par ce paramètre
#' le reçoivent, les autres restent sur leur propre valeur.
#'
#' @param type Identifiant du type de question.
#' @param n_options Paramètre de séance : entier ou `"varie"`.
#'
#' @return Un entier, ou `NA_integer_` si la notion ne s'applique pas au type.
#' @noRd
parametres_n_options <- function(type, n_options) {
	spec <- question_type(type)
	bornes <- spec$n_options

	if (is.null(bornes)) {
		return(NA_integer_)
	}

	if (identical(n_options, "varie")) {
		return(as.integer(bornes[[sample.int(length(bornes), 1L)]]))
	}

	if (!isTRUE(spec$options_pilotees)) {
		return(as.integer(spec$n_options_defaut))
	}

	souhait <- as.integer(n_options)

	if (souhait %in% bornes) {
		return(souhait)
	}

	# Hors bornes du type : on retient la valeur admissible la plus proche du
	# souhait, ce qui préserve l'intention de l'utilisateur.
	as.integer(bornes[[which.min(abs(bornes - souhait))]])
}
