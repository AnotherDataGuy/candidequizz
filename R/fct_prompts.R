# Chargement et rendu des gabarits de prompts.
#
# Les prompts vivent dans `inst/prompts/` et non dans le code R. Ajuster une
# consigne de rédaction ne demande donc ni modification de fonction ni
# rechargement du package en production, et l'historique Git des prompts se lit
# indépendamment de celui de la logique.
#
# Les substitutions utilisent `<<nom>>` et non les accolades de {glue} : les
# textes à trous emploient `{{1}}` comme marqueur, et les schémas JSON cités
# dans les prompts contiennent des accolades.

.prompt_cache <- new.env(parent = emptyenv())

#' Chemin d'un gabarit de prompt
#'
#' @param nom Nom du gabarit, sans extension.
#'
#' @return Le chemin du fichier.
#' @noRd
prompt_path <- function(nom) {
	chemin <- system.file("prompts", paste0(nom, ".md"), package = "candidequizz")

	if (!nzchar(chemin) || !file.exists(chemin)) {
		rlang::abort(
			c(
				glue::glue("Gabarit de prompt introuvable : `{nom}`."),
				i = "Les gabarits sont attendus dans `inst/prompts/`."
			),
			class = "candidequizz_prompt_absent"
		)
	}

	chemin
}

#' Lire un gabarit de prompt
#'
#' @param nom Nom du gabarit, sans extension.
#'
#' @return Le contenu du fichier, en une chaîne.
#' @noRd
prompt_lire <- function(nom) {
	if (!exists(nom, envir = .prompt_cache, inherits = FALSE)) {
		contenu <- paste(readLines(prompt_path(nom), encoding = "UTF-8"), collapse = "\n")
		assign(nom, contenu, envir = .prompt_cache)
	}

	get(nom, envir = .prompt_cache)
}

#' Rendre un gabarit de prompt
#'
#' @param nom Nom du gabarit, sans extension.
#' @param ... Valeurs de substitution nommées. Chaque `nom = valeur` remplace
#'   `<<nom>>` dans le gabarit.
#'
#' @return Le prompt rendu.
#' @noRd
prompt_rendre <- function(nom, ...) {
	gabarit <- prompt_lire(nom)
	valeurs <- list(...)

	for (cle in names(valeurs)) {
		gabarit <- gsub(
			paste0("<<", cle, ">>"),
			paste(as.character(valeurs[[cle]]), collapse = "\n"),
			gabarit,
			fixed = TRUE
		)
	}

	restants <- unlist(regmatches(gabarit, gregexpr("<<[a-z_]+>>", gabarit)))

	if (length(restants)) {
		rlang::abort(
			c(
				glue::glue("Substitutions manquantes dans le gabarit `{nom}`."),
				x = glue::glue("Non remplac\u00e9es : {paste(unique(restants), collapse = ', ')}.")
			),
			class = "candidequizz_prompt_incomplet"
		)
	}

	gabarit
}

#' Socle commun à tous les prompts système
#'
#' Rappelle le contexte de l'entretien de naturalisation et les invariants de
#' rédaction des questions. Centralisé ici pour n'exister qu'en un
#' seul exemplaire.
#'
#' @return Le socle rendu.
#' @noRd
prompt_socle <- function() {
	prompt_lire("socle")
}

#' Mettre en forme une graine pour un prompt
#'
#' Transmet au modèle la matière factuelle de la graine : objectif
#' d'apprentissage, question de référence, options avec leur statut, et erreurs
#' typiques documentées. Le modèle reste libre de reformuler et de changer de
#' type, mais pas d'inventer des faits que la graine ne porte pas.
#'
#' Les options de référence sont mélangées à chaque appel : le classeur place
#' souvent la bonne réponse en tête, et un ordre stable ancre le rédacteur.
#'
#' @param graine Une ligne du tableau des graines, issue de [bank_seeds()].
#'
#' @return Une chaîne de caractères.
#' @noRd
prompt_bloc_graine <- function(graine) {
	options <- graine$options[[1L]]
	erreurs <- graine$erreurs[[1L]]

	if (NROW(options) > 1L) {
		options <- options[sample.int(NROW(options)), , drop = FALSE]
	}

	lignes <- c(
		glue::glue("Graine {graine$seed_id} \u2014 {graine$theme} / {graine$sub_theme}"),
		glue::glue("Objectif d'apprentissage : {graine$objectif}"),
		if (!is.na(graine$difficulte_graine %||% NA_integer_)) {
			glue::glue(
				"Difficult\u00e9 intrins\u00e8que de la graine : {graine$difficulte_graine} sur 10 ",
				"(1 = accessible, 10 = exigeant). Calibrez l'\u00e9nonc\u00e9 et les distracteurs ",
				"sur ce niveau, sans le mentionner \u00e0 l'utilisateur."
			)
		},
		glue::glue("Question de r\u00e9f\u00e9rence : {graine$question_ref}"),
		if (nzchar(trimws(as.character(graine$explication %||% "")))) {
			glue::glue(
				"Explication p\u00e9dagogique de r\u00e9f\u00e9rence (montr\u00e9e \u00e0 l'utilisateur ",
				"apr\u00e8s sa r\u00e9ponse ; ne pas la recopier dans l'\u00e9nonc\u00e9) : ",
				"{graine$explication}"
			)
		},
		"Options de r\u00e9f\u00e9rence :",
		glue::glue(
			"  - {options$texte} ",
			"({ifelse(options$is_correct, 'CORRECTE', 'fausse')})"
		)
	)

	if (nrow(erreurs)) {
		lignes <- c(
			lignes,
			"Erreurs typiques document\u00e9es, \u00e0 r\u00e9employer comme distracteurs :",
			glue::glue("  - [{erreurs$diagnostic_tag}] {erreurs$description}")
		)
	}

	if (!is.na(graine$cadre) && nzchar(graine$cadre)) {
		lignes <- c(lignes, glue::glue("Cadrage du sous-th\u00e8me : {graine$cadre}"))
	}

	paste(lignes, collapse = "\n")
}

#' Mettre en forme un bloc de consignes pour le rédacteur
#'
#' @param consignes Liste de consignes d'un même type de question.
#'
#' @return Une chaîne de caractères.
#' @noRd
prompt_bloc_consignes <- function(consignes) {
	type <- question_type(consignes[[1L]]$type)

	blocs <- lapply(consignes, function(consigne) {
		paste(
			c(
				glue::glue("### Consigne num\u00e9ro {consigne$numero}"),
				glue::glue(
					"Difficult\u00e9 adaptative vis\u00e9e : {consigne$difficulte} sur 5 ",
					"(niveau de s\u00e9ance du r\u00e9gleur)."
				),
				type$consigne(consigne),
				"",
				prompt_bloc_graine(consigne$graine)
			),
			collapse = "\n"
		)
	})

	paste(unlist(blocs), collapse = "\n\n")
}
