# Réponses de l'utilisateur.
#
# Une réponse ne porte pas seulement un score : elle porte aussi le temps mis à
# répondre, le temps mis avant de toucher à l'écran, le nombre de changements
# d'avis et les confusions touchées. Ce sont ces mesures, et non le seul score,
# qui permettront de dire à l'utilisateur *comment* il apprend.

#' Enregistrer une réponse à un item
#'
#' La notation est déléguée au registre des types : cette fonction ne connaît
#' aucun type de question en particulier.
#'
#' @param item L'item posé.
#' @param reponse La réponse brute de l'utilisateur, dans la forme attendue par
#'   le type.
#' @param affiche_le Horodatage d'affichage de la question.
#' @param premiere_interaction_ms Délai avant la première interaction, en
#'   millisecondes. `NA` si l'utilisateur n'a rien touché.
#' @param n_changements Nombre de fois où l'utilisateur a changé d'avis.
#' @param passee La question a-t-elle été passée sans répondre.
#' @param repondu_le Horodatage de la validation.
#'
#' @return Une réponse.
#' @noRd
reponse_nouvelle <- function(item,
														 reponse,
														 affiche_le,
														 premiere_interaction_ms = NA_real_,
														 n_changements = 0L,
														 passee = FALSE,
														 repondu_le = Sys.time()) {
	notation <- if (passee) {
		list(score = 0, is_correct = FALSE, detail = list(diagnostic_tags = character(0)))
	} else {
		question_type(item$type)$noter(item, reponse)
	}

	list(
		item_id = item$item_id,
		numero = item$numero,
		indice_seance = item$indice_seance,
		lot = item$lot,
		type = item$type,
		difficulte = item$difficulte,
		difficulte_graine = as.integer(item$difficulte_graine %||% NA_integer_),
		seed_id = item$seed_id,
		full_theme_id = item$full_theme_id,
		theme = item$theme,
		sub_theme = item$sub_theme,
		reponse = reponse,
		score = as.numeric(notation$score),
		is_correct = isTRUE(notation$is_correct),
		diagnostic_tags = as.character(notation$detail$diagnostic_tags %||% character(0)),
		detail = notation$detail,
		affiche_le = affiche_le,
		repondu_le = repondu_le,
		duree_ms = as.numeric(difftime(repondu_le, affiche_le, units = "secs")) * 1000,
		premiere_interaction_ms = as.numeric(premiere_interaction_ms),
		n_changements = as.integer(n_changements),
		passee = isTRUE(passee)
	)
}

#' Les réponses sous forme de tableau
#'
#' Base de tous les calculs statistiques et de l'export des réponses brutes.
#'
#' @param reponses Liste de réponses.
#'
#' @return Un tibble.
#' @noRd
reponses_tibble <- function(reponses) {
	vide <- tibble::tibble(
		item_id = character(0),
		indice_seance = integer(0),
		lot = integer(0),
		type = character(0),
		difficulte = integer(0),
		difficulte_graine = integer(0),
		full_theme_id = character(0),
		theme = character(0),
		sub_theme = character(0),
		score = numeric(0),
		is_correct = logical(0),
		duree_s = numeric(0),
		latence_s = numeric(0),
		n_changements = integer(0),
		passee = logical(0)
	)

	if (!length(reponses)) {
		return(vide)
	}

	texte <- function(champ) {
		vapply(reponses, function(r) as.character(r[[champ]] %||% NA_character_)[[1L]], character(1))
	}

	nombre <- function(champ) {
		vapply(reponses, function(r) as.numeric(r[[champ]] %||% NA_real_)[[1L]], numeric(1))
	}

	booleen <- function(champ) {
		vapply(reponses, function(r) isTRUE(r[[champ]]), logical(1))
	}

	tibble::tibble(
		item_id = texte("item_id"),
		indice_seance = as.integer(nombre("indice_seance")),
		lot = as.integer(nombre("lot")),
		type = texte("type"),
		difficulte = as.integer(nombre("difficulte")),
		difficulte_graine = as.integer(nombre("difficulte_graine")),
		full_theme_id = texte("full_theme_id"),
		theme = texte("theme"),
		sub_theme = texte("sub_theme"),
		score = nombre("score"),
		is_correct = booleen("is_correct"),
		duree_s = nombre("duree_ms") / 1000,
		latence_s = nombre("premiere_interaction_ms") / 1000,
		n_changements = as.integer(nombre("n_changements")),
		passee = booleen("passee")
	)
}

#' Toutes les confusions touchées
#'
#' @param reponses Liste de réponses.
#'
#' @return Un tibble `diagnostic_tag`, `n`, trié par fréquence décroissante.
#' @noRd
reponses_confusions <- function(reponses) {
	tags <- unlist(lapply(reponses, function(r) r$diagnostic_tags), use.names = FALSE)
	tags <- tags[!is.na(tags) & nzchar(tags)]

	if (!length(tags)) {
		return(tibble::tibble(diagnostic_tag = character(0), n = integer(0)))
	}

	effectifs <- sort(table(tags), decreasing = TRUE)

	tibble::tibble(
		diagnostic_tag = names(effectifs),
		n = as.integer(effectifs)
	)
}

#' Tableau d'export d'une séance (questions posées)
#'
#' Cinq colonnes pédagogiques uniquement — jamais la structure interne de la
#' banque ni les métadonnées techniques de passation.
#'
#' @param seance Une séance (items + réponses).
#'
#' @return Un tibble : question, thème, options, réponse correcte, explication.
#' @noRd
seance_export_tableau <- function(seance) {
	colonnes <- c(
		"La question",
		"Le th\u00e8me",
		"Les options de r\u00e9ponse",
		"R\u00e9ponse correcte",
		"Explication"
	)

	vide <- stats::setNames(
		tibble::tibble(
			a = character(0),
			b = character(0),
			c = character(0),
			d = character(0),
			e = character(0)
		),
		colonnes
	)

	reponses <- seance$reponses %||% list()
	items <- seance$items %||% list()

	if (!length(reponses) || !length(items)) {
		return(vide)
	}

	ids <- vapply(items, function(i) as.character(i$item_id %||% ""), character(1))
	noms <- ids
	noms[nzchar(noms)] <- ids[nzchar(noms)]
	par_id <- stats::setNames(items, noms)

	lignes <- lapply(reponses, function(r) {
		id <- as.character(r$item_id %||% "")
		item <- if (nzchar(id) && id %in% names(par_id)) par_id[[id]] else NULL

		if (is.null(item)) {
			return(NULL)
		}

		theme_lib <- trimws(paste(
			as.character(item$theme %||% ""),
			as.character(item$sub_theme %||% ""),
			sep = " / "
		))
		theme_lib <- sub("\\s*/\\s*$", "", theme_lib)
		theme_lib <- sub("^\\s*/\\s*", "", theme_lib)

		stats::setNames(
			tibble::tibble(
				a = as.character(item$enonce %||% ""),
				b = theme_lib,
				c = item_options_texte(item),
				d = question_type_solution(item),
				e = as.character(item$explication %||% "")
			),
			colonnes
		)
	})

	lignes <- Filter(Negate(is.null), lignes)

	if (!length(lignes)) {
		return(vide)
	}

	tibble::as_tibble(do.call(rbind, lignes))
}

#' Libellé d'une réponse utilisateur, pour le bilan
#'
#' Sans item, lecture générique de la valeur brute. Avec l'item, les indices
#' d'options ou de positions sont traduits en libellés.
#'
#' @param reponse Une réponse enregistrée.
#' @param item L'item correspondant, ou `NULL`.
#'
#' @return Une chaîne.
#' @noRd
reponse_libelle <- function(reponse, item = NULL) {
	if (isTRUE(reponse$passee)) {
		return("Question pass\u00e9e")
	}

	valeur <- reponse$reponse

	if (is.null(valeur) || (is.atomic(valeur) && !length(valeur))) {
		return("\u2014")
	}

	lu <- if (!is.null(item)) tryCatch(reponse_libelle_item(valeur, item), error = function(e) NULL)
	if (length(lu) == 1L && !is.na(lu) && nzchar(lu)) {
		return(lu)
	}

	if (is.atomic(valeur)) {
		texte <- trimws(paste(as.character(valeur), collapse = ", "))

		return(if (nzchar(texte)) texte else "\u2014")
	}

	plats <- unlist(valeur, use.names = FALSE)
	texte <- trimws(paste(as.character(plats), collapse = ", "))

	if (nzchar(texte)) texte else "\u2014"
}

#' Traduire une réponse brute selon le type de l'item
#'
#' @param valeur Réponse brute (indices, positions, booléen ou mots).
#' @param item L'item.
#'
#' @return Une chaîne, ou `NULL` si le type n'est pas reconnu.
#' @noRd
reponse_libelle_item <- function(valeur, item) {
	brut <- unlist(valeur, use.names = FALSE)
	indices <- suppressWarnings(as.integer(brut))

	switch(
		as.character(item$type %||% ""),
		qcm_single = ,
		qcm_multiple = {
			textes <- vld_textes(item$payload$options)
			ok <- indices[!is.na(indices) & indices >= 1L & indices <= length(textes)]
			if (length(ok)) paste(textes[unique(ok)], collapse = " ; ") else NULL
		},
		ordre = {
			textes <- vapply(item$payload$elements, function(e) as.character(e$texte), character(1))
			ok <- indices[!is.na(indices) & indices >= 1L & indices <= length(textes)]
			if (length(ok)) paste(seq_along(ok), textes[ok], sep = ". ", collapse = " \u2192 ") else NULL
		},
		vrai_faux = {
			v <- suppressWarnings(as.logical(brut[[1L]]))
			if (is.na(v)) NULL else if (v) "Vrai" else "Faux"
		},
		texte_a_trous = {
			mots <- as.character(brut)
			mots[is.na(mots) | !nzchar(trimws(mots))] <- "\u2014"
			paste(mots, collapse = " \u00b7 ")
		},
		NULL
	)
}
