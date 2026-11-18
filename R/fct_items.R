# Construction et représentation des items.
#
# Un item est la forme interne d'une question prête à poser. Les champs communs
# à tous les types vivent au premier niveau ; ce qui est propre au type vit dans
# `payload`, dont la forme est celle du schéma déclaré par le type. Cette
# convention évite d'avoir à déclarer une fonction de conversion par type : le
# schéma de sortie du modèle est déjà la description du `payload`.

#' Champs du schéma qui appartiennent à l'item et non à son `payload`
#'
#' @return Un vecteur de caractères.
#' @noRd
item_champs_generiques <- function() {
	c("numero", "enonce", "explication")
}

#' Identifiant d'item
#'
#' Déterministe : une séance rechargée retrouve les mêmes identifiants, ce qui
#' permet de rapprocher les réponses enregistrées des items sans dépendre de
#' l'ordre de lecture.
#'
#' @param consigne La consigne d'origine.
#'
#' @return Une chaîne de caractères.
#' @noRd
item_id <- function(consigne) {
	glue::glue("{consigne$graine$seed_id}-L{consigne$lot}Q{consigne$numero}")
}

#' Assembler un item à partir d'une sortie de modèle
#'
#' @param brut L'objet renvoyé par le modèle pour cet item.
#' @param consigne La consigne correspondante.
#' @param modele Identifiant du modèle qui a produit l'item.
#'
#' @return Un item.
#' @noRd
item_depuis_llm <- function(brut, consigne, modele = NA_character_) {
	graine <- consigne$graine
	generiques <- item_champs_generiques()

	explication_banque <- trimws(as.character(graine$explication %||% ""))
	explication_llm <- trimws(as.character(brut$explication %||% ""))

	item <- list(
		item_id = as.character(item_id(consigne)),
		numero = consigne$numero,
		indice_seance = consigne$indice_seance,
		lot = consigne$lot,
		type = consigne$type,
		difficulte = consigne$difficulte,
		n_options = consigne$n_options,
		seed_id = graine$seed_id,
		full_theme_id = graine$full_theme_id,
		theme_id = graine$theme_id,
		theme = graine$theme,
		sub_theme_id = graine$sub_theme_id,
		sub_theme = graine$sub_theme,
		objectif = graine$objectif,
		difficulte_graine = as.integer(graine$difficulte_graine %||% NA_integer_),
		enonce = as.character(brut$enonce %||% ""),
		# La colonne EXPLANATION du classeur est le texte montré après réponse.
		# Le LLM peut en proposer une, utilisée seulement en secours.
		explication = if (nzchar(explication_banque)) explication_banque else explication_llm,
		payload = brut[setdiff(names(brut), generiques)],
		source = "llm",
		modele = as.character(modele),
		genere_le = Sys.time(),
		empreinte_graine = cache_empreinte(graine)
	)

	item_melanger_options(item)
}

#' Mélanger les options d'un QCM
#'
#' Le modèle (et les graines) placent trop souvent la bonne réponse en première
#' option. L'ordre d'affichage est donc tiré ici, après assemblage : la
#' notation lit `is_correct`, pas la lettre.
#'
#' @param item L'item assemblé.
#'
#' @return L'item, options permutées si le type en a.
#' @noRd
item_melanger_options <- function(item) {
	options <- item$payload$options

	if (!is.list(options) || length(options) < 2L) {
		return(item)
	}

	item$payload$options <- options[sample.int(length(options))]
	item
}

#' Assembler un item de repli directement depuis sa graine
#'
#' Mode dégradé : sans clé d'API, ou après échec répété du modèle, la séance
#' continue avec la question de référence et ses options. Si deux ou trois
#' choix sont demandés, seule une partie des distracteurs est conservée.
#' La question est alors moins variée
#' mais exacte, ce qui vaut mieux qu'une séance qui s'arrête.
#'
#' @param consigne La consigne correspondante.
#' @param motif Raison du repli, consignée pour le rapport de séance.
#'
#' @return Un item de type `qcm_single`.
#' @noRd
item_depuis_graine <- function(consigne, motif = NA_character_) {
	graine <- consigne$graine
	options <- graine$options[[1L]]
	erreurs <- graine$erreurs[[1L]]
	tags <- rep("", nrow(options))
	fausses <- which(!options$is_correct)
	# M1, M2, ... suivent les options fausses dans leur ordre de référence.
	# Associer les tags avant de mélanger, jamais à une lettre affichée.
	rangs <- match(paste0("M", seq_along(fausses)), toupper(erreurs$mistake_id))
	connues <- !is.na(rangs)
	tags[fausses[connues]] <- erreurs$diagnostic_tag[rangs[connues]]
	if (identical(consigne$type, "qcm_single")) {
		n <- as.integer(consigne$n_options)
		if (n %in% 2:4 && n < nrow(options)) {
			gardees <- sort(c(which(options$is_correct),
				fausses[sample.int(length(fausses), n - 1L)]))
			options <- options[gardees, , drop = FALSE]
			tags <- tags[gardees]
		}
	}

	item <- item_depuis_llm(
		brut = list(
			enonce = graine$question_ref,
			explication = graine$explication %||% graine$objectif,
			options = lapply(seq_len(nrow(options)), function(i) {
				list(
					texte = options$texte[[i]],
					is_correct = options$is_correct[[i]],
					diagnostic_tag = tags[[i]]
				)
			})
		),
		consigne = consigne,
		modele = NA_character_
	)

	# Le repli sert les options de la graine : son type est donc toujours un QCM
	# à option unique, quel que soit le type demandé.
	item$type_demande <- as.character(consigne$type %||% "qcm_single")
	item$type <- "qcm_single"
	item$n_options <- as.integer(nrow(options))
	item$source <- "graine_repli"
	item$motif_repli <- as.character(motif)

	item
}

#' Le lot d'items, sous forme de tableau
#'
#' Vue de synthèse pour la console et pour les rapports. Le `payload` n'y
#' figure pas : il n'a pas de forme commune entre les types.
#'
#' @param items Liste d'items.
#'
#' @return Un tibble.
#' @noRd
items_tibble <- function(items) {
	vide <- tibble::tibble(
		item_id = character(0),
		numero = integer(0),
		type = character(0),
		difficulte = integer(0),
		sub_theme = character(0),
		enonce = character(0),
		source = character(0)
	)

	if (!length(items)) {
		return(vide)
	}

	texte <- function(champ) {
		vapply(items, function(i) as.character(i[[champ]] %||% NA_character_)[[1L]], character(1))
	}

	entier <- function(champ) {
		as.integer(vapply(
			items,
			function(i) as.numeric(i[[champ]] %||% NA_real_)[[1L]],
			numeric(1)
		))
	}

	tibble::tibble(
		item_id = texte("item_id"),
		numero = entier("numero"),
		type = texte("type"),
		difficulte = entier("difficulte"),
		sub_theme = texte("sub_theme"),
		enonce = texte("enonce"),
		source = texte("source")
	)
}

#' Options d'un item, sous forme de texte pour l'export
#'
#' @param item L'item.
#'
#' @return Une chaîne unique, options séparées par ` | `.
#' @noRd
item_options_texte <- function(item) {
	type <- as.character(item$type %||% "")

	if (type %in% c("qcm_single", "qcm_multiple")) {
		options <- item$payload$options %||% list()
		if (!length(options)) {
			return("")
		}
		textes <- vapply(options, function(o) as.character(o$texte %||% ""), character(1))
		return(paste(seq_along(textes), textes, sep = ". ", collapse = " | "))
	}

	if (identical(type, "vrai_faux")) {
		return("1. Vrai | 2. Faux")
	}

	if (identical(type, "ordre")) {
		elements <- item$payload$elements %||% list()
		if (!length(elements)) {
			return("")
		}
		textes <- vapply(elements, function(e) as.character(e$texte %||% ""), character(1))
		return(paste(seq_along(textes), textes, sep = ". ", collapse = " | "))
	}

	if (identical(type, "texte_a_trous")) {
		trous <- item$payload$trous %||% list()
		mots <- vapply(trous, function(t) as.character(t$mot_correct %||% ""), character(1))
		distracteurs <- as.character(item$payload$distracteurs %||% character(0))
		banque <- unique(c(mots[nzchar(mots)], distracteurs[nzchar(distracteurs)]))
		if (!length(banque)) {
			return(as.character(item$payload$texte %||% ""))
		}
		return(paste(seq_along(banque), banque, sep = ". ", collapse = " | "))
	}

	""
}

#' Mettre en forme un item pour la vérification factuelle
#'
#' Le contenu propre au type est transmis en JSON plutôt que mis en forme type
#' par type : le vérificateur juge du fond, la présentation lui importe peu, et
#' ce choix évite d'ajouter une fonction au contrat des types de questions.
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
prompt_bloc_item <- function(item) {
	paste(
		c(
			glue::glue("### Item num\u00e9ro {item$numero} \u2014 type : {question_type(item$type)$libelle}"),
			glue::glue("Th\u00e8me : {item$theme} / {item$sub_theme}"),
			glue::glue("Objectif de la graine : {item$objectif}"),
			glue::glue("\u00c9nonc\u00e9 : {item$enonce}"),
			"Contenu :",
			jsonlite::toJSON(item$payload, auto_unbox = TRUE, pretty = TRUE),
			glue::glue("Explication fournie : {item$explication}")
		),
		collapse = "\n"
	)
}
