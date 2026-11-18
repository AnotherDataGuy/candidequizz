# Agent A-3 : rédacteur d'items.
#
# Agent LLM. Un appel par type de question présent parmi les consignes que le
# cache n'a pas couvertes, chaque appel étant contraint par le schéma strict du
# type. C'est le seul agent qui produit du contenu.
#
# En cas d'absence de clé ou d'échec du modèle, l'agent ne propage pas
# l'erreur : il sert des items de repli tirés directement des graines. Une
# séance commencée doit pouvoir se terminer.

#' Rappel de variété à injecter dans l'invite
#'
#' Une graine peut être servie plusieurs fois dans une même séance quand la
#' sélection thématique est étroite. Le rédacteur doit alors être averti :
#' sans cela il produit deux fois la même question, et l'utilisateur a le
#' sentiment de tourner en rond.
#'
#' @param consignes Consignes du groupe.
#'
#' @return Une chaîne de caractères, éventuellement vide.
#' @noRd
redacteur_rappel_variete <- function(consignes) {
	occurrences <- vapply(
		consignes,
		function(c) as.integer(c$graine$occurrences_total %||% 1L),
		integer(1)
	)

	if (!any(occurrences > 1L)) {
		return("")
	}

	paste(
		"Attention : certaines graines apparaissent plusieurs fois dans ce lot,",
		"car la s\u00e9lection th\u00e9matique est \u00e9troite. Pour chaque graine r\u00e9p\u00e9t\u00e9e,",
		"change nettement d'angle d'une consigne \u00e0 l'autre : interroge une autre",
		"facette de l'objectif d'apprentissage, et non la m\u00eame question reformul\u00e9e."
	)
}

#' Consigne de niveau de langue, ignorée hors IA
#' @noRd
redacteur_rappel_langue <- function(langue) {
	if (identical(langue, "simple")) {
		return(paste(
			"Niveau de langue : phrases courtes, vocabulaire courant,",
			"sans tournure administrative."
		))
	}

	if (identical(langue, "administratif")) {
		return(paste(
			"Niveau de langue : formulations administratives claires,",
			"proches des textes officiels, sans jargon inutile."
		))
	}

	""
}

#' Schéma de sortie du rédacteur pour un type donné
#'
#' @param type Identifiant du type de question.
#' @param n_options Nombre d'options à demander.
#'
#' @return Un `ellmer::type_object()` enveloppant un tableau d'items.
#' @noRd
redacteur_schema <- function(type, n_options) {
	ellmer::type_object(
		items = ellmer::type_array(
			question_type(type)$schema(n_options),
			"Un item par consigne re\u00e7ue, dans l'ordre des consignes."
		)
	)
}

#' Apparier les items renvoyés par le modèle aux consignes
#'
#' L'appariement se fait par le `numero` que le modèle recopie, et non par la
#' position dans le tableau : un modèle qui omet ou duplique un item ne doit pas
#' décaler silencieusement tous les suivants, ce qui rattacherait une question à
#' la mauvaise graine et fausserait les statistiques par thème.
#'
#' @param bruts Items renvoyés par le modèle.
#' @param consignes Consignes du groupe.
#'
#' @return Une liste de même longueur que `consignes`, contenant l'item brut ou
#'   `NULL` si le modèle ne l'a pas fourni.
#' @noRd
redacteur_apparier <- function(bruts, consignes) {
	numeros_attendus <- vapply(consignes, function(c) as.integer(c$numero), integer(1))

	numeros_recus <- vapply(
		bruts,
		function(b) suppressWarnings(as.integer(b$numero %||% NA_integer_))[[1L]],
		integer(1)
	)

	lapply(numeros_attendus, function(numero) {
		positions <- which(!is.na(numeros_recus) & numeros_recus == numero)

		if (!length(positions)) {
			return(NULL)
		}

		bruts[[positions[[1L]]]]
	})
}

#' Rédiger les items d'un groupe de consignes de même type
#'
#' @param consignes Consignes du groupe.
#' @param ctx Contexte, pour le mode dégradé.
#'
#' @return `list(items, journal)`.
#' @noRd
redacteur_groupe <- function(consignes, ctx) {
	type <- question_type(consignes[[1L]]$type)
	n_options <- consignes[[1L]]$n_options

	if (ctx$degrade) {
		motif <- if (identical(ctx$parametres$generation, "ia_off")) {
			"s\u00e9ance sans IA"
		} else if (is.null(ctx$coupure_llm)) {
			"cl\u00e9 d'API absente"
		} else {
			"mod\u00e8le indisponible pour ce lot"
		}

		return(list(
			items = lapply(consignes, item_depuis_graine, motif = motif),
			journal = NULL
		))
	}

	appel <- llm_appel_structure(
		agent = "redacteur",
		role = "redacteur",
		systeme = prompt_rendre("redacteur_systeme", socle = prompt_socle()),
		invite = prompt_rendre(
			"redacteur_invite",
			nb_items = length(consignes),
			type_libelle = type$libelle,
			rappel_variete = paste(
			redacteur_rappel_variete(consignes),
			redacteur_rappel_langue(ctx$parametres$langue)
		),
			bloc_consignes = prompt_bloc_consignes(consignes)
		),
		schema = redacteur_schema(type$id, n_options)
	)

	if (!appel$ok) {
		return(list(
			items = lapply(
				consignes,
				item_depuis_graine,
				motif = glue::glue("\u00e9chec du mod\u00e8le : {appel$message}")
			),
			journal = appel$journal
		))
	}

	modele <- appel$journal$modele
	apparies <- redacteur_apparier(appel$donnees$items %||% list(), consignes)

	items <- lapply(seq_along(consignes), function(i) {
		if (is.null(apparies[[i]])) {
			return(item_depuis_graine(
				consignes[[i]],
				motif = "item absent de la r\u00e9ponse du mod\u00e8le"
			))
		}

		item_depuis_llm(apparies[[i]], consignes[[i]], modele = modele)
	})

	list(items = items, journal = appel$journal)
}

#' Rédiger les items d'un lot
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `items` renseigné.
#' @noRd
agent_redacteur <- function(ctx) {
	items <- ctx$items %||% list()
	servis <- vapply(items, function(i) as.integer(i$numero), integer(1))
	a_rediger <- Filter(function(c) !as.integer(c$numero) %in% servis, ctx$consignes)
	groupes <- consignes_par_type(a_rediger)

	for (groupe in groupes) {
		resultat <- redacteur_groupe(groupe, ctx)
		items <- c(items, resultat$items)
		ctx <- ctx_journaliser(ctx, resultat$journal)
	}

	# Remettre dans l'ordre des consignes : le regroupement par type a servi les
	# appels, il ne doit pas dicter l'ordre de passation.
	numeros <- vapply(items, function(i) as.integer(i$numero), integer(1))
	ctx$items <- items[order(numeros)]

	n_repli <- sum(vapply(ctx$items, function(i) identical(i$source, "graine_repli"), logical(1)))

	ctx_detail(
		ctx,
		n_items = length(ctx$items),
		n_appels = length(groupes),
		n_repli = n_repli
	)
}
