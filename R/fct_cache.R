# Cache de questions vérifiées.
#
# Une question écrite par le modèle et validée par le vérificateur factuel a
# coûté un appel de rédaction et un appel de relecture. La jeter à la fin de la
# séance, c'est repayer les deux la prochaine fois que la graine revient. Le
# cache la garde sur l'appareil, avec l'empreinte de sa graine : si le
# classeur change, la question devient invisible au lieu d'être servie fausse.
#
# Un seul écrivain : le processus principal, après intégration d'une tranche.
# Le processus de préparation ne fait que lire, ce qui évite toute écriture
# concurrente.

#' Le cache est-il activé
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
cache_actif <- function() {
	isTRUE(cq_config("cache", "actif"))
}

#' Chemin du fichier de cache
#'
#' Rangé sous le répertoire des séances, dans un sous-dossier : il suit ainsi
#' `CANDIDEQUIZZ_STORE_DIR` (tests, choix de l'utilisateur) sans être pris pour
#' une séance par `store_lister()`.
#'
#' @return Un chemin.
#' @noRd
cache_chemin <- function() {
	file.path(store_dir(), "cache", "questions_verifiees.rds")
}

#' Empreinte du contenu d'une graine
#'
#' Tout ce que la question reformule : si l'un de ces champs change dans le
#' classeur, les questions dérivées de l'ancienne version ne sont plus servies.
#'
#' @param graine Une ligne de graine.
#'
#' @return Une chaîne de caractères.
#' @noRd
cache_empreinte <- function(graine) {
	options <- graine$options[[1L]] %||% list()

	rlang::hash(list(
		as.character(graine$seed_id),
		as.character(graine$question_ref %||% ""),
		as.character(graine$objectif %||% ""),
		as.character(graine$explication %||% ""),
		as.character(options$texte %||% character(0)),
		as.logical(options$is_correct %||% logical(0))
	))
}

#' Lire le cache
#'
#' @param chemin Chemin du fichier.
#'
#' @return Une liste d'entrées, vide si le fichier est absent ou illisible.
#' @noRd
cache_lire <- function(chemin = cache_chemin()) {
	if (!file.exists(chemin)) {
		return(list())
	}

	entrees <- tryCatch(readRDS(chemin), error = function(e) list())

	if (is.list(entrees)) entrees else list()
}

#' Écrire le cache
#'
#' Écriture dans un fichier temporaire puis renommage : le processus de
#' préparation qui lit au même moment voit l'ancienne version ou la nouvelle,
#' jamais un fichier à moitié écrit.
#'
#' @param entrees Liste d'entrées.
#' @param chemin Chemin du fichier.
#'
#' @return `TRUE` si l'écriture a réussi, invisiblement.
#' @noRd
cache_ecrire <- function(entrees, chemin = cache_chemin()) {
	dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
	temporaire <- tempfile("cache-", tmpdir = dirname(chemin), fileext = ".rds")
	saveRDS(entrees, temporaire)

	ok <- file.rename(temporaire, chemin)

	if (!ok) {
		ok <- file.copy(temporaire, chemin, overwrite = TRUE)
		unlink(temporaire)
	}

	invisible(ok)
}

#' Valeur de vérité d'un item « vrai ou faux »
#'
#' @param item Un item.
#'
#' @return `TRUE`, `FALSE`, ou `NA` pour les autres types.
#' @noRd
cache_verite <- function(item) {
	if (!identical(item$type, "vrai_faux")) {
		return(NA)
	}

	as.logical(item$payload$reponse %||% NA)[[1L]]
}

#' Une question en cache répond-elle à une consigne
#'
#' Même graine dans la même version, même type, même nombre d'options, même
#' valeur de vérité pour un vrai-faux, et une difficulté proche.
#'
#' @param entree Une entrée du cache.
#' @param consigne La consigne.
#' @param empreinte Empreinte de la graine de la consigne.
#' @param tolerance Écart de difficulté admis.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
cache_compatible <- function(entree, consigne, empreinte, tolerance) {
	if (!identical(as.character(entree$seed_id), as.character(consigne$graine$seed_id)) ||
			!identical(entree$empreinte, empreinte) ||
			!identical(entree$type, consigne$type)) {
		return(FALSE)
	}

	if (!identical(
		as.integer(entree$n_options %||% NA_integer_),
		as.integer(consigne$n_options %||% NA_integer_)
	)) {
		return(FALSE)
	}
	# Ne pas se fier uniquement au nombre déclaré dans un ancien cache.
	if (consigne$type %in% c("qcm_single", "qcm_multiple") &&
		length(entree$item$payload$options) != as.integer(consigne$n_options)) {
		return(FALSE)
	}

	if (identical(consigne$type, "vrai_faux") &&
			!identical(entree$verite, isTRUE(consigne$verite_attendue))) {
		return(FALSE)
	}

	abs(as.integer(entree$difficulte) - as.integer(consigne$difficulte)) <= tolerance
}

#' Choisir une question en cache pour une consigne
#'
#' La version la plus proche en difficulté, puis la moins servie. À écart et
#' `n_servi` égaux, on tire au sort : sans cela la même variante revient
#' toujours (ordre stable du fichier). Tant qu'une graine a moins de
#' `variantes_max` versions et que la meilleure a déjà été servie, on préfère
#' en écrire une nouvelle : la question se renouvelle quelques fois avant que
#' les versions ne tournent.
#'
#' @param entrees Entrées du cache.
#' @param consigne La consigne.
#' @param exclure_ids Identifiants déjà utilisés dans la séance.
#' @param variantes_max Nombre de versions à constituer avant de les réutiliser.
#' @param tolerance Écart de difficulté admis.
#'
#' @return Une entrée, ou `NULL` s'il faut rédiger.
#' @noRd
cache_choisir <- function(entrees,
													consigne,
													exclure_ids = character(0),
													variantes_max = cq_config("cache", "variantes_max"),
													tolerance = cq_config("cache", "tolerance_difficulte")) {
	if (!length(entrees)) {
		return(NULL)
	}

	empreinte <- cache_empreinte(consigne$graine)
	compatibles <- Filter(
		function(e) cache_compatible(e, consigne, empreinte, as.integer(tolerance)),
		entrees
	)
	candidats <- Filter(function(e) !e$cache_id %in% exclure_ids, compatibles)

	if (!length(candidats)) {
		return(NULL)
	}

	ecart <- vapply(
		candidats,
		function(e) abs(as.integer(e$difficulte) - as.integer(consigne$difficulte)),
		integer(1)
	)
	servi <- vapply(candidats, function(e) as.integer(e$n_servi %||% 0L), integer(1))
	# Tirage sur les ex æquo : `runif` casse l'ordre stable du fichier sans
	# privilégier une variante plus ancienne ou plus récente.
	rang <- order(ecart, servi, stats::runif(length(candidats)))[[1L]]

	if (servi[[rang]] >= 1L && length(compatibles) < as.integer(variantes_max)) {
		return(NULL)
	}

	candidats[[rang]]
}

#' Reconstituer un item à partir d'une entrée du cache
#'
#' @param entree L'entrée choisie.
#' @param consigne La consigne à laquelle elle répond.
#'
#' @return Un item prêt à poser.
#' @noRd
item_depuis_cache <- function(entree, consigne) {
	item <- entree$item
	explication <- trimws(as.character(consigne$graine$explication %||% ""))

	item$item_id <- as.character(item_id(consigne))
	item$numero <- consigne$numero
	item$indice_seance <- consigne$indice_seance
	item$lot <- consigne$lot
	item$difficulte <- as.integer(entree$difficulte)
	item$source <- "cache"
	item$cache_id <- entree$cache_id
	item$verifie <- TRUE

	if (nzchar(explication)) {
		item$explication <- explication
	}

	item_melanger_options(item)
}

#' Une entrée de cache pour un item vérifié
#'
#' @param item Item rédigé par le modèle et validé par le vérificateur.
#' @param maintenant Horodatage.
#'
#' @return Une entrée.
#' @noRd
cache_entree <- function(item, maintenant = Sys.time()) {
	gabarit <- item[setdiff(names(item), c("item_id", "numero", "indice_seance", "lot"))]

	list(
		cache_id = rlang::hash(list(item$seed_id, item$type, item$enonce, item$payload)),
		seed_id = as.character(item$seed_id),
		empreinte = item$empreinte_graine,
		type = item$type,
		n_options = as.integer(item$n_options %||% NA_integer_),
		verite = cache_verite(item),
		difficulte = as.integer(item$difficulte),
		item = gabarit,
		cree_le = maintenant,
		n_servi = 1L,
		dernier_servi = maintenant
	)
}

#' Mettre le cache à jour après une tranche
#'
#' Ajoute les questions rédigées et vérifiées, compte les resservies. Un échec
#' est silencieux : le cache est une économie, jamais une condition pour que la
#' séance continue.
#'
#' @param items Items de la tranche intégrée.
#' @param chemin Chemin du fichier.
#' @param max_entrees Taille maximale ; les entrées servies il y a le plus
#'   longtemps sortent en premier.
#'
#' @return `TRUE` si le cache a été réécrit, invisiblement.
#' @noRd
cache_integrer <- function(items,
													 chemin = cache_chemin(),
													 max_entrees = cq_config("cache", "max_entrees")) {
	if (!cache_actif() || !length(items)) {
		return(invisible(FALSE))
	}

	tryCatch(
		{
			entrees <- cache_lire(chemin)
			ids <- vapply(entrees, function(e) as.character(e$cache_id), character(1))
			maintenant <- Sys.time()
			modifie <- FALSE

			for (item in items) {
				if (identical(item$source, "cache")) {
					position <- match(item$cache_id, ids)

					if (!is.na(position)) {
						entrees[[position]]$n_servi <- as.integer(entrees[[position]]$n_servi %||% 0L) + 1L
						entrees[[position]]$dernier_servi <- maintenant
						modifie <- TRUE
					}

					next
				}

				if (!identical(item$source, "llm") || !isTRUE(item$verifie) || is.null(item$empreinte_graine)) {
					next
				}

				entree <- cache_entree(item, maintenant)

				if (!entree$cache_id %in% ids) {
					entrees <- c(entrees, list(entree))
					ids <- c(ids, entree$cache_id)
					modifie <- TRUE
				}
			}

			if (!modifie) {
				return(invisible(FALSE))
			}

			if (length(entrees) > as.integer(max_entrees)) {
				recence <- vapply(entrees, function(e) as.numeric(e$dernier_servi %||% 0), numeric(1))
				entrees <- entrees[order(recence, decreasing = TRUE)][seq_len(as.integer(max_entrees))]
			}

			cache_ecrire(entrees, chemin)
		},
		error = function(e) invisible(FALSE)
	)
}

#' Paramètres de cache à transmettre à la génération
#'
#' @param seance La séance en cours, ou `NULL` avant le premier lot.
#'
#' @return `list(chemin, exclure_ids)`, ou `NULL` si le cache est désactivé.
#' @noRd
cache_parametres <- function(seance = NULL) {
	if (!cache_actif()) {
		return(NULL)
	}

	deja <- if (is.null(seance)) {
		character(0)
	} else {
		ids <- vapply(seance$items, function(i) as.character(i$cache_id %||% NA_character_), character(1))
		ids[!is.na(ids)]
	}

	list(chemin = cache_chemin(), exclure_ids = deja)
}
