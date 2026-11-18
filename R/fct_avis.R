# Avis des utilisateurs sur chaque question : difficulté ressentie (1 à 10)
# et commentaire facultatif. Les avis vivent dans la séance (`seance$avis`),
# indexés par `item_id`, et sont donc enregistrés avec elle.
#
# Deux notes par question :
# - `note_seance` : donnée pendant la séance, avant de voir la correction
#   (seulement si l'option « Pendant et à la fin » est choisie) ;
# - `note` : la note finale, validée sur l'écran « Vos avis ».
# Comparer les deux montre si l'avis change une fois la correction connue.

#' Longueur maximale d'un commentaire, en caractères
#' @noRd
avis_commentaire_max <- function() 1000L

#' Rendre inertes les cellules qu'Excel lirait comme des formules
#'
#' Un texte saisi par un utilisateur qui commence par `=`, `+`, `-` ou `@`
#' serait exécuté comme une formule à l'ouverture du CSV dans Excel. Une
#' apostrophe en tête le force en texte.
#'
#' @param x Vecteur de caractères.
#'
#' @return `x`, avec une apostrophe devant les valeurs concernées.
#' @noRd
avis_csv_inerte <- function(x) {
	sub("^([=+@-])", "'\\1", x)
}

#' Enregistrer un avis dans la séance
#'
#' @param seance Séance.
#' @param item_id Identifiant de la question.
#' @param note Note de 1 à 10.
#' @param commentaire Texte libre (facultatif).
#' @param moment `"seance"` (pendant la passation) ou `"finale"` (écran Vos avis).
#'
#' @return La séance mise à jour.
#' @noRd
avis_enregistrer <- function(seance, item_id, note, commentaire = NULL, moment = c("seance", "finale")) {
	moment <- match.arg(moment)
	note <- suppressWarnings(as.integer(note))

	if (length(note) != 1L || is.na(note) || note < 1L || note > 10L) {
		rlang::abort("La note doit aller de 1 \u00e0 10.", class = "candidequizz_avis_invalide")
	}

	# Borne la taille du texte libre : il est stocké dans la séance sérialisée.
	commentaire <- substr(trimws(as.character(commentaire %||% "")), 1L, avis_commentaire_max())
	avis <- seance$avis[[item_id]] %||% list(item_id = item_id, note_seance = NA_integer_)

	if (identical(moment, "seance")) {
		avis$note_seance <- note
	} else {
		avis$note <- note
	}
	avis$commentaire <- if (nzchar(commentaire)) commentaire else NA_character_
	avis$note_le <- Sys.time()

	seance$avis[[item_id]] <- avis
	seance
}

#' Clé d'une question, pour reconnaître une question déjà notée
#'
#' L'identifiant d'item dépend de la place de la question dans la séance
#' (`<graine>-L<lot>Q<numéro>`) : il change d'une séance à l'autre. Une même
#' graine avec le même énoncé est la même question ; une réécriture par l'IA
#' (autre énoncé) compte comme une question nouvelle, à noter.
#'
#' @param seed_id Identifiant de la graine.
#' @param enonce Énoncé tel qu'affiché.
#'
#' @return Une chaîne de caractères.
#' @noRd
avis_cle <- function(seed_id, enonce) {
	paste(as.character(seed_id), trimws(as.character(enonce)), sep = " | ")
}

#' Notes déjà données par un compte lors de séances précédentes
#'
#' @param user_id Compte.
#' @param sauf_session_id Séance en cours, à ignorer.
#'
#' @return Une liste nommée par [avis_cle()] : `note` et `commentaire`. Quand
#'   une question a été notée plusieurs fois, la note la plus récente l'emporte.
#' @noRd
avis_anterieurs <- function(user_id, sauf_session_id = NULL) {
	# De la plus récente à la plus ancienne (ordre de stats_charger()).
	seances <- tryCatch(stats_charger(user_id = user_id), error = function(e) list())
	notes <- list()

	for (s in seances) {
		if (identical(as.character(s$session_id), as.character(sauf_session_id %||% ""))) {
			next
		}

		for (q in avis_questions(s)) {
			cle <- avis_cle(q$seed_id, q$enonce)

			if (!is.na(q$note) && is.null(notes[[cle]])) {
				notes[[cle]] <- list(note = q$note, commentaire = q$commentaire)
			}
		}
	}

	notes
}

#' Questions à noter, avec les avis déjà donnés
#'
#' Une ligne par question vue (validée ou passée), dans l'ordre de la séance.
#'
#' @param seance Séance.
#'
#' @return Une liste de listes.
#' @noRd
avis_questions <- function(seance) {
	lapply(seance$reponses %||% list(), function(r) {
		it <- seance_item(seance, r$indice_seance %||% r$numero)
		avis <- seance$avis[[r$item_id]] %||% list()

		# Options de la question (types à options), avec la bonne réponse et
		# celle de l'utilisateur, pour qu'il se souvienne de la question.
		choisies <- suppressWarnings(as.integer(unlist(r$reponse %||% integer())))
		options <- lapply(seq_along(it$payload$options %||% list()), function(i) {
			o <- it$payload$options[[i]]
			list(
				lettre = LETTERS[[i]],
				texte = as.character(o$texte %||% ""),
				correcte = isTRUE(o$is_correct),
				choisie = !isTRUE(r$passee) && i %in% choisies
			)
		})

		list(
			item_id = r$item_id,
			seed_id = as.character(r$seed_id %||% NA_character_),
			numero = as.integer(r$indice_seance %||% r$numero),
			enonce = as.character(it$enonce %||% ""),
			options = options,
			passee = isTRUE(r$passee),
			is_correct = isTRUE(r$is_correct),
			note_seance = as.integer(avis$note_seance %||% NA_integer_),
			note = as.integer(avis$note %||% avis$note_seance %||% NA_integer_),
			commentaire = as.character(avis$commentaire %||% NA_character_)
		)
	})
}

#' Tous les avis validés sont-ils là ?
#' @noRd
avis_complets <- function(seance) {
	isTRUE(!is.null(seance$avis_valides_le))
}

#' Toutes les séances enregistrées, tous comptes confondus (administration)
#' @noRd
avis_seances_toutes <- function() {
	seances <- if (identical(store_backend(), "sqlite")) {
		lignes <- db_query("SELECT blob FROM seances")
		lapply(lignes$blob, store_sqlite_unserialize)
	} else {
		fichiers <- list.files(store_dir(), pattern = "\\.rds$", full.names = TRUE)
		lapply(fichiers, function(f) tryCatch(readRDS(f), error = function(e) NULL))
	}

	Filter(function(s) !is.null(s) && !is.null(s$session_id), seances)
}

#' Tableau de tous les avis, pour l'export administrateur
#'
#' @param seances Liste de séances (par défaut, toutes).
#'
#' @return Un tibble, une ligne par question notée.
#' @noRd
avis_tableau <- function(seances = avis_seances_toutes()) {
	lignes <- lapply(seances, function(s) {
		if (!length(s$avis)) {
			return(NULL)
		}

		qs <- avis_questions(s)
		qs <- Filter(function(q) !is.null(s$avis[[q$item_id]]), qs)

		if (!length(qs)) {
			return(NULL)
		}

		tibble::tibble(
			session_id = s$session_id,
			user_id = as.character(s$user_id %||% NA_character_),
			profil = as.character(s$parametres$profil %||% NA_character_),
			numero = vapply(qs, `[[`, integer(1), "numero"),
			item_id = vapply(qs, `[[`, character(1), "item_id"),
			seed_id = vapply(qs, `[[`, character(1), "seed_id"),
			enonce = vapply(qs, `[[`, character(1), "enonce"),
			juste = vapply(qs, `[[`, logical(1), "is_correct"),
			passee = vapply(qs, `[[`, logical(1), "passee"),
			note_pendant = vapply(qs, `[[`, integer(1), "note_seance"),
			note_finale = vapply(qs, function(q) as.integer(s$avis[[q$item_id]]$note %||% NA_integer_), integer(1)),
			commentaire = vapply(qs, `[[`, character(1), "commentaire"),
			note_le = vapply(qs, function(q) format(s$avis[[q$item_id]]$note_le %||% NA), character(1))
		)
	})

	lignes <- Filter(Negate(is.null), lignes)

	if (!length(lignes)) {
		return(tibble::tibble())
	}

	tableau <- do.call(rbind, lignes)
	# Note changée entre la passation et l'écran final.
	tableau$note_modifiee <- !is.na(tableau$note_pendant) & !is.na(tableau$note_finale) &
		tableau$note_pendant != tableau$note_finale
	tableau
}
