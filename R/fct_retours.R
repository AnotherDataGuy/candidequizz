# Retours des participants, question par question (administration).
#
# Tout est relu depuis les séances enregistrées (réponses et avis vivent dans
# la séance sérialisée) : pas de seconde copie des données à tenir à jour.
# Seul le statut « traité » a sa propre table, `retours_statut`.
#
# La note d'un avis est une DIFFICULTÉ RESSENTIE (1 facile, 10 difficile),
# pas une note de qualité. Les signaux utiles pour corriger la banque sont
# donc : les commentaires, le taux d'échec, et l'écart entre difficulté
# ressentie et difficulté prévue dans la banque.

#' Identifiants de compte vers alias, pour afficher qui a commenté
#'
#' @return Vecteur nommé `user_id -> username` (vide hors base SQL).
#' @noRd
retours_noms <- function() {
	if (!identical(store_backend(), "sqlite")) {
		return(character(0))
	}

	comptes <- db_query("SELECT user_id, username FROM users")
	stats::setNames(as.character(comptes$username), as.character(comptes$user_id))
}

#' Statut de chaque graine (`a_traiter` ou `traite`)
#'
#' @return Vecteur nommé `seed_id -> statut` (vide hors base SQL).
#' @noRd
retours_statuts <- function() {
	if (!identical(store_backend(), "sqlite")) {
		return(character(0))
	}

	lignes <- db_query("SELECT seed_id, statut FROM retours_statut")
	stats::setNames(as.character(lignes$statut), as.character(lignes$seed_id))
}

#' Marquer une graine comme traitée, ou la remettre à traiter
#'
#' @param seed_id Identifiant de la graine.
#' @param statut `"traite"` ou `"a_traiter"`.
#'
#' @return `TRUE`, invisiblement.
#' @noRd
retours_marquer <- function(seed_id, statut = c("traite", "a_traiter")) {
	statut <- match.arg(statut)
	seed_id <- as.character(seed_id %||% "")

	if (length(seed_id) != 1L || !nzchar(seed_id)) {
		rlang::abort("Question inconnue.", class = "candidequizz_retours_invalide")
	}

	if (!identical(store_backend(), "sqlite")) {
		rlang::abort(
			"Le suivi des retours demande la base SQL.",
			class = "candidequizz_retours_invalide"
		)
	}

	db_execute(
		paste(
			"INSERT INTO retours_statut (seed_id, statut, maj_le) VALUES (?, ?, ?)",
			"ON CONFLICT (seed_id) DO UPDATE SET statut = excluded.statut, maj_le = excluded.maj_le"
		),
		params = list(seed_id, statut, store_sqlite_temps(Sys.time()))
	)

	invisible(TRUE)
}

#' Tableau des retours, une ligne par graine
#'
#' @param seances Séances de tous les comptes.
#' @param noms Vecteur `user_id -> alias` (voir [retours_noms()]).
#' @param statuts Vecteur `seed_id -> statut` (voir [retours_statuts()]).
#' @param bank Banque chargée, pour l'énoncé d'origine et le thème. `NULL` :
#'   l'énoncé affiché pendant la séance est utilisé.
#'
#' @return Un tibble : `seed_id`, `theme`, `question`, `n_reponses`,
#'   `taux_echec` (0 à 1, NA sans réponse), `duree_mediane_s`, `n_avis`,
#'   `difficulte_ressentie` (moyenne 1 à 10), `difficulte_banque`,
#'   `n_commentaires`, `commentaires` (colonne liste de textes « alias : texte »),
#'   `statut`.
#' @noRd
retours_tableau <- function(seances = avis_seances_toutes(),
														noms = retours_noms(),
														statuts = retours_statuts(),
														bank = tryCatch(bank_load(), error = function(e) NULL)) {
	vide <- tibble::tibble(
		seed_id = character(0), theme = character(0), question = character(0),
		n_reponses = integer(0), taux_echec = numeric(0), duree_mediane_s = numeric(0),
		n_avis = integer(0), difficulte_ressentie = numeric(0), difficulte_banque = numeric(0),
		n_commentaires = integer(0), commentaires = list(), statut = character(0)
	)

	# Réponses : une ligne par question vue.
	reponses <- do.call(rbind, lapply(seances, function(s) {
		rs <- s$reponses %||% list()
		if (!length(rs)) {
			return(NULL)
		}
		data.frame(
			seed_id = vapply(rs, function(r) as.character(r$seed_id %||% NA_character_), character(1)),
			enonce = vapply(seq_along(rs), function(i) {
				it <- tryCatch(seance_item(s, rs[[i]]$indice_seance %||% rs[[i]]$numero), error = function(e) NULL)
				as.character(it$enonce %||% NA_character_)
			}, character(1)),
			juste = vapply(rs, function(r) isTRUE(r$is_correct), logical(1)),
			passee = vapply(rs, function(r) isTRUE(r$passee), logical(1)),
			duree_s = vapply(rs, function(r) as.numeric(r$duree_ms %||% NA_real_) / 1000, numeric(1)),
			stringsAsFactors = FALSE
		)
	}))

	avis <- avis_tableau(seances)

	ids <- unique(c(
		if (!is.null(reponses)) reponses$seed_id,
		if (nrow(avis)) avis$seed_id
	))
	ids <- ids[!is.na(ids) & nzchar(ids)]

	if (!length(ids)) {
		return(vide)
	}

	graines <- if (!is.null(bank)) bank$graines else NULL

	lignes <- lapply(ids, function(id) {
		r <- if (is.null(reponses)) reponses else reponses[reponses$seed_id %in% id, , drop = FALSE]
		vues <- if (is.null(r)) r else r[!r$passee, , drop = FALSE]
		a <- if (nrow(avis)) avis[avis$seed_id %in% id, , drop = FALSE] else avis

		notes <- if (nrow(a)) ifelse(is.na(a$note_finale), a$note_pendant, a$note_finale) else integer(0)
		notes <- notes[!is.na(notes)]

		textes <- if (nrow(a)) a$commentaire else character(0)
		garder <- !is.na(textes) & nzchar(trimws(textes))
		auteurs <- if (nrow(a)) unname(noms[a$user_id[garder]]) else character(0)
		auteurs[is.na(auteurs)] <- "compte supprim\u00e9 ou local"
		commentaires <- if (any(garder)) paste0(auteurs, " : ", textes[garder]) else character(0)

		g <- if (!is.null(graines)) graines[graines$seed_id %in% id, , drop = FALSE] else NULL
		question <- if (!is.null(g) && nrow(g)) {
			as.character(g$question_ref[[1L]])
		} else {
			vus <- c(if (!is.null(r)) r$enonce, if (nrow(a)) a$enonce)
			vus <- vus[!is.na(vus) & nzchar(vus)]
			if (length(vus)) vus[[1L]] else NA_character_
		}

		tibble::tibble(
			seed_id = id,
			theme = if (!is.null(g) && nrow(g)) as.character(g$theme[[1L]]) else NA_character_,
			question = question,
			n_reponses = if (is.null(vues)) 0L else nrow(vues),
			taux_echec = if (is.null(vues) || !nrow(vues)) NA_real_ else mean(!vues$juste),
			duree_mediane_s = if (is.null(vues) || !any(!is.na(vues$duree_s))) NA_real_ else stats::median(vues$duree_s, na.rm = TRUE),
			n_avis = length(notes),
			difficulte_ressentie = if (length(notes)) mean(notes) else NA_real_,
			difficulte_banque = if (!is.null(g) && nrow(g)) suppressWarnings(as.numeric(g$difficulte_graine[[1L]])) else NA_real_,
			n_commentaires = length(commentaires),
			commentaires = list(commentaires),
			statut = unname(statuts[id]) %||% NA_character_
		)
	})

	tableau <- do.call(rbind, lignes)
	tableau$statut[is.na(tableau$statut)] <- "a_traiter"
	tableau
}

#' Filtrer et trier les retours pour l'écran d'administration
#'
#' @param tableau Résultat de [retours_tableau()].
#' @param statut `"a_traiter"`, `"traite"` ou `"tous"`.
#' @param tri `"commentaires"`, `"echec"` ou `"difficulte"`.
#' @param avec_avis Ne garder que les questions qui ont un avis ou un
#'   commentaire.
#'
#' @return Le tableau filtré et trié.
#' @noRd
retours_filtrer <- function(tableau, statut = "a_traiter", tri = "commentaires", avec_avis = TRUE) {
	if (!nrow(tableau)) {
		return(tableau)
	}

	if (!identical(statut, "tous")) {
		tableau <- tableau[tableau$statut == statut, , drop = FALSE]
	}

	if (isTRUE(avec_avis)) {
		tableau <- tableau[tableau$n_avis > 0L | tableau$n_commentaires > 0L, , drop = FALSE]
	}

	# Valeurs manquantes en dernier.
	bas <- function(x) ifelse(is.na(x), -Inf, x)

	ordre <- switch(
		tri,
		echec = order(-bas(tableau$taux_echec), -tableau$n_reponses),
		difficulte = order(-bas(tableau$difficulte_ressentie), -tableau$n_avis),
		order(-tableau$n_commentaires, -bas(tableau$taux_echec))
	)

	tableau[ordre, , drop = FALSE]
}

#' Version CSV du tableau des retours (commentaires réunis, cellules inertes)
#'
#' @param tableau Résultat de [retours_tableau()].
#'
#' @return Un data.frame prêt pour `utils::write.csv()`.
#' @noRd
retours_csv <- function(tableau) {
	sortie <- as.data.frame(tableau[setdiff(names(tableau), "commentaires")])
	sortie$commentaires <- vapply(tableau$commentaires, paste, character(1), collapse = " | ")
	for (col in c("question", "commentaires")) {
		sortie[[col]] <- avis_csv_inerte(sortie[[col]])
	}
	sortie
}
