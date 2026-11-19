# Tableau de bord d'administration : chiffres calculés à partir des données
# déjà enregistrées (comptes, séances, réponses, avis). Aucune table en plus.
#
# Les fonctions de calcul reçoivent les séances et les comptes en argument :
# elles se testent sans base. Seules admin_comptes_bruts() et
# admin_seances() lisent la base.

#' Comptes enregistrés, avec leur identifiant interne (sans secret)
#'
#' @return Un data.frame : `user_id`, `username`, `is_admin`, `created_at`.
#' @noRd
admin_comptes_bruts <- function() {
	vide <- data.frame(
		user_id = character(0), username = character(0),
		is_admin = logical(0), created_at = character(0),
		stringsAsFactors = FALSE
	)

	if (!identical(store_backend(), "sqlite")) {
		return(vide)
	}

	lignes <- db_query("SELECT user_id, username, is_admin, created_at FROM users ORDER BY created_at")

	if (!nrow(lignes)) {
		return(vide)
	}

	data.frame(
		user_id = as.character(lignes$user_id),
		username = as.character(lignes$username),
		is_admin = as.integer(lignes$is_admin) == 1L,
		created_at = as.character(lignes$created_at),
		stringsAsFactors = FALSE
	)
}

#' Toutes les séances lisibles, tous comptes confondus
#' @noRd
admin_seances <- function() {
	tryCatch(avis_seances_toutes(), error = function(e) list())
}

#' Horodatage en secondes (NA si absent ou illisible)
#' @noRd
admin_temps <- function(x) {
	if (is.null(x) || !length(x)) {
		return(NA_real_)
	}
	v <- suppressWarnings(as.numeric(x[[1L]]))
	if (is.na(v) && is.character(x)) {
		v <- suppressWarnings(as.numeric(as.POSIXct(x[[1L]], tz = "UTC")))
	}
	v
}

#' Réponses de toutes les séances, une ligne par question répondue
#'
#' Questions passées exclues. `compte` dit si la réponse compte pour
#' l'ouverture de l'IA (réponses trop rapides exclues, comme ia_etat()).
#'
#' @return Un data.frame : `user_id`, `seed_id`, `juste`, `compte`, `quand`
#'   (secondes depuis 1970, UTC).
#' @noRd
admin_reponses <- function(seances) {
	lignes <- lapply(seances, function(s) {
		rs <- Filter(function(r) !isTRUE(r$passee), s$reponses %||% list())
		if (!length(rs)) {
			return(NULL)
		}
		debut <- admin_temps(s$cree_le)
		data.frame(
			user_id = as.character(s$user_id %||% NA_character_),
			seed_id = vapply(rs, function(r) as.character(r$seed_id %||% NA_character_)[[1L]], character(1)),
			juste = vapply(rs, function(r) isTRUE(r$is_correct), logical(1)),
			compte = vapply(rs, function(r) isTRUE(tryCatch(anti_abus_reponse_comptee(r), error = function(e) TRUE)), logical(1)),
			quand = vapply(rs, function(r) {
				t <- admin_temps(r$repondu_le)
				if (is.na(t)) debut else t
			}, numeric(1)),
			stringsAsFactors = FALSE
		)
	})

	lignes <- Filter(Negate(is.null), lignes)

	if (!length(lignes)) {
		return(data.frame(
			user_id = character(0), seed_id = character(0), juste = logical(0),
			compte = logical(0), quand = numeric(0), stringsAsFactors = FALSE
		))
	}

	do.call(rbind, lignes)
}

#' Statut d'activité d'un compte
#'
#' @param derniere Dernière activité (secondes), NA si jamais.
#' @param maintenant Instant de référence (secondes).
#' @param jours_actif Au-delà, le compte est « inactif ».
#'
#' @return `"actif"`, `"inactif"` ou `"jamais"`.
#' @noRd
admin_statut <- function(derniere, maintenant, jours_actif = 14) {
	ifelse(
		is.na(derniere), "jamais",
		ifelse(maintenant - derniere <= jours_actif * 86400, "actif", "inactif")
	)
}

#' Tableau des comptes pour l'écran d'administration
#'
#' @param comptes Résultat de [admin_comptes_bruts()].
#' @param seances Liste de séances (tous comptes).
#' @param maintenant Instant de référence.
#' @param seuil_ia Questions différentes nécessaires pour ouvrir l'IA.
#'
#' @return Un data.frame, une ligne par compte : `username`, `is_admin`,
#'   `cree_le` (secondes), `n_seances`, `n_reponses`, `taux_reussite`
#'   (0 à 1, NA sans réponse), `n_distinctes`, `seuil_ia`, `derniere`
#'   (secondes, NA si jamais), `statut`.
#' @noRd
admin_comptes_tableau <- function(comptes,
																	seances,
																	maintenant = Sys.time(),
																	seuil_ia = tryCatch(ia_seuil(), error = function(e) 400L)) {
	maintenant <- as.numeric(maintenant)
	reponses <- admin_reponses(seances)

	debuts <- data.frame(
		user_id = vapply(seances, function(s) as.character(s$user_id %||% NA_character_)[[1L]], character(1)),
		quand = vapply(seances, function(s) admin_temps(s$cree_le), numeric(1)),
		stringsAsFactors = FALSE
	)

	lignes <- lapply(seq_len(nrow(comptes)), function(i) {
		id <- comptes$user_id[[i]]
		r <- reponses[reponses$user_id %in% id, , drop = FALSE]
		d <- debuts[debuts$user_id %in% id, , drop = FALSE]
		moments <- c(r$quand, d$quand)
		moments <- moments[!is.na(moments)]
		graines <- r$seed_id[r$compte & !is.na(r$seed_id) & nzchar(r$seed_id)]

		data.frame(
			username = comptes$username[[i]],
			is_admin = isTRUE(comptes$is_admin[[i]]),
			cree_le = admin_temps(comptes$created_at[[i]]),
			n_seances = nrow(d),
			n_reponses = nrow(r),
			taux_reussite = if (nrow(r)) mean(r$juste) else NA_real_,
			n_distinctes = length(unique(graines)),
			seuil_ia = as.integer(seuil_ia),
			derniere = if (length(moments)) max(moments) else NA_real_,
			stringsAsFactors = FALSE
		)
	})

	if (!length(lignes)) {
		return(data.frame(
			username = character(0), is_admin = logical(0), cree_le = numeric(0),
			n_seances = integer(0), n_reponses = integer(0), taux_reussite = numeric(0),
			n_distinctes = integer(0), seuil_ia = integer(0), derniere = numeric(0),
			statut = character(0), stringsAsFactors = FALSE
		))
	}

	tableau <- do.call(rbind, lignes)
	tableau$statut <- admin_statut(tableau$derniere, maintenant)
	# Les plus récemment actifs d'abord, les « jamais venus » à la fin.
	tableau[order(is.na(tableau$derniere), -tableau$derniere, tableau$username), , drop = FALSE]
}

#' Filtrer le tableau des comptes (recherche d'alias et statut)
#'
#' @param tableau Résultat de [admin_comptes_tableau()].
#' @param texte Morceau d'alias cherché.
#' @param statut `"tous"`, `"actif"`, `"inactif"` ou `"jamais"`.
#' @noRd
admin_comptes_filtrer <- function(tableau, texte = "", statut = "tous") {
	texte <- tolower(trimws(as.character(texte %||% "")))
	if (nzchar(texte)) {
		tableau <- tableau[grepl(texte, tableau$username, fixed = TRUE), , drop = FALSE]
	}
	if (!identical(statut %||% "tous", "tous")) {
		tableau <- tableau[tableau$statut == statut, , drop = FALSE]
	}
	tableau
}

#' Activité jour par jour (réponses données et comptes actifs)
#'
#' @param seances Liste de séances.
#' @param jours Nombre de jours affichés, aujourd'hui compris.
#' @param maintenant Instant de référence.
#'
#' @return Un data.frame : `jour` (Date, UTC), `n_reponses`, `n_comptes`.
#' @noRd
admin_activite_jours <- function(seances, jours = 30L, maintenant = Sys.time()) {
	fin <- as.Date(as.POSIXct(as.numeric(maintenant), origin = "1970-01-01", tz = "UTC"))
	tous <- seq(fin - (jours - 1L), fin, by = "day")
	r <- admin_reponses(seances)
	r <- r[!is.na(r$quand), , drop = FALSE]
	j <- as.Date(as.POSIXct(r$quand, origin = "1970-01-01", tz = "UTC"))

	data.frame(
		jour = tous,
		n_reponses = vapply(seq_along(tous), function(i) sum(j == tous[[i]]), integer(1)),
		n_comptes = vapply(seq_along(tous), function(i) length(unique(r$user_id[j == tous[[i]]])), integer(1))
	)
}

#' Chiffres clés de la vue d'ensemble
#'
#' @param comptes Résultat de [admin_comptes_tableau()].
#' @param activite Résultat de [admin_activite_jours()].
#' @param retours Résultat de [retours_tableau()] (ou `NULL`).
#'
#' @return Une liste de nombres.
#' @noRd
admin_chiffres <- function(comptes, activite, retours = NULL) {
	sept <- utils::tail(activite, 7L)
	n_rep <- sum(comptes$n_reponses)
	a_revoir <- if (is.null(retours) || !nrow(retours)) {
		0L
	} else {
		sum(retours$statut == "a_traiter" & retours$n_commentaires > 0L)
	}

	list(
		n_comptes = nrow(comptes),
		n_actifs = sum(comptes$statut == "actif"),
		n_jamais = sum(comptes$statut == "jamais"),
		reponses_7j = sum(sept$n_reponses),
		reponses_total = n_rep,
		reussite = if (n_rep) sum(comptes$taux_reussite * comptes$n_reponses, na.rm = TRUE) / n_rep else NA_real_,
		n_commentaires = if (is.null(retours) || !nrow(retours)) 0L else sum(retours$n_commentaires),
		a_revoir = as.integer(a_revoir)
	)
}

#' Échec moyen par thème (réponses de toutes les questions du thème)
#'
#' @param retours Résultat de [retours_tableau()].
#'
#' @return Un data.frame trié par échec décroissant : `theme`, `n_reponses`,
#'   `taux_echec`.
#' @noRd
admin_themes <- function(retours) {
	if (is.null(retours) || !nrow(retours)) {
		return(data.frame(theme = character(0), n_reponses = integer(0), taux_echec = numeric(0)))
	}

	t <- retours[!is.na(retours$taux_echec) & retours$n_reponses > 0L, , drop = FALSE]
	t$theme[is.na(t$theme) | !nzchar(t$theme)] <- "Autre"

	if (!nrow(t)) {
		return(data.frame(theme = character(0), n_reponses = integer(0), taux_echec = numeric(0)))
	}

	themes <- unique(t$theme)
	sortie <- data.frame(
		theme = themes,
		n_reponses = vapply(themes, function(x) sum(t$n_reponses[t$theme == x]), numeric(1)),
		taux_echec = vapply(themes, function(x) {
			k <- t$theme == x
			sum(t$taux_echec[k] * t$n_reponses[k]) / sum(t$n_reponses[k])
		}, numeric(1)),
		stringsAsFactors = FALSE,
		row.names = NULL
	)
	sortie$n_reponses <- as.integer(sortie$n_reponses)
	sortie[order(-sortie$taux_echec), , drop = FALSE]
}

#' Mot de passe aléatoire, facile à recopier
#'
#' Trois groupes de 4 caractères (« k7Pq-mR2x-Tz9b ») sans caractères
#' ambigus (0/O, 1/l/I). Tirage cryptographique ({sodium}), sans biais.
#'
#' @return Une chaîne de 14 caractères.
#' @noRd
admin_mot_de_passe <- function() {
	alphabet <- strsplit("abcdefghjkmnpqrstuvwxyzABCDEFGHJKMNPQRSTUVWXYZ23456789", "")[[1L]]
	k <- length(alphabet)
	limite <- 256L - (256L %% k)
	tirage <- integer(0)

	while (length(tirage) < 12L) {
		octets <- as.integer(sodium::random(24L))
		tirage <- c(tirage, octets[octets < limite] %% k)
	}

	car <- alphabet[tirage[1:12] + 1L]
	paste(
		paste(car[1:4], collapse = ""),
		paste(car[5:8], collapse = ""),
		paste(car[9:12], collapse = ""),
		sep = "-"
	)
}

#' « aujourd'hui », « hier », « il y a 3 j », « il y a 5 sem. », « aucune séance »
#'
#' @param quand Secondes (NA : jamais).
#' @param maintenant Instant de référence.
#' @noRd
admin_depuis <- function(quand, maintenant = Sys.time()) {
	if (is.na(quand)) {
		return("aucune s\u00e9ance")
	}
	ref <- as.Date(as.POSIXct(as.numeric(maintenant), origin = "1970-01-01", tz = "Europe/Paris"))
	jour <- as.Date(as.POSIXct(quand, origin = "1970-01-01", tz = "Europe/Paris"))
	n <- as.integer(ref - jour)

	if (n <= 0L) return("aujourd'hui")
	if (n == 1L) return("hier")
	if (n < 14L) return(sprintf("il y a %d j", n))
	if (n < 60L) return(sprintf("il y a %d sem.", n %/% 7L))
	sprintf("il y a %d mois", n %/% 30L)
}
