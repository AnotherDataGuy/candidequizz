# Retours sur l'application : « Un problème avec l'appli ? Une suggestion ? »
#
# Bouton de l'en-tête, version en ligne seulement (mode « comptes ») : la
# version à télécharger n'a personne pour lire les retours. Les retours vont
# dans la table `retours_appli` ; l'administrateur les lit dans la section
# Administration, onglet « Retours appli ». Interface : mod_retour_appli.R.

#' Genres de retour
#' @noRd
retour_appli_genres <- function() {
	c(probleme = "Quelque chose cloche", suggestion = "J'ai une id\u00e9e")
}

#' Parties de l'application proposées dans la liste
#'
#' @return Vecteur nommé `code -> libellé`, « Autre » en dernier.
#' @noRd
retour_appli_parties <- function() {
	c(
		accueil = "Accueil",
		composer = "Pr\u00e9parer une s\u00e9ance",
		questions = "Pendant les questions",
		corrections = "Corrections et explications",
		avis = "Vos avis sur les questions",
		bilan = "Bilan de fin de s\u00e9ance",
		progression = "Carnet de progression",
		ressources = "Ressources",
		ia = "Questions r\u00e9dig\u00e9es par l'IA",
		compte = "Connexion et compte",
		autre = "Autre"
	)
}

#' Partie proposée d'office, d'après la section affichée
#'
#' @param section Section courante (`accueil`, `seances`...).
#' @return Un code de [retour_appli_parties()], ou `NULL`.
#' @noRd
retour_appli_partie_depuis <- function(section) {
	switch(
		as.character(section %||% ""),
		accueil = "accueil",
		seances = "composer",
		statistiques = "progression",
		ressources = "ressources",
		NULL
	)
}

#' Longueur maximale du texte d'un retour
#' @noRd
retour_appli_max <- function() 2000L

#' Retours au plus par compte et par jour (contre les abus)
#' @noRd
retour_appli_max_jour <- function() 10L

#' Vérifier un retour avant enregistrement
#'
#' @return La liste nettoyée (`genre`, `partie`, `partie_autre`, `texte`).
#'   Erreur de classe `candidequizz_retour_appli_invalide`, au message
#'   affichable, sinon.
#' @noRd
retour_appli_valider <- function(genre, partie, partie_autre = NULL, texte = NULL) {
	refuser <- function(message) {
		rlang::abort(message, class = "candidequizz_retour_appli_invalide")
	}
	un <- function(x) {
		x <- as.character(x %||% "")
		if (length(x) != 1L || is.na(x)) "" else trimws(x)
	}

	genre <- un(genre)
	partie <- un(partie)
	partie_autre <- un(partie_autre)
	texte <- un(texte)

	if (!genre %in% names(retour_appli_genres())) {
		refuser("Dites-nous d'abord s'il s'agit d'un probl\u00e8me ou d'une id\u00e9e.")
	}
	if (!partie %in% names(retour_appli_parties())) {
		refuser("Choisissez la partie de l'application concern\u00e9e.")
	}
	if (identical(partie, "autre") && nchar(partie_autre) < 2L) {
		refuser("Pr\u00e9cisez o\u00f9, dans l'application.")
	}
	if (nchar(texte) < 10L) {
		refuser("Quelques mots de plus, s'il vous pla\u00eet (10 caract\u00e8res au moins).")
	}
	if (nchar(texte) > retour_appli_max()) {
		refuser(sprintf("Le texte d\u00e9passe %d caract\u00e8res.", retour_appli_max()))
	}

	list(
		genre = genre,
		partie = partie,
		partie_autre = if (identical(partie, "autre")) substr(partie_autre, 1L, 120L) else NA_character_,
		texte = texte
	)
}

#' Contexte technique joint au retour (écran, navigateur, thème, section)
#'
#' Seulement des informations utiles pour reproduire un problème, toutes
#' affichées à la personne avant l'envoi. Rien qui l'identifie au-delà de
#' son alias.
#'
#' @param brut Liste envoyée par le navigateur (largeur, hauteur, navigateur,
#'   thème) ; peut être `NULL`.
#' @param section Section affichée à l'ouverture.
#' @return Une chaîne JSON.
#' @noRd
retour_appli_contexte <- function(brut = NULL, section = NULL) {
	brut <- if (is.list(brut)) brut else list()
	coupe <- function(x, n) substr(as.character(x %||% "")[[1L]], 1L, n)

	jsonlite::toJSON(
		list(
			section = coupe(section, 30L),
			ecran = coupe(brut$ecran, 20L),
			navigateur = coupe(brut$navigateur, 60L),
			theme = coupe(brut$theme, 10L)
		),
		auto_unbox = TRUE
	)
}

#' Résumé lisible du navigateur (« Chrome · Android »)
#'
#' @param ua Chaîne User-Agent.
#' @noRd
retour_appli_navigateur <- function(ua) {
	ua <- as.character(ua %||% "")
	nav <- if (grepl("Edg/", ua)) "Edge" else if (grepl("Firefox/", ua)) "Firefox" else
		if (grepl("Chrome/|CriOS/", ua)) "Chrome" else if (grepl("Safari/", ua)) "Safari" else "Autre navigateur"
	sys <- if (grepl("Android", ua)) "Android" else if (grepl("iPhone|iPad", ua)) "iOS" else
		if (grepl("Windows", ua)) "Windows" else if (grepl("Mac OS", ua)) "macOS" else
		if (grepl("Linux", ua)) "Linux" else "syst\u00e8me inconnu"
	paste(nav, sys, sep = " \u00b7 ")
}

#' Enregistrer un retour
#'
#' @param user_id Compte connecté.
#' @param retour Résultat de [retour_appli_valider()].
#' @param contexte Résultat de [retour_appli_contexte()].
#' @param maintenant Instant de l'envoi.
#'
#' @return Le numéro du retour (entier), invisiblement.
#' @noRd
retour_appli_enregistrer <- function(user_id, retour, contexte = "{}", maintenant = Sys.time()) {
	if (!identical(store_backend(), "sqlite")) {
		rlang::abort(
			"Les retours demandent la base de la version en ligne.",
			class = "candidequizz_retour_appli_invalide"
		)
	}
	user_id <- as.character(user_id %||% "")
	existe <- nzchar(user_id) && nrow(db_query(
		"SELECT user_id FROM users WHERE user_id = ?", params = list(user_id)
	)) > 0L
	if (!existe) {
		rlang::abort("Compte inconnu.", class = "candidequizz_retour_appli_invalide")
	}

	depuis <- store_sqlite_temps(maintenant - 86400)
	n <- db_query(
		"SELECT COUNT(*) AS n FROM retours_appli WHERE user_id = ? AND cree_le >= ?",
		params = list(user_id, depuis)
	)$n[[1L]]
	if (n >= retour_appli_max_jour()) {
		rlang::abort(
			"Beaucoup de retours aujourd'hui : merci ! R\u00e9essayez demain.",
			class = "candidequizz_retour_appli_invalide"
		)
	}

	quand <- store_sqlite_temps(maintenant)
	db_execute(
		paste(
			"INSERT INTO retours_appli (user_id, cree_le, genre, partie, partie_autre, texte, contexte, statut)",
			"VALUES (?, ?, ?, ?, ?, ?, ?, 'nouveau')"
		),
		params = list(
			user_id, quand, retour$genre, retour$partie,
			retour$partie_autre, retour$texte, as.character(contexte)
		)
	)

	id <- db_query(
		"SELECT MAX(id) AS id FROM retours_appli WHERE user_id = ? AND cree_le = ?",
		params = list(user_id, quand)
	)$id[[1L]]

	invisible(as.integer(id))
}

#' Retours enregistrés, les plus récents d'abord (administration)
#'
#' @return Un data.frame avec l'alias de l'auteur (`username`).
#' @noRd
retour_appli_lister <- function() {
	vide <- retour_appli_lister_vide()
	if (!identical(store_backend(), "sqlite")) {
		return(vide)
	}
	retour_appli_lister_base(vide)
}

#' Tableau vide des retours (mêmes colonnes)
#' @noRd
retour_appli_lister_vide <- function() {
	data.frame(
		id = integer(0), username = character(0), cree_le = character(0),
		genre = character(0), partie = character(0), partie_autre = character(0),
		texte = character(0), contexte = character(0), statut = character(0),
		stringsAsFactors = FALSE
	)
}

#' Lecture des retours en base
#' @noRd
retour_appli_lister_base <- function(vide) {
	lignes <- db_query(paste(
		"SELECT r.id, u.username, r.cree_le, r.genre, r.partie, r.partie_autre,",
		"r.texte, r.contexte, r.statut",
		"FROM retours_appli r LEFT JOIN users u ON u.user_id = r.user_id",
		"ORDER BY r.cree_le DESC, r.id DESC"
	))
	if (!nrow(lignes)) {
		return(vide)
	}

	data.frame(
		id = as.integer(lignes$id),
		username = ifelse(is.na(lignes$username), "compte supprim\u00e9", as.character(lignes$username)),
		cree_le = as.character(lignes$cree_le),
		genre = as.character(lignes$genre),
		partie = as.character(lignes$partie),
		partie_autre = as.character(lignes$partie_autre),
		texte = as.character(lignes$texte),
		contexte = as.character(lignes$contexte),
		statut = as.character(lignes$statut),
		stringsAsFactors = FALSE
	)
}

#' Changer le statut d'un retour (`nouveau` ou `traite`)
#' @noRd
retour_appli_marquer <- function(id, statut = c("traite", "nouveau")) {
	statut <- match.arg(statut)
	id <- suppressWarnings(as.integer(id %||% NA))
	if (length(id) != 1L || is.na(id)) {
		rlang::abort("Retour inconnu.", class = "candidequizz_retour_appli_invalide")
	}
	db_execute("UPDATE retours_appli SET statut = ? WHERE id = ?", params = list(statut, id))
	invisible(TRUE)
}
