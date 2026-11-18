# Garde-fous contre l'usage abusif : rafales de séances, dépenses d'IA,
# réponses données sans lire pour débloquer l'IA.
#
# Réglages : section `anti_abus` de golem-config.yml. Actif seulement dans la
# version en ligne (mode « comptes ») : la version à télécharger n'a qu'un
# utilisateur, qui paie l'IA avec sa propre clé. L'administrateur n'est
# jamais bridé, sauf par le budget global du jour. Les compteurs viennent de la base (table `seances`), pas de la
# mémoire du processus : ils valent pour tous les onglets d'un compte et
# survivent à un redémarrage. Les heures sont en UTC, comme en base.
#
# Ce qui est protégé, et où c'est branché :
# - rythme des séances par compte : anti_abus_seance_autorisee(), appelé par
#   mod_seances.R avant toute préparation ;
# - une seule préparation à la fois par compte : anti_abus_verrouiller() ;
# - quota d'IA par compte et budget d'IA global par jour :
#   anti_abus_ia_autorisee(), la séance démarre alors sans IA ;
# - réponses trop rapides : anti_abus_reponse_comptee(), utilisé par
#   ia_etat() (fct_progression.R) pour le déblocage de l'IA.

#' Les garde-fous sont-ils actifs ?
#'
#' Oui en mode « comptes » avec la base SQL ; non en mode local (un seul
#' utilisateur) ni avec le stockage en fichiers .rds (tests).
#' @noRd
anti_abus_actif <- function() {
	identical(auth_mode(), "comptes") && identical(store_backend(), "sqlite")
}

#' Réglage de la section `anti_abus`
#' @noRd
anti_abus_cfg <- function(cle) {
	cq_config("anti_abus", cle)
}

#' Début d'une fenêtre de temps, au format stocké en base (UTC)
#' @noRd
anti_abus_depuis <- function(secondes = NULL, jour = FALSE) {
	if (isTRUE(jour)) {
		return(paste(format(Sys.time(), "%Y-%m-%d", tz = "UTC"), "00:00:00"))
	}
	store_sqlite_temps(Sys.time() - secondes)
}

#' Séances lisibles créées depuis un instant, pour un compte ou pour tous
#'
#' @param depuis Horodatage UTC « AAAA-MM-JJ HH:MM:SS ».
#' @param user_id Compte, ou `NULL` pour tous les comptes.
#'
#' @return Une liste de séances.
#' @noRd
anti_abus_seances_depuis <- function(depuis, user_id = NULL) {
	if (!identical(store_backend(), "sqlite")) {
		return(list())
	}


	lignes <- if (is.null(user_id)) {
		db_query("SELECT blob FROM seances WHERE cree_le >= ?", params = list(depuis))
	} else {
		db_query(
			"SELECT blob FROM seances WHERE user_id = ? AND cree_le >= ?",
			params = list(as.character(user_id), depuis)
		)
	}

	seances <- lapply(lignes$blob, store_sqlite_unserialize)
	seances[!vapply(seances, is.null, logical(1))]
}

#' Le compte peut-il démarrer une séance maintenant ?
#'
#' Au plus `seances_par_heure` séances sur la dernière heure, et au moins
#' `delai_entre_seances_s` secondes depuis la précédente.
#'
#' @param user_id Compte.
#' @param admin L'administrateur n'est pas limité.
#'
#' @return Une liste : `ok`, et `message` à afficher quand `ok` est faux.
#' @noRd
anti_abus_seance_autorisee <- function(user_id, admin = FALSE) {
	if (isTRUE(admin) || !anti_abus_actif() || !store_sqlite_user_ok(user_id)) {
		return(list(ok = TRUE))
	}

	max_heure <- as.integer(anti_abus_cfg("seances_par_heure"))
	delai <- as.numeric(anti_abus_cfg("delai_entre_seances_s"))

	ligne <- db_query(
		"SELECT COUNT(*) AS n, MAX(cree_le) AS derniere FROM seances WHERE user_id = ? AND cree_le >= ?",
		params = list(as.character(user_id), anti_abus_depuis(3600))
	)
	n <- as.integer(ligne$n[[1L]])
	derniere <- ligne$derniere[[1L]]

	if (n >= max_heure) {
		return(list(
			ok = FALSE,
			message = sprintf(
				"Vous avez d\u00e9j\u00e0 lanc\u00e9 %d s\u00e9ances dans l'heure. Faites une pause, puis revenez : la limite se lib\u00e8re au fil de l'heure.",
				n
			)
		))
	}

	if (!is.na(derniere)) {
		ecoule <- as.numeric(difftime(Sys.time(), as.POSIXct(derniere, tz = "UTC"), units = "secs"))
		if (ecoule < delai) {
			return(list(
				ok = FALSE,
				message = sprintf(
					"Une s\u00e9ance vient d'\u00eatre lanc\u00e9e. Patientez %d secondes avant d'en commencer une autre.",
					as.integer(ceiling(delai - ecoule))
				)
			))
		}
	}

	list(ok = TRUE)
}

#' L'IA peut-elle servir pour une nouvelle séance de ce compte ?
#'
#' Deux plafonds, remis à zéro chaque jour à minuit UTC :
#' `ia_seances_par_jour` séances avec IA par compte, et `ia_budget_jour_usd`
#' dollars dépensés par toute l'application (coût enregistré à chaque appel,
#' voir llm_synthese_journal()).
#'
#' @param user_id Compte.
#' @param admin L'administrateur n'est pas limité par compte, mais le budget
#'   global vaut pour tous.
#'
#' @return Une liste : `ok`, et `message` quand `ok` est faux.
#' @noRd
anti_abus_ia_autorisee <- function(user_id, admin = FALSE) {
	if (!anti_abus_actif()) {
		return(list(ok = TRUE))
	}

	aujourdhui <- anti_abus_depuis(jour = TRUE)
	toutes <- anti_abus_seances_depuis(aujourdhui)

	budget <- as.numeric(anti_abus_cfg("ia_budget_jour_usd"))
	depense <- sum(vapply(toutes, function(s) {
		as.numeric(stats_cout_seance(s)$cout_usd[[1L]] %||% 0)
	}, numeric(1)), na.rm = TRUE)

	if (depense >= budget) {
		return(list(
			ok = FALSE,
			message = "L'IA est en pause jusqu'\u00e0 demain : le budget du jour est atteint. Votre s\u00e9ance d\u00e9marre avec les questions de la banque."
		))
	}

	if (!isTRUE(admin)) {
		max_jour <- as.integer(anti_abus_cfg("ia_seances_par_jour"))
		avec_ia <- Filter(function(s) {
			identical(as.character(s$user_id), as.character(user_id)) &&
				!identical(s$parametres$generation, "ia_off")
		}, toutes)

		if (length(avec_ia) >= max_jour) {
			return(list(
				ok = FALSE,
				message = sprintf(
					"Vous avez fait %d s\u00e9ances avec IA aujourd'hui, le maximum par jour. Celle-ci d\u00e9marre avec les questions de la banque ; l'IA revient demain.",
					length(avec_ia)
				)
			))
		}
	}

	list(ok = TRUE)
}

#' Une réponse compte-t-elle pour débloquer l'IA ?
#'
#' Une réponse donnée en moins de `temps_min_reponse_s` secondes n'a pas pu
#' être lue : elle reste dans les statistiques, mais ne fait pas avancer le
#' déblocage. La durée est mesurée par le serveur (reponse_construire()).
#'
#' @param reponse Une réponse enregistrée.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
anti_abus_reponse_comptee <- function(reponse) {
	if (!anti_abus_actif()) {
		return(TRUE)
	}
	duree_s <- as.numeric(reponse$duree_ms %||% NA_real_) / 1000
	# Durée inconnue (anciennes séances) : comptée, faute de preuve contraire.
	is.na(duree_s) || duree_s >= as.numeric(anti_abus_cfg("temps_min_reponse_s"))
}

# --- Une seule préparation de séance à la fois par compte -------------------
#
# Le verrou anti double-clic de mod_seances.R ne vaut que pour un onglet. Ce
# registre, partagé par tout le processus R, empêche deux onglets du même
# compte de préparer deux séances en même temps.

.anti_abus_en_cours <- new.env(parent = emptyenv())

#' Prendre le verrou de préparation d'un compte
#'
#' @return `TRUE` si le verrou est pris, `FALSE` s'il l'était déjà.
#' @noRd
anti_abus_verrouiller <- function(user_id) {
	cle <- as.character(user_id %||% "")
	if (!nzchar(cle)) {
		return(TRUE)
	}
	if (exists(cle, envir = .anti_abus_en_cours, inherits = FALSE)) {
		return(FALSE)
	}
	assign(cle, Sys.time(), envir = .anti_abus_en_cours)
	TRUE
}

#' Rendre le verrou de préparation d'un compte
#' @noRd
anti_abus_liberer <- function(user_id) {
	cle <- as.character(user_id %||% "")
	if (nzchar(cle) && exists(cle, envir = .anti_abus_en_cours, inherits = FALSE)) {
		rm(list = cle, envir = .anti_abus_en_cours)
	}
	invisible(NULL)
}
