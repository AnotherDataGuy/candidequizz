# Comptes et jetons de connexion.
#
# Le mot de passe n'est jamais stocké. Le navigateur garde un jeton opaque ;
# le serveur n'en conserve que l'empreinte.

#' Durée de vie d'un jeton, en jours
#' @noRd
auth_token_jours <- function() {
	as.integer(cq_config("auth", "token_jours") %||% 30L)
}

#' Mode d'accès à l'application
#'
#' - `local` : version à télécharger. Pas d'écran de connexion ; toutes les
#'   séances appartiennent à un compte local unique, sur l'ordinateur.
#' - `comptes` : version en ligne. Connexion obligatoire, une progression par
#'   compte.
#'
#' La variable d'environnement `CANDIDEQUIZZ_AUTH_MODE` prime sur
#' `auth.mode` de `inst/golem-config.yml`. Clé absente : `comptes`.
#'
#' @return `"local"` ou `"comptes"`.
#' @noRd
auth_mode <- function() {
	force <- Sys.getenv("CANDIDEQUIZZ_AUTH_MODE", "")

	mode <- if (nzchar(force)) {
		force
	} else {
		tryCatch(as.character(cq_config("auth", "mode")), error = function(e) "comptes")
	}

	if (identical(mode, "local")) "local" else "comptes"
}

#' Compte unique du mode local
#' @noRd
auth_utilisateur_local <- function() {
	list(user_id = "local", username = "local", is_admin = FALSE, token = NULL)
}

#' L'inscription publique est-elle ouverte
#' @noRd
auth_inscription_ouverte <- function() {
	identical(as.character(cq_config("auth", "inscription")), "ouverte")
}

#' Normaliser un identifiant de connexion
#' @noRd
auth_username <- function(username) {
	tolower(trimws(as.character(username %||% "")))
}

#' Créer le premier administrateur si la table est vide
#'
#' Lit `CANDIDEQUIZZ_ADMIN_USER` et `CANDIDEQUIZZ_ADMIN_PASSWORD`. Ne fait
#' rien si un compte existe déjà, ou si les variables sont absentes.
#'
#' @noRd
auth_bootstrap <- function() {
	if (!identical(store_backend(), "sqlite")) {
		return(invisible(NULL))
	}

	store_sqlite_con()

	# Ménage : un jeton expiré n'était effacé que si quelqu'un le présentait.
	# Les dates sont stockées en texte « AAAA-MM-JJ HH:MM:SS » (UTC) :
	# l'ordre alphabétique est donc l'ordre chronologique.
	db_execute(
		"DELETE FROM auth_tokens WHERE expires_at < ?",
		params = list(store_sqlite_temps(Sys.time()))
	)

	n <- db_query("SELECT COUNT(*) AS n FROM users")$n[[1L]]

	if (n > 0L) {
		return(invisible(NULL))
	}

	user <- Sys.getenv("CANDIDEQUIZZ_ADMIN_USER", "")
	pass <- Sys.getenv("CANDIDEQUIZZ_ADMIN_PASSWORD", "")

	if (!nzchar(user) || !nzchar(pass)) {
		return(invisible(NULL))
	}

	auth_creer(user, pass, admin = TRUE)
	invisible(NULL)
}

#' Ligne utilisateur, ou NULL
#' @noRd
auth_trouver <- function(username) {
	username <- auth_username(username)

	if (!nzchar(username)) {
		return(NULL)
	}

	ligne <- db_query(
		"SELECT user_id, username, password_hash, created_at, is_admin FROM users WHERE username = ?",
		params = list(username)
	)

	if (!nrow(ligne)) {
		return(NULL)
	}

	list(
		user_id = as.character(ligne$user_id[[1L]]),
		username = as.character(ligne$username[[1L]]),
		password_hash = as.character(ligne$password_hash[[1L]]),
		created_at = as.character(ligne$created_at[[1L]]),
		is_admin = isTRUE(as.integer(ligne$is_admin[[1L]]) == 1L)
	)
}

#' Compte public, sans le mot de passe
#' @noRd
auth_public <- function(user, token = NULL) {
	list(
		user_id = user$user_id,
		username = user$username,
		is_admin = isTRUE(user$is_admin),
		token = token
	)
}

#' Créer un compte
#'
#' @param admin Le premier compte, ou une création par l'administrateur.
#' @noRd
auth_creer <- function(username, password, admin = FALSE, par_admin = FALSE) {
	# `par_admin` : compte ordinaire cr\u00e9\u00e9 par l'administrateur, permis m\u00eame
	# quand les inscriptions publiques sont ferm\u00e9es.
	if (!admin && !isTRUE(par_admin) && !auth_inscription_ouverte()) {
		rlang::abort(
			"Les inscriptions sont ferm\u00e9es.",
			class = "candidequizz_auth_fermee"
		)
	}

	username <- auth_username(username)
	password <- as.character(password %||% "")

	if (!grepl("^[a-z0-9_]{3,40}$", username)) {
		rlang::abort(
			"L'identifiant doit contenir 3 \u00e0 40 caract\u00e8res : lettres, chiffres ou _.",
			class = "candidequizz_auth_invalide"
		)
	}

	if (nchar(password) < 8L) {
		rlang::abort(
			"Le mot de passe doit contenir au moins 8 caract\u00e8res.",
			class = "candidequizz_auth_invalide"
		)
	}

	if (!is.null(auth_trouver(username))) {
		rlang::abort(
			"Cet identifiant est d\u00e9j\u00e0 utilis\u00e9.",
			class = "candidequizz_auth_existe"
		)
	}

	# Al\u00e9a cryptographique : ind\u00e9pendant de la graine de R (set.seed).
	user_id <- paste0("u-", sodium::bin2hex(sodium::random(8L)))

	db_execute(
		paste(
			"INSERT INTO users (user_id, username, password_hash, created_at, is_admin)",
			"VALUES (?, ?, ?, ?, ?)"
		),
		params = list(
			user_id,
			username,
			sodium::password_store(password),
			store_sqlite_temps(Sys.time()),
			if (isTRUE(admin)) 1L else 0L
		)
	)

	auth_public(auth_trouver(username))
}

#' Empreinte d'un jeton
#' @noRd
auth_token_hash <- function(token) {
	sodium::bin2hex(sodium::sha256(charToRaw(as.character(token))))
}

#' Ouvrir un jeton pour un compte
#' @noRd
auth_token_creer <- function(user_id) {
	token <- sodium::bin2hex(sodium::random(24L))
	expires <- store_sqlite_temps(Sys.time() + as.difftime(auth_token_jours(), units = "days"))

	db_execute(
		"INSERT INTO auth_tokens (token_hash, user_id, expires_at) VALUES (?, ?, ?)",
		params = list(auth_token_hash(token), as.character(user_id), expires)
	)

	token
}

#' Retrouver le compte d'un jeton encore valide
#' @noRd
auth_token_resoudre <- function(token) {
	if (is.null(token) || !nzchar(as.character(token))) {
		return(NULL)
	}

	if (!identical(store_backend(), "sqlite")) {
		return(NULL)
	}

	ligne <- db_query(
		paste(
			"SELECT t.user_id, t.expires_at, u.username, u.is_admin",
			"FROM auth_tokens t JOIN users u ON u.user_id = t.user_id",
			"WHERE t.token_hash = ?"
		),
		params = list(auth_token_hash(token))
	)

	if (!nrow(ligne)) {
		return(NULL)
	}

	expires <- as.POSIXct(ligne$expires_at[[1L]], tz = "UTC")

	if (is.na(expires) || expires < Sys.time()) {
		auth_token_revoquer(token)
		return(NULL)
	}

	auth_public(
		list(
			user_id = as.character(ligne$user_id[[1L]]),
			username = as.character(ligne$username[[1L]]),
			is_admin = isTRUE(as.integer(ligne$is_admin[[1L]]) == 1L)
		),
		token = as.character(token)
	)
}

#' Oublier un jeton
#' @noRd
auth_token_revoquer <- function(token) {
	if (is.null(token) || !nzchar(as.character(token))) {
		return(invisible(FALSE))
	}

	db_execute(
		"DELETE FROM auth_tokens WHERE token_hash = ?",
		params = list(auth_token_hash(token))
	)

	invisible(TRUE)
}

# --- Freinage des essais de mot de passe ------------------------------------
#
# Les échecs sont comptés par identifiant, pour tout le processus R : recharger
# la page ne remet pas le compteur à zéro. Aucun `Sys.sleep()` : une pause
# figerait l'application pour tous les utilisateurs connectés, pas seulement
# pour celui qui se trompe. Le compteur repart de zéro au redémarrage de
# l'application.

.auth_echecs <- new.env(parent = emptyenv())

#' Un identifiant est-il temporairement bloqué
#'
#' @param username Identifiant saisi.
#' @param max Nombre d'échecs tolérés avant blocage.
#' @param delai_s Durée du blocage après le dernier échec, en secondes.
#'
#' @return `TRUE` si les essais doivent être refusés pour l'instant.
#' @noRd
auth_bloque <- function(username, max = 5L, delai_s = 60) {
	cle <- auth_username(username)

	if (!nzchar(cle)) {
		return(FALSE)
	}

	e <- .auth_echecs[[cle]]

	!is.null(e) &&
		e$n >= max &&
		as.numeric(difftime(Sys.time(), e$dernier, units = "secs")) < delai_s
}

#' Compter un échec de connexion pour un identifiant
#' @noRd
auth_noter_echec <- function(username) {
	cle <- auth_username(username)

	if (!nzchar(cle)) {
		return(invisible(NULL))
	}

	n <- (.auth_echecs[[cle]]$n %||% 0L) + 1L
	assign(cle, list(n = n, dernier = Sys.time()), envir = .auth_echecs)
	invisible(n)
}

#' Oublier les échecs d'un identifiant (connexion réussie)
#' @noRd
auth_oublier_echecs <- function(username) {
	cle <- auth_username(username)

	if (nzchar(cle) && exists(cle, envir = .auth_echecs, inherits = FALSE)) {
		rm(list = cle, envir = .auth_echecs)
	}

	invisible(NULL)
}

#' Connecter un compte et ouvrir un jeton
#' @noRd
auth_connecter <- function(username, password) {
	user <- auth_trouver(username)
	ok <- FALSE

	if (!is.null(user)) {
		ok <- isTRUE(tryCatch(
			sodium::password_verify(user$password_hash, as.character(password %||% "")),
			error = function(e) FALSE
		))
	}

	if (!ok) {
		rlang::abort(
			"Identifiant ou mot de passe incorrect.",
			class = "candidequizz_auth_invalide"
		)
	}

	auth_public(user, token = auth_token_creer(user$user_id))
}

#' Changer le mot de passe d'un compte existant
#' @noRd
auth_reinitialiser <- function(username, password) {
	user <- auth_trouver(username)

	if (is.null(user)) {
		rlang::abort(
			"Compte introuvable.",
			class = "candidequizz_auth_invalide"
		)
	}

	password <- as.character(password %||% "")

	if (nchar(password) < 8L) {
		rlang::abort(
			"Le mot de passe doit contenir au moins 8 caract\u00e8res.",
			class = "candidequizz_auth_invalide"
		)
	}

	db_execute(
		"UPDATE users SET password_hash = ? WHERE user_id = ?",
		params = list(sodium::password_store(password), user$user_id)
	)

	# Déconnecter le compte partout : un jeton volé ne survit pas à la
	# réinitialisation du mot de passe.
	db_execute(
		"DELETE FROM auth_tokens WHERE user_id = ?",
		params = list(user$user_id)
	)

	invisible(TRUE)
}

#' Supprimer un compte et toutes ses données
#'
#' Pour l'administrateur (version en ligne) : efface le compte, ses jetons de
#' connexion, ses séances (réponses et avis compris) et ses signalements.
#' Irréversible. Un compte administrateur ne peut pas être supprimé ici, ce
#' qui évite de se fermer la porte.
#'
#' @param username Identifiant du compte à supprimer.
#'
#' @return Le nombre de séances effacées, invisiblement.
#' @noRd
auth_supprimer <- function(username) {
	user <- auth_trouver(username)

	if (is.null(user)) {
		rlang::abort("Compte introuvable.", class = "candidequizz_auth_invalide")
	}

	if (isTRUE(user$is_admin)) {
		rlang::abort(
			"Un compte administrateur ne peut pas \u00eatre supprim\u00e9 depuis l'application.",
			class = "candidequizz_auth_invalide"
		)
	}

	n <- db_query("SELECT COUNT(*) AS n FROM seances WHERE user_id = ?", params = list(user$user_id))$n[[1L]]

	for (table in c("auth_tokens", "seances", "signalements", "users")) {
		db_execute(paste("DELETE FROM", table, "WHERE user_id = ?"), params = list(user$user_id))
	}

	invisible(as.integer(n))
}

#' Liste des comptes, sans secret
#' @noRd
auth_lister <- function() {
	tableau <- db_query(
		"SELECT username, is_admin, created_at FROM users ORDER BY created_at"
	)

	if (!nrow(tableau)) {
		return(tibble::tibble(
			username = character(0),
			is_admin = logical(0),
			created_at = character(0)
		))
	}

	tibble::tibble(
		username = as.character(tableau$username),
		is_admin = as.integer(tableau$is_admin) == 1L,
		created_at = as.character(tableau$created_at)
	)
}
