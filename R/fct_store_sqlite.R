# Séances dans un fichier SQLite.
#
# Un seul fichier, une ligne par séance, le blob est la séance R sérialisée.
# Le processus principal est le seul écrivain : les travailleurs {future}
# ne doivent pas ouvrir cette connexion.

.store_sqlite <- new.env(parent = emptyenv())

#' Backend de persistance actif
#'
#' La variable d'environnement prime sur le fichier de configuration, pour
#' que les tests restent sur les fichiers `.rds` sans toucher au SQLite.
#'
#' @return `"rds"` ou `"sqlite"`.
#' @noRd
store_backend <- function() {
	force <- Sys.getenv("CANDIDEQUIZZ_STORE_BACKEND", "")

	if (nzchar(force)) {
		return(force)
	}

	tryCatch(
		as.character(cq_config("store", "backend")),
		error = function(e) "rds"
	)
}

#' Chemin du fichier SQLite
#' @noRd
store_sqlite_chemin <- function() {
	file.path(store_dir(creer = TRUE), "candidequizz.sqlite")
}

#' Horodatage stocké dans les colonnes (le blob garde le POSIXct)
#' @noRd
store_sqlite_temps <- function(valeur) {
	if (is.null(valeur) || length(valeur) != 1L || is.na(valeur)) {
		return(NA_character_)
	}

	format(as.POSIXct(valeur), "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Moteur SQL utilisé par le backend `sqlite`
#'
#' Le backend `sqlite` désigne « une base SQL » : un fichier SQLite local par
#' défaut, ou une base PostgreSQL distante si `CANDIDEQUIZZ_DATABASE_URL` est
#' définie (version en ligne : le disque de l'hébergeur peut être effacé à
#' chaque redémarrage). Les requêtes sont écrites une seule fois, avec des `?`,
#' et `store_sql()` les adapte à PostgreSQL.
#'
#' @return `"postgres"` ou `"sqlite"`.
#' @noRd
store_sql_moteur <- function() {
	if (nzchar(Sys.getenv("CANDIDEQUIZZ_DATABASE_URL", ""))) "postgres" else "sqlite"
}

#' Adapter les paramètres `?` à PostgreSQL (`$1`, `$2`…)
#'
#' Aucune requête de l'application ne contient de `?` littéral.
#' @noRd
store_sql <- function(sql) {
	if (!identical(store_sql_moteur(), "postgres")) {
		return(sql)
	}

	n <- lengths(regmatches(sql, gregexpr("?", sql, fixed = TRUE)))

	for (k in seq_len(n)) {
		sql <- sub("?", paste0("$", k), sql, fixed = TRUE)
	}

	sql
}

#' Exécuter une requête de lecture ou d'écriture
#'
#' Une base distante peut fermer une connexion restée inactive : en cas
#' d'erreur, on rouvre la connexion et on réessaie une fois.
#'
#' @noRd
db_appel <- function(f, sql, params = NULL) {
	essayer <- function() {
		if (is.null(params)) {
			f(store_sqlite_con(), store_sql(sql))
		} else {
			f(store_sqlite_con(), store_sql(sql), params = params)
		}
	}

	if (!identical(store_sql_moteur(), "postgres")) {
		return(essayer())
	}

	tryCatch(essayer(), error = function(e) {
		try(DBI::dbDisconnect(.store_sqlite$con), silent = TRUE)
		.store_sqlite$con <- NULL
		essayer()
	})
}

#' @noRd
db_query <- function(sql, params = NULL) db_appel(DBI::dbGetQuery, sql, params)

#' @noRd
db_execute <- function(sql, params = NULL) db_appel(DBI::dbExecute, sql, params)

#' Connexion unique, rouverte si le répertoire de store change
#' @noRd
store_sqlite_con <- function() {
	if (identical(store_sql_moteur(), "postgres")) {
		return(store_postgres_con())
	}

	chemin <- store_sqlite_chemin()
	actuel <- .store_sqlite$chemin %||% ""
	con <- .store_sqlite$con

	if (!identical(actuel, chemin) || is.null(con) || !DBI::dbIsValid(con)) {
		if (!is.null(con)) {
			try(DBI::dbDisconnect(con), silent = TRUE)
		}

		con <- DBI::dbConnect(RSQLite::SQLite(), chemin)
		DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
		DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
		.store_sqlite$con <- con
		.store_sqlite$chemin <- chemin
		store_sqlite_schema(con)
	}

	.store_sqlite$con
}

#' Connexion PostgreSQL unique (version en ligne)
#'
#' `CANDIDEQUIZZ_DATABASE_URL` est une adresse de connexion standard, par ex.
#' `postgresql://utilisateur:motdepasse@hote:5432/base?sslmode=require`.
#' Elle se range dans les variables secrètes de l'hébergeur, jamais dans le code.
#'
#' @noRd
store_postgres_con <- function() {
	url <- Sys.getenv("CANDIDEQUIZZ_DATABASE_URL", "")
	con <- .store_sqlite$con

	if (identical(.store_sqlite$chemin, url) && !is.null(con) && DBI::dbIsValid(con)) {
		return(con)
	}

	if (!requireNamespace("RPostgres", quietly = TRUE)) {
		rlang::abort(
			c(
				"CANDIDEQUIZZ_DATABASE_URL est d\u00e9finie, mais le paquet {RPostgres} n'est pas install\u00e9.",
				i = "install.packages(\"RPostgres\")"
			),
			class = "candidequizz_store_postgres_absent"
		)
	}

	if (!is.null(con)) {
		try(DBI::dbDisconnect(con), silent = TRUE)
	}

	# bigint = "integer" : COUNT(*) revient en entier R, pas en integer64.
	con <- do.call(
		DBI::dbConnect,
		c(list(RPostgres::Postgres(), bigint = "integer"), store_postgres_url(url))
	)
	.store_sqlite$con <- con
	.store_sqlite$chemin <- url
	# Taire les avis « relation already exists » de CREATE TABLE IF NOT EXISTS.
	DBI::dbExecute(con, "SET client_min_messages TO WARNING")
	store_sqlite_schema(con)

	con
}

#' Découper une adresse `postgresql://utilisateur:motdepasse@hote:port/base?sslmode=require`
#'
#' @return Liste d'arguments pour `DBI::dbConnect()` (host, port, user,
#'   password, dbname, sslmode…). Les éléments absents sont omis.
#' @noRd
store_postgres_url <- function(url) {
	motif <- "^postgres(ql)?://(([^:@/]*)(:([^@/]*))?@)?([^:/?]+)(:([0-9]+))?(/([^?]*))?(\\?(.*))?$"

	if (!grepl(motif, url)) {
		rlang::abort(
			"CANDIDEQUIZZ_DATABASE_URL doit avoir la forme postgresql://utilisateur:motdepasse@hote:port/base",
			class = "candidequizz_store_postgres_url"
		)
	}

	m <- regmatches(url, regexec(motif, url))[[1L]]
	args <- list(
		user = utils::URLdecode(m[[4L]]),
		password = utils::URLdecode(m[[6L]]),
		host = m[[7L]],
		port = m[[9L]],
		dbname = m[[11L]]
	)

	if (nzchar(m[[13L]])) {
		for (paire in strsplit(m[[13L]], "&", fixed = TRUE)[[1L]]) {
			kv <- strsplit(paire, "=", fixed = TRUE)[[1L]]
			if (length(kv) == 2L) args[[kv[[1L]]]] <- utils::URLdecode(kv[[2L]])
		}
	}

	args[nzchar(unlist(args))]
}

#' Créer les tables si elles manquent
#'
#' Seuls deux types diffèrent entre SQLite et PostgreSQL : le binaire de la
#' séance et l'identifiant auto-incrémenté des signalements.
#' @noRd
store_sqlite_schema <- function(con) {
	postgres <- inherits(con, "PqConnection")
	type_blob <- if (postgres) "BYTEA" else "BLOB"
	id_auto <- if (postgres) "id BIGSERIAL PRIMARY KEY," else "id INTEGER PRIMARY KEY AUTOINCREMENT,"

	DBI::dbExecute(con, paste(
		"CREATE TABLE IF NOT EXISTS users (",
		"user_id TEXT PRIMARY KEY,",
		"username TEXT UNIQUE NOT NULL,",
		"password_hash TEXT NOT NULL,",
		"created_at TEXT NOT NULL,",
		"is_admin INTEGER NOT NULL DEFAULT 0",
		")"
	))
	DBI::dbExecute(con, paste(
		"CREATE TABLE IF NOT EXISTS auth_tokens (",
		"token_hash TEXT PRIMARY KEY,",
		"user_id TEXT NOT NULL,",
		"expires_at TEXT NOT NULL",
		")"
	))
	DBI::dbExecute(con, paste(
		"CREATE TABLE IF NOT EXISTS seances (",
		"session_id TEXT PRIMARY KEY,",
		"user_id TEXT NOT NULL,",
		"statut TEXT,",
		"cree_le TEXT,",
		"termine_le TEXT,",
		"libelle TEXT,",
		paste("blob", type_blob, "NOT NULL"),
		")"
	))
	DBI::dbExecute(con, paste(
		"CREATE TABLE IF NOT EXISTS signalements (",
		id_auto,
		"user_id TEXT,",
		"session_id TEXT,",
		"item_id TEXT,",
		"motif TEXT,",
		"cree_le TEXT",
		")"
	))

	# Retours des participants : statut « traité » par graine (fct_retours.R).
	DBI::dbExecute(con, paste(
		"CREATE TABLE IF NOT EXISTS retours_statut (",
		"seed_id TEXT PRIMARY KEY,",
		"statut TEXT NOT NULL,",
		"maj_le TEXT",
		")"
	))

	# Index des recherches par compte (listes de séances, progression,
	# révocation des jetons). Même syntaxe en SQLite et en PostgreSQL ;
	# sans effet si l'index existe déjà.
	DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS seances_user ON seances (user_id, cree_le)")
	DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS auth_tokens_user ON auth_tokens (user_id)")

	invisible(con)
}

#' Forcer l'écriture du WAL dans le fichier principal
#'
#' Utile avant de télécharger une copie : sinon le fichier copié peut être
#' incomplet.
#'
#' @noRd
store_sqlite_checkpoint <- function() {
	if (identical(store_sql_moteur(), "postgres")) {
		return(invisible(NULL))
	}

	db_execute("PRAGMA wal_checkpoint(FULL)")
	invisible(store_sqlite_chemin())
}

#' Un identifiant de compte est-il utilisable
#' @noRd
store_sqlite_user_ok <- function(user_id) {
	!is.null(user_id) && length(user_id) == 1L && nzchar(as.character(user_id))
}

#' Refuser une écriture sans compte
#' @noRd
store_sqlite_exiger_user <- function(user_id) {
	if (store_sqlite_user_ok(user_id)) {
		return(as.character(user_id))
	}

	rlang::abort(
		"Aucune session de compte : impossible d'enregistrer cette s\u00e9ance.",
		class = "candidequizz_store_user_absent"
	)
}

#' Relire un blob de séance
#' @noRd
store_sqlite_unserialize <- function(blob) {
	if (is.null(blob) || !length(blob)) {
		return(NULL)
	}

	brut <- if (is.list(blob)) blob[[1L]] else blob

	if (is.null(brut) || !length(brut)) {
		return(NULL)
	}

	tryCatch(unserialize(as.raw(brut)), error = function(e) NULL)
}

#' Enregistrer une séance pour un compte
#' @noRd
store_sqlite_sauver <- function(seance, user_id = NULL) {
	user_id <- store_sqlite_exiger_user(user_id %||% seance$user_id)
	seance$user_id <- user_id
	# ON CONFLICT : même syntaxe en SQLite et en PostgreSQL. La clause WHERE
	# empêche qu'une séance soit réécrite sous un autre compte.
	db_execute(
		paste(
			"INSERT INTO seances",
			"(session_id, user_id, statut, cree_le, termine_le, libelle, blob)",
			"VALUES (?, ?, ?, ?, ?, ?, ?)",
			"ON CONFLICT (session_id) DO UPDATE SET",
			"statut = excluded.statut, cree_le = excluded.cree_le,",
			"termine_le = excluded.termine_le, libelle = excluded.libelle, blob = excluded.blob",
			"WHERE seances.user_id = excluded.user_id"
		),
		params = list(
			as.character(seance$session_id),
			user_id,
			as.character(seance$statut %||% "en_cours"),
			store_sqlite_temps(seance$cree_le),
			store_sqlite_temps(seance$termine_le),
			as.character(seance$libelle %||% seance$session_id),
			# blob::blob : format binaire accepté par RSQLite comme par RPostgres.
			blob::blob(serialize(seance, NULL))
		)
	)

	invisible(store_sqlite_chemin())
}

#' Charger une séance si elle appartient au compte
#' @noRd
store_sqlite_charger <- function(session_id, user_id = NULL) {
	user_id <- store_sqlite_exiger_user(user_id)
	ligne <- db_query(
		"SELECT user_id, blob FROM seances WHERE session_id = ?",
		params = list(as.character(session_id))
	)

	if (!nrow(ligne) || !identical(as.character(ligne$user_id[[1L]]), user_id)) {
		rlang::abort(
			glue::glue("Aucune s\u00e9ance enregistr\u00e9e sous l'identifiant `{session_id}`."),
			class = "candidequizz_store_seance_absente"
		)
	}

	seance <- store_sqlite_unserialize(ligne$blob)

	if (is.null(seance)) {
		rlang::abort(
			"La s\u00e9ance enregistr\u00e9e est illisible.",
			class = "candidequizz_store_seance_absente"
		)
	}

	seance$user_id <- user_id
	store_migrer(seance)
}

#' Séances d'un compte, de la plus récente à la plus ancienne
#' @noRd
store_sqlite_seances <- function(user_id) {
	if (!store_sqlite_user_ok(user_id)) {
		return(list())
	}

	lignes <- db_query(
		"SELECT blob FROM seances WHERE user_id = ? ORDER BY cree_le DESC",
		params = list(as.character(user_id))
	)

	if (!nrow(lignes)) {
		return(list())
	}

	seances <- lapply(lignes$blob, store_sqlite_unserialize)
	seances[!vapply(seances, is.null, logical(1))]
}

#' Résumé des séances d'un compte
#' @noRd
store_sqlite_lister <- function(user_id = NULL) {
	vide <- tibble::tibble(
		session_id = character(0),
		libelle = character(0),
		cree_le = as.POSIXct(character(0)),
		statut = character(0),
		nb_questions = integer(0),
		n_repondues = integer(0),
		score = numeric(0)
	)

	seances <- store_sqlite_seances(user_id)

	if (!length(seances)) {
		return(vide)
	}

	lignes <- lapply(seances, store_resume)
	lignes <- lignes[!vapply(lignes, is.null, logical(1))]

	if (!length(lignes)) {
		return(vide)
	}

	tableau <- do.call(rbind, lignes)
	tableau[order(tableau$cree_le, decreasing = TRUE), , drop = FALSE]
}

#' Supprimer une séance du compte
#' @noRd
store_sqlite_supprimer <- function(session_id, user_id = NULL) {
	user_id <- store_sqlite_exiger_user(user_id)
	n <- db_execute(
		"DELETE FROM seances WHERE session_id = ? AND user_id = ?",
		params = list(as.character(session_id), user_id)
	)

	n > 0L
}

#' Graines vues par un compte
#' @noRd
store_sqlite_seeds <- function(n_seances, sauf_session_id, user_id) {
	if (!is.null(n_seances) && !is.na(as.integer(n_seances)) && as.integer(n_seances) <= 0L) {
		return(character(0))
	}

	seances <- store_sqlite_seances(user_id)

	if (!is.null(sauf_session_id)) {
		seances <- Filter(
			function(s) !identical(as.character(s$session_id), as.character(sauf_session_id)),
			seances
		)
	}

	if (!is.null(n_seances) && !is.na(as.integer(n_seances))) {
		seances <- utils::head(seances, as.integer(n_seances))
	}

	unique(unlist(lapply(seances, seance_seeds_servies), use.names = FALSE))
}

#' Signalements, pour l'écran d'administration
#' @noRd
signalement_lister <- function() {
	vide <- tibble::tibble(
		user_id = character(0),
		session_id = character(0),
		item_id = character(0),
		motif = character(0),
		cree_le = character(0)
	)

	if (!identical(store_backend(), "sqlite")) {
		chemin <- file.path(store_dir(), "signalements.rds")

		if (!file.exists(chemin)) {
			return(vide)
		}

		lignes <- readRDS(chemin)

		if (!length(lignes)) {
			return(vide)
		}

		return(do.call(rbind, lapply(lignes, function(l) {
			tibble::tibble(
				user_id = as.character(l$user_id),
				session_id = as.character(l$session_id),
				item_id = as.character(l$item_id),
				motif = as.character(l$motif),
				cree_le = as.character(l$cree_le)
			)
		})))
	}

	tableau <- db_query(
		"SELECT user_id, session_id, item_id, motif, cree_le FROM signalements ORDER BY id DESC"
	)

	if (!nrow(tableau)) {
		return(vide)
	}

	tibble::as_tibble(tableau)
}
