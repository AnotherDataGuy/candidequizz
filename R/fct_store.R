# Persistance des séances.
#
# Un fichier `.rds` par séance, écrit après chaque réponse validée pour que rien
# ne soit perdu si l'application est fermée en cours de route.
#
# Les fichiers ne vivent pas dans le projet mais dans le répertoire de données
# utilisateur (`tools::R_user_dir()`). Un paquet installé n'a pas le droit
# d'écrire dans sa propre bibliothèque, et une application ne doit pas dépendre
# du répertoire de travail depuis lequel on l'a lancée : les séances d'un
# utilisateur survivent ainsi à une réinstallation comme à un déplacement du
# projet.

#' Répertoire de stockage des séances
#'
#' @param creer Créer le répertoire s'il est absent.
#'
#' @return Le chemin du répertoire.
#' @noRd
store_dir <- function(creer = FALSE) {
	# La variable d'environnement permet aux tests de travailler dans un
	# répertoire temporaire, et à un utilisateur de choisir où vont ses données.
	chemin <- Sys.getenv("CANDIDEQUIZZ_STORE_DIR", "")

	if (!nzchar(chemin)) {
		chemin <- file.path(tools::R_user_dir("candidequizz", which = "data"), "seances")
	}

	if (creer && !dir.exists(chemin)) {
		dir.create(chemin, recursive = TRUE, showWarnings = FALSE)
	}

	chemin
}

#' Chemin du fichier d'une séance
#'
#' @param session_id Identifiant de la séance.
#'
#' @return Le chemin du fichier.
#' @noRd
store_chemin <- function(session_id) {
	file.path(store_dir(), paste0(session_id, ".rds"))
}

#' Version du schéma de séance
#'
#' @return Un entier.
#' @noRd
store_version_schema <- function() {
	as.integer(cq_config("store", "schema_version"))
}

#' Identifiant de séance
#'
#' Horodaté pour que le tri par nom corresponde au tri chronologique, avec un
#' suffixe aléatoire pour le cas de deux séances créées dans la même seconde.
#'
#' @return Une chaîne de caractères.
#' @noRd
store_nouvel_id <- function() {
	paste0(
		format(Sys.time(), "%Y%m%d-%H%M%S"),
		"-",
		sodium::bin2hex(sodium::random(3L))
	)
}

#' Libellé par défaut d'une séance
#'
#' @param parametres Paramètres de la séance.
#'
#' @return Une chaîne de caractères.
#' @noRd
store_libelle_defaut <- function(parametres) {
	mode <- if (identical(parametres$profil, "examen_blanc")) {
		"Examen blanc"
	} else if (identical(parametres$profil, "revision_ciblee")) {
		"R\u00e9vision cibl\u00e9e"
	} else if (identical(parametres$mode, "qcm_classique")) {
		"QCM"
	} else {
		"Types vari\u00e9s"
	}

	glue::glue(
		"{mode} \u2014 {parametres$nb_questions} questions \u2014 ",
		"{format(Sys.time(), '%d/%m/%Y %H:%M')}"
	)
}

#' Créer une séance
#'
#' @param parametres Paramètres validés.
#' @param plan Plan produit par l'agent planificateur.
#' @param libelle Libellé de la séance.
#'
#' @return Une séance.
#' @noRd
seance_nouvelle <- function(parametres, plan, libelle = NULL) {
	list(
		session_id = store_nouvel_id(),
		libelle = libelle %||% parametres$libelle %||% as.character(store_libelle_defaut(parametres)),
		cree_le = Sys.time(),
		termine_le = NA,
		statut = "en_cours",
		parametres = parametres,
		plan = plan,
		items = list(),
		reponses = list(),
		lots_generes = 0L,
		journal_llm = list(),
		trace = list(),
		generation_etat = parametres$generation %||% "ia_off",
		user_id = NULL,
		version_schema = store_version_schema(),
		version_banque = bank_load()$version
	)
}

#' Ajouter un lot généré à une séance
#'
#' @param seance La séance.
#' @param ctx Contexte de génération renvoyé par [generer_lot()].
#'
#' @return La séance enrichie.
#' @noRd
seance_ajouter_lot <- function(seance, ctx) {
	seance$items <- c(seance$items, ctx$items)
	seance$journal_llm <- c(seance$journal_llm, ctx$journal_llm)
	seance$trace <- c(seance$trace, ctx$trace)
	seance$lots_generes <- seance$lots_generes + 1L
	seance$plan <- ctx$plan
	seance_noter_generation(seance, ctx)
}

#' Mémoriser l'état de génération après un lot
#'
#' `ia_off` reste sans IA. `ia_on` passe à `ia_coupure` si le lot a dû
#' se replier. Une relance réussie revient à `ia_on`.
#'
#' @param seance La séance.
#' @param ctx Contexte du lot qui vient d'être produit.
#'
#' @return La séance.
#' @noRd
seance_noter_generation <- function(seance, ctx) {
	gen <- seance$parametres$generation %||% "ia_on"

	etat <- if (identical(gen, "ia_off")) {
		"ia_off"
	} else if (isTRUE(ctx$degrade) || !is.null(ctx$coupure_llm)) {
		"ia_coupure"
	} else {
		"ia_on"
	}

	seance$generation_etat <- etat
	seance$parametres$generation_etat <- etat
	seance$parametres$generation_retenter <- NULL

	seance
}

#' Enregistrer une réponse dans une séance
#'
#' @param seance La séance.
#' @param reponse La réponse produite par [reponse_nouvelle()].
#'
#' @return La séance enrichie.
#' @noRd
seance_ajouter_reponse <- function(seance, reponse) {
	# Réécriture plutôt qu'ajout si l'item a déjà été répondu : une séance
	# rechargée ne doit pas compter deux fois la même question.
	deja <- vapply(seance$reponses, function(r) identical(r$item_id, reponse$item_id), logical(1))

	if (any(deja)) {
		seance$reponses[[which(deja)[[1L]]]] <- reponse
	} else {
		seance$reponses <- c(seance$reponses, list(reponse))
	}

	seance
}

#' Identifiants de graines déjà servies
#'
#' @param seance La séance.
#'
#' @return Un vecteur de caractères.
#' @noRd
seance_seeds_servies <- function(seance) {
	if (!length(seance$items)) {
		return(character(0))
	}

	unique(vapply(seance$items, function(i) as.character(i$seed_id), character(1)))
}

#' Graines déjà posées dans les séances récentes
#'
#' A-2 / P-03 : au redémarrage, une nouvelle séance doit éviter ce que
#' l'utilisateur a déjà vu, pas seulement ce qui a été servi *dans* la séance
#' en cours. `bank_tirer()` se rabat sur les graines déjà vues seulement si le
#' vivier est épuisé.
#'
#' @param n_seances Nombre de séances les plus récentes à consulter. `0` pour
#'   n'exclure personne. `NULL` pour toutes les séances enregistrées.
#' @param sauf_session_id Séance à ignorer, typiquement celle en cours, déjà
#'   couverte par [seance_seeds_servies()].
#'
#' @return Un vecteur d'identifiants, éventuellement vide.
#' @noRd
store_seeds_historiques <- function(n_seances = cq_config("seance", "anti_repetition_n_seances"),
																		sauf_session_id = NULL,
																		user_id = NULL) {
	if (identical(store_backend(), "sqlite")) {
		return(store_sqlite_seeds(n_seances, sauf_session_id, user_id))
	}

	fichiers <- list.files(store_dir(), pattern = "\\.rds$", full.names = TRUE)

	if (!length(fichiers)) {
		return(character(0))
	}

	if (!is.null(n_seances) && !is.na(as.integer(n_seances)) && as.integer(n_seances) <= 0L) {
		return(character(0))
	}

	lignes <- lapply(fichiers, function(fichier) {
		seance <- tryCatch(readRDS(fichier), error = function(e) NULL)

		if (is.null(seance) || is.null(seance$session_id)) {
			return(NULL)
		}

		if (!is.null(sauf_session_id) && identical(as.character(seance$session_id), as.character(sauf_session_id))) {
			return(NULL)
		}

		list(
			cree_le = as.POSIXct(seance$cree_le %||% NA),
			seeds = seance_seeds_servies(seance)
		)
	})

	lignes <- lignes[!vapply(lignes, is.null, logical(1))]

	if (!length(lignes)) {
		return(character(0))
	}

	ordre <- order(
		vapply(lignes, function(l) as.numeric(l$cree_le), numeric(1)),
		decreasing = TRUE,
		na.last = TRUE
	)
	lignes <- lignes[ordre]

	if (!is.null(n_seances) && !is.na(as.integer(n_seances))) {
		lignes <- utils::head(lignes, as.integer(n_seances))
	}

	unique(unlist(lapply(lignes, function(l) l$seeds), use.names = FALSE))
}

#' Réponses récentes utilisables par l'apprentissage adaptatif
#'
#' Les réponses restent sur l'appareil et sont transmises uniquement aux
#' règles déterministes. Le mode examen les ignore dans le régleur.
#'
#' @param n_seances Nombre de séances récentes à retenir.
#' @param sauf_session_id Séance courante à exclure pour éviter les doublons.
#'
#' @return Une liste plate de réponses.
#' @noRd
store_reponses_historiques <- function(
		n_seances = cq_config("seance", "anti_repetition_n_seances"),
		sauf_session_id = NULL,
		user_id = NULL) {
	seances <- stats_charger(user_id = user_id)

	if (!is.null(sauf_session_id)) {
		seances <- Filter(
			function(s) !identical(as.character(s$session_id), as.character(sauf_session_id)),
			seances
		)
	}

	n <- if (length(n_seances)) {
		suppressWarnings(as.integer(n_seances[[1L]]))
	} else {
		NA_integer_
	}
	if (!is.null(n_seances) && !is.na(n) && is.finite(n)) {
		n <- max(0L, n)
		seances <- utils::head(seances, n)
	}

	unlist(lapply(seances, function(s) s$reponses %||% list()), recursive = FALSE)
}

#' Graines à éviter pour le prochain tirage
#'
#' Union des graines de la séance en cours et de celles des séances récentes
#' encore en mémoire.
#'
#' @param seance La séance en cours, ou `NULL` avant le premier lot.
#'
#' @return Un vecteur d'identifiants.
#' @noRd
seance_seeds_a_eviter <- function(seance = NULL, user_id = NULL) {
	user_id <- user_id %||% seance$user_id
	unique(c(
		if (is.null(seance)) character(0) else seance_seeds_servies(seance),
		store_seeds_historiques(sauf_session_id = seance$session_id, user_id = user_id)
	))
}

#' L'item à poser
#'
#' @param seance La séance.
#' @param position Rang de la question dans la séance.
#'
#' @return Un item, ou `NULL` s'il n'est pas encore généré.
#' @noRd
seance_item <- function(seance, position) {
	position <- as.integer(position)

	if (!length(seance$items) || is.na(position) || position < 1L) {
		return(NULL)
	}

	for (item in seance$items) {
		rang <- as.integer(item$indice_seance %||% item$numero %||% NA_integer_)

		if (identical(rang, position)) {
			return(item)
		}
	}

	if (position <= length(seance$items)) {
		return(seance$items[[position]])
	}

	NULL
}

#' Rang de la première question sans réponse
#'
#' @param seance La séance.
#'
#' @return Un entier.
#' @noRd
seance_position_reprise <- function(seance) {
	length(seance$reponses) + 1L
}

#' Numéro de lot d'une position dans la séance
#'
#' @param plan Plan de séance.
#' @param position Rang de la question, 1-based.
#'
#' @return Un entier, ou `NA_integer_` hors plan.
#' @noRd
seance_lot_de_position <- function(plan, position) {
	position <- as.integer(position)

	for (i in seq_len(plan$n_lots)) {
		if (position %in% plan$indices_lots[[i]]) {
			return(as.integer(i))
		}
	}

	NA_integer_
}

#' Nombre de questions déjà écrites
#'
#' @param seance La séance.
#'
#' @return Le rang de la dernière question disponible, `0` si aucune.
#' @noRd
seance_dernier_rang_pret <- function(seance) {
	n <- as.integer(seance$lots_generes %||% 0L)

	if (is.null(seance$plan) || n < 1L) {
		return(0L)
	}

	as.integer(sum(seance$plan$tailles_lots[seq_len(n)]))
}

#' Faut-il préparer la tranche suivante
#'
#' File glissante : la tranche suivante part dès qu'il ne reste que
#' `prefetch_questions_restantes` questions prêtes, question affichée
#' comprise, quelle que soit la tranche dans laquelle on se trouve.
#'
#' @param seance La séance.
#' @param position Rang affiché.
#' @param seuil Nombre de questions prêtes en dessous duquel on prépare la suite.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
seance_doit_precharger <- function(seance,
																	 position,
																	 seuil = cq_config("seance", "prefetch_questions_restantes")) {
	if (is.null(seance$plan) || seance$lots_generes >= seance$plan$n_lots) {
		return(FALSE)
	}

	position <- as.integer(position)

	if (is.na(position) || position < 1L) {
		return(FALSE)
	}

	seance_dernier_rang_pret(seance) - position + 1L <= as.integer(seuil)
}

#' La séance est-elle achevée
#'
#' @param seance La séance.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
seance_achevee <- function(seance) {
	length(seance$reponses) >= seance$parametres$nb_questions
}

#' La séance est-elle close (terminée ou abandonnée)
#'
#' Une séance abandonnée n'a pas forcément toutes ses questions. Elle doit
#' quand même ouvrir le bilan, pas la passation.
#'
#' @param seance La séance.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
seance_closee <- function(seance) {
	identical(seance$statut, "terminee") ||
		identical(seance$statut, "abandonnee") ||
		seance_achevee(seance)
}

#' La séance a-t-elle un chronomètre global
#'
#' @param seance La séance.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
seance_chrono_actif <- function(seance) {
	identical(seance$parametres$chronometre %||% "aucun", "global") &&
		is.finite(seance$parametres$limite_s %||% NA_real_)
}

#' Avancer le chronomètre
#'
#' @param seance La séance.
#' @param maintenant Instant courant.
#'
#' @return La séance, `chrono_ecoule_s` à jour.
#' @noRd
seance_chrono_tick <- function(seance, maintenant = Sys.time()) {
	if (!seance_chrono_actif(seance) || seance_closee(seance)) {
		return(seance)
	}

	marque <- seance$chrono_marque_le
	if (!inherits(marque, "POSIXt") || is.na(marque)) {
		seance$chrono_marque_le <- maintenant
		seance$chrono_ecoule_s <- seance$chrono_ecoule_s %||% 0
		return(seance)
	}

	delta <- as.numeric(difftime(maintenant, marque, units = "secs"))
	seance$chrono_ecoule_s <- (seance$chrono_ecoule_s %||% 0) + max(0, delta)
	seance$chrono_marque_le <- maintenant
	seance
}

#' Secondes restantes au chronomètre
#'
#' @param seance La séance.
#'
#' @return Un nombre, éventuellement négatif si le temps est dépassé.
#' @noRd
seance_chrono_reste <- function(seance) {
	if (!seance_chrono_actif(seance)) {
		return(NA_real_)
	}

	seance$parametres$limite_s - (seance$chrono_ecoule_s %||% 0)
}

#' Libellé mm:ss du temps restant
#'
#' @param reste Secondes restantes.
#'
#' @return Une chaîne.
#' @noRd
seance_chrono_texte <- function(reste) {
	reste <- max(0, as.integer(floor(reste)))
	glue::glue("{reste %/% 60}:{sprintf('%02d', reste %% 60)}")
}

#' Clore une séance dont le temps est écoulé
#'
#' Les questions déjà générées et non répondues sont comptées comme passées.
#'
#' @param seance La séance.
#'
#' @return La séance close.
#' @noRd
seance_expirer_chrono <- function(seance) {
	repondus <- vapply(
		seance$reponses,
		function(r) as.character(r$item_id %||% ""),
		character(1)
	)

	for (item in seance$items) {
		if (item$item_id %in% repondus) {
			next
		}

		seance <- seance_ajouter_reponse(
			seance,
			reponse_nouvelle(
				item = item,
				reponse = NULL,
				affiche_le = Sys.time(),
				passee = TRUE
			)
		)
	}

	seance$chrono_epuise <- TRUE
	seance_clore(seance, "terminee")
}

#' Clore une séance
#'
#' @param seance La séance.
#' @param statut `"terminee"` ou `"abandonnee"`.
#'
#' @return La séance close.
#' @noRd
seance_clore <- function(seance, statut = "terminee") {
	seance$statut <- statut
	seance$termine_le <- Sys.time()

	seance
}

#' Écrire une séance sur le disque
#'
#' Écriture atomique : fichier temporaire dans le même répertoire puis
#' renommage. Une interruption pendant l'écriture ne peut donc pas laisser un
#' fichier de séance à moitié écrit, qui serait illisible au rechargement.
#'
#' @param seance La séance.
#'
#' @return Le chemin du fichier, invisiblement.
#' @noRd
store_sauver <- function(seance, user_id = NULL) {
	user_id <- user_id %||% seance$user_id

	if (identical(store_backend(), "sqlite")) {
		return(store_sqlite_sauver(seance, user_id))
	}

	repertoire <- store_dir(creer = TRUE)
	destination <- file.path(repertoire, paste0(seance$session_id, ".rds"))
	temporaire <- paste0(destination, ".tmp")

	saveRDS(seance, temporaire)

	if (!file.rename(temporaire, destination)) {
		unlink(temporaire)

		rlang::abort(
			c(
				"Impossible d'enregistrer la s\u00e9ance.",
				x = glue::glue("Destination : {destination}")
			),
			class = "candidequizz_store_ecriture_impossible"
		)
	}

	invisible(destination)
}

#' Migrer une séance vers le schéma courant
#'
#' Sans cette étape, toute évolution de structure rendrait illisible
#' l'historique de l'utilisateur. La fonction est volontairement écrite dès
#' maintenant, alors qu'il n'y a qu'une version : le jour où une migration sera
#' nécessaire, l'endroit où l'écrire existera déjà.
#'
#' @param seance La séance lue.
#'
#' @return La séance migrée.
#' @noRd
store_migrer <- function(seance) {
	version <- as.integer(seance$version_schema %||% 0L)

	if (version > store_version_schema()) {
		rlang::abort(
			c(
				"S\u00e9ance enregistr\u00e9e par une version plus r\u00e9cente de l'application.",
				i = glue::glue(
					"Sch\u00e9ma du fichier : {version}, sch\u00e9ma support\u00e9 : ",
					"{store_version_schema()}."
				)
			),
			class = "candidequizz_store_schema_futur"
		)
	}

	seance$version_schema <- store_version_schema()

	seance
}

#' Charger une séance
#'
#' @param session_id Identifiant de la séance.
#'
#' @return La séance.
#' @noRd
store_charger <- function(session_id, user_id = NULL) {
	if (identical(store_backend(), "sqlite")) {
		return(store_sqlite_charger(session_id, user_id))
	}

	chemin <- store_chemin(session_id)

	if (!file.exists(chemin)) {
		rlang::abort(
			glue::glue("Aucune s\u00e9ance enregistr\u00e9e sous l'identifiant `{session_id}`."),
			class = "candidequizz_store_seance_absente"
		)
	}

	seance <- store_migrer(readRDS(chemin))

	if (!is.null(user_id)) {
		seance$user_id <- as.character(user_id)
	}

	seance
}

#' Supprimer une séance
#'
#' @param session_id Identifiant de la séance.
#'
#' @return `TRUE` si le fichier a été supprimé.
#' @noRd
store_supprimer <- function(session_id, user_id = NULL) {
	if (identical(store_backend(), "sqlite")) {
		return(store_sqlite_supprimer(session_id, user_id))
	}

	chemin <- store_chemin(session_id)

	if (!file.exists(chemin)) {
		return(FALSE)
	}

	unlink(chemin) == 0L
}

#' Lister les séances enregistrées
#'
#' @return Un tibble, de la plus récente à la plus ancienne.
#' @noRd
store_resume <- function(seance) {
	if (is.null(seance) || is.null(seance$session_id)) {
		return(NULL)
	}

	reponses <- reponses_tibble(seance$reponses)

	tibble::tibble(
		session_id = as.character(seance$session_id),
		libelle = as.character(seance$libelle %||% seance$session_id),
		cree_le = as.POSIXct(seance$cree_le),
		statut = as.character(seance$statut %||% "en_cours"),
		nb_questions = as.integer(seance$parametres$nb_questions %||% NA_integer_),
		n_repondues = nrow(reponses),
		score = if (nrow(reponses)) mean(reponses$score) else NA_real_
	)
}

store_lister <- function(user_id = NULL) {
	if (identical(store_backend(), "sqlite")) {
		return(store_sqlite_lister(user_id))
	}

	vide <- tibble::tibble(
		session_id = character(0),
		libelle = character(0),
		cree_le = as.POSIXct(character(0)),
		statut = character(0),
		nb_questions = integer(0),
		n_repondues = integer(0),
		score = numeric(0)
	)

	fichiers <- list.files(store_dir(), pattern = "\\.rds$", full.names = TRUE)

	if (!length(fichiers)) {
		return(vide)
	}

	lignes <- lapply(fichiers, function(fichier) {
		# Un fichier illisible ne doit pas empêcher de lister les autres :
		# l'utilisateur perdrait l'accès à tout son historique pour une séance
		# corrompue.
		seance <- tryCatch(readRDS(fichier), error = function(e) NULL)

		if (is.null(seance) || is.null(seance$session_id)) {
			return(NULL)
		}

		store_resume(seance)
	})

	lignes <- lignes[!vapply(lignes, is.null, logical(1))]

	if (!length(lignes)) {
		return(vide)
	}

	tableau <- do.call(rbind, lignes)

	tableau[order(tableau$cree_le, decreasing = TRUE), , drop = FALSE]
}

#' La séance en cours la plus récente
#'
#' Alimente le bloc de reprise de l'accueil.
#'
#' @return La ligne de la séance dans le tableau de [store_lister()], ou `NULL`
#'   si aucune séance n'est reprenable.
#' @noRd
store_seance_reprenable <- function(user_id = NULL) {
	seances <- store_lister(user_id = user_id)
	en_cours <- seances[
		seances$statut == "en_cours" & seances$n_repondues < seances$nb_questions,
		,
		drop = FALSE
	]

	if (!nrow(en_cours)) {
		return(NULL)
	}

	en_cours[1L, , drop = FALSE]
}
