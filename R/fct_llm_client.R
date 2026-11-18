# Couche d'accès aux modèles OpenAI.
#
# Tous les appels réseau de l'application passent par ici. Aucun agent
# n'appelle {ellmer} directement : cela garantit que chaque appel est compté,
# journalisé, et soumis à la même politique de reprise.

#' La clé API est-elle disponible
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
llm_cle_disponible <- function() {
	nzchar(Sys.getenv("OPENAI_API_KEY", ""))
}

#' L'utilisateur peut-il coller sa propre clé dans l'application
#'
#' Seulement dans la version à télécharger (mode `local`) : l'utilisateur est
#' sur son propre ordinateur et paie ses appels. Jamais dans la version en
#' ligne, où personne ne doit confier sa clé au serveur de l'administrateur.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
llm_cle_saisie_permise <- function() {
	identical(auth_mode(), "local")
}

#' Une chaîne a-t-elle la forme d'une clé OpenAI
#'
#' Contrôle de forme seulement (`sk-` puis au moins 20 caractères sans
#' espace) : une clé révoquée ou sans crédit sera signalée par OpenAI au
#' premier appel.
#'
#' @param cle Texte saisi.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
llm_cle_valide <- function(cle) {
	cle <- trimws(as.character(cle %||% ""))
	length(cle) == 1L && grepl("^sk-[A-Za-z0-9_-]{20,}$", cle)
}

#' Utiliser une clé collée dans l'application
#'
#' La clé n'est gardée qu'en mémoire, pour la session R en cours (variable
#' d'environnement `OPENAI_API_KEY`). Elle n'est écrite ni dans les séances,
#' ni dans les journaux, ni sur le disque. Les processus d'arrière-plan la
#' reçoivent à chaque lot (voir `async_appeler()`).
#'
#' @param cle Texte saisi.
#'
#' @return `TRUE`, invisiblement.
#' @noRd
llm_cle_definir <- function(cle) {
	if (!llm_cle_saisie_permise()) {
		rlang::abort(
			"La cl\u00e9 OpenAI se r\u00e8gle sur le serveur, pas dans l'application.",
			class = "candidequizz_cle_refusee"
		)
	}

	if (!llm_cle_valide(cle)) {
		rlang::abort(
			paste(
				"Cette cl\u00e9 ne ressemble pas \u00e0 une cl\u00e9 OpenAI :",
				"elle commence par sk- et ne contient pas d'espace."
			),
			class = "candidequizz_cle_invalide"
		)
	}

	Sys.setenv(OPENAI_API_KEY = trimws(as.character(cle)))
	invisible(TRUE)
}

#' L'IA est-elle choisie par défaut pour une nouvelle séance
#'
#' Il faut une clé ET `llm.par_defaut = ia_on` dans la configuration. La version
#' à télécharger (configuration `default`) laisse l'IA éteinte : l'utilisateur
#' peut l'activer dans « Options avancées » s'il a ajouté sa propre clé.
#' Clé absente du fichier de configuration : comportement historique (`ia_on`).
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
llm_par_defaut <- function() {
	if (!llm_cle_disponible()) {
		return(FALSE)
	}

	defaut <- tryCatch(
		as.character(cq_config("llm", "par_defaut")),
		error = function(e) "ia_on"
	)

	identical(defaut, "ia_on")
}

#' Exiger une clé API, avec un message actionnable
#'
#' @return `TRUE`, invisiblement.
#' @noRd
llm_exiger_cle <- function() {
	if (llm_cle_disponible()) {
		return(invisible(TRUE))
	}

	rlang::abort(
		c(
			"Aucune cl\u00e9 d'API OpenAI n'a \u00e9t\u00e9 trouv\u00e9e.",
			i = paste(
				"Collez votre cl\u00e9 dans l'application (\u00e9tape 2, \u00ab Avec IA \u00bb),",
				"ou ajoutez une ligne `OPENAI_API_KEY=sk-...` dans un fichier `.Renviron`,",
				"puis red\u00e9marrez la session R."
			),
			i = "Ce fichier est exclu du d\u00e9p\u00f4t : la cl\u00e9 ne sera jamais versionn\u00e9e."
		),
		class = "candidequizz_llm_cle_absente"
	)
}

#' Une ligne de journal d'appel LLM
#'
#' @param agent Identifiant de l'agent appelant.
#' @param role Rôle de modèle utilisé.
#' @param modele Identifiant du modèle.
#' @param tokens_entree,tokens_sortie Tokens consommés.
#' @param duree_s Durée de l'appel, en secondes.
#' @param tentatives Nombre de tentatives effectuées.
#' @param statut `"ok"` ou `"echec"`.
#' @param message Message d'erreur éventuel.
#'
#' @return Une liste d'une ligne de journal.
#' @noRd
llm_ligne_journal <- function(agent,
															role,
															modele,
															tokens_entree = 0L,
															tokens_sortie = 0L,
															duree_s = NA_real_,
															tentatives = 1L,
															statut = "ok",
															message = NA_character_) {
	list(
		agent = agent,
		role = role,
		modele = modele,
		tokens_entree = as.integer(tokens_entree),
		tokens_sortie = as.integer(tokens_sortie),
		cout_usd = cq_llm_cout(modele, tokens_entree, tokens_sortie),
		duree_s = as.numeric(duree_s),
		tentatives = as.integer(tentatives),
		statut = statut,
		message = as.character(message),
		horodatage = Sys.time()
	)
}

#' Tokens consommés par une conversation
#'
#' @param chat Un objet `Chat` d'`{ellmer}`.
#'
#' @return `list(entree, sortie)`.
#' @noRd
llm_tokens <- function(chat) {
	tokens <- tryCatch(chat$get_tokens(), error = function(e) NULL)

	if (is.null(tokens) || !nrow(tokens)) {
		return(list(entree = 0L, sortie = 0L))
	}

	list(
		entree = sum(tokens$input, na.rm = TRUE),
		sortie = sum(tokens$output, na.rm = TRUE)
	)
}

#' Une erreur d'appel vaut-elle une nouvelle tentative
#'
#' Une requête refusée (clé invalide, schéma rejeté, modèle inconnu) échouera
#' à l'identique : la reprendre ne fait que retarder le repli. Les erreurs
#' transitoires (429, 5xx) sont déjà reprises par `{httr2}` sous `{ellmer}`.
#'
#' @param erreur La condition capturée.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
llm_erreur_reprenable <- function(erreur) {
	!inherits(erreur, c("httr2_http_400", "httr2_http_401", "httr2_http_403", "httr2_http_404"))
}

#' Appel structuré à un modèle, avec reprises
#'
#' Chaque requête est bornée par `llm.timeout_s`. Les reprises réseau sont
#' laissées à `{ellmer}` (`llm.http_max_tries`), cette fonction ne reprend que
#' les échecs applicatifs, avec un recul de 1 s puis 2 s. Les tokens d'une
#' tentative ratée après réponse du modèle sont comptés : ils sont facturés.
#'
#' @param agent Identifiant de l'agent appelant, pour le journal.
#' @param role Rôle de modèle, parmi [cq_llm_roles()].
#' @param systeme Prompt système.
#' @param invite Message utilisateur.
#' @param schema Schéma de sortie attendu, un `ellmer::type_object()`.
#' @param max_tentatives Nombre maximal de tentatives.
#'
#' @return `list(donnees, journal, ok, message)`. En cas d'échec après toutes
#'   les tentatives, `donnees` vaut `NULL` et `ok` est `FALSE` : c'est à
#'   l'agent appelant de décider du repli, jamais à cette fonction.
#' @noRd
llm_appel_structure <- function(agent,
																role,
																systeme,
																invite,
																schema,
																max_tentatives = cq_config("llm", "max_retries")) {
	llm_exiger_cle()

	rlang::local_options(
		ellmer_timeout_s = as.numeric(cq_config("llm", "timeout_s")),
		ellmer_max_tries = as.integer(cq_config("llm", "http_max_tries"))
	)

	modele <- cq_llm_model(role)
	parametres <- cq_llm_params(role)
	max_tentatives <- max(1L, as.integer(max_tentatives))

	debut <- Sys.time()
	derniere_erreur <- NA_character_
	tokens <- list(entree = 0L, sortie = 0L)
	tentative <- 0L

	while (tentative < max_tentatives) {
		tentative <- tentative + 1L
		chat <- NULL

		resultat <- tryCatch(
			{
				chat <- ellmer::chat_openai(
					model = modele,
					system_prompt = systeme,
					params = parametres,
					echo = "none"
				)

				# convert = FALSE : listes R imbriquées prévisibles, plutôt que la
				# conversion en data frames à colonnes-listes qui rendrait les
				# validateurs plus fragiles.
				list(donnees = chat$chat_structured(invite, type = schema, convert = FALSE), erreur = NULL)
			},
			error = function(e) list(donnees = NULL, erreur = e)
		)

		if (!is.null(chat)) {
			consommes <- llm_tokens(chat)
			tokens$entree <- tokens$entree + consommes$entree
			tokens$sortie <- tokens$sortie + consommes$sortie
		}

		if (is.null(resultat$erreur)) {
			return(list(
				donnees = resultat$donnees,
				ok = TRUE,
				message = NA_character_,
				journal = llm_ligne_journal(
					agent = agent,
					role = role,
					modele = modele,
					tokens_entree = tokens$entree,
					tokens_sortie = tokens$sortie,
					duree_s = as.numeric(difftime(Sys.time(), debut, units = "secs")),
					tentatives = tentative,
					statut = "ok"
				)
			))
		}

		derniere_erreur <- conditionMessage(resultat$erreur)

		if (!llm_erreur_reprenable(resultat$erreur)) {
			break
		}

		if (tentative < max_tentatives) {
			Sys.sleep(2^(tentative - 1L))
		}
	}

	list(
		donnees = NULL,
		ok = FALSE,
		message = derniere_erreur,
		journal = llm_ligne_journal(
			agent = agent,
			role = role,
			modele = modele,
			tokens_entree = tokens$entree,
			tokens_sortie = tokens$sortie,
			duree_s = as.numeric(difftime(Sys.time(), debut, units = "secs")),
			tentatives = tentative,
			statut = "echec",
			message = derniere_erreur
		)
	)
}

#' Synthèse du journal d'appels
#'
#' @param journal Liste de lignes produites par [llm_ligne_journal()].
#'
#' @return Un tibble d'une ligne : nombre d'appels, tokens, coût, durée.
#' @noRd
llm_synthese_journal <- function(journal) {
	if (!length(journal)) {
		return(tibble::tibble(
			n_appels = 0L,
			n_echecs = 0L,
			tokens_entree = 0L,
			tokens_sortie = 0L,
			cout_usd = 0,
			duree_s = 0
		))
	}

	nombre <- function(champ) {
		vapply(journal, function(l) as.numeric(l[[champ]] %||% NA_real_), numeric(1))
	}

	tibble::tibble(
		n_appels = length(journal),
		n_echecs = sum(vapply(journal, function(l) identical(l$statut, "echec"), logical(1))),
		tokens_entree = as.integer(sum(nombre("tokens_entree"), na.rm = TRUE)),
		tokens_sortie = as.integer(sum(nombre("tokens_sortie"), na.rm = TRUE)),
		cout_usd = sum(nombre("cout_usd"), na.rm = TRUE),
		duree_s = sum(nombre("duree_s"), na.rm = TRUE)
	)
}
