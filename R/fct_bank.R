# Lecture et accès à la banque de graines.
#
# Rappel de conception : le classeur Excel est un GUIDE pédagogique, pas un
# stock de questions prêtes à servir. Les fonctions de ce fichier ne font que
# lire, valider et échantillonner des graines. Ce sont les agents de génération
# qui décident ensuite de la forme finale des questions, et qui ont le droit de
# reformuler, changer de type, réécrire les options ou recomposer les graines.

# Cache du package : la banque est lue une seule fois par session R.
.bank_cache <- new.env(parent = emptyenv())

#' Colonnes attendues dans la feuille des graines
#' @noRd
bank_colonnes_graines <- function() {
	c(
		"MAIN_QUESTION_ID",
		"MAIN_QUESTION",
		"LEARNING_GOAL",
		"ANSWER_OPTIONS",
		"POSSIBLE_MISTAKES",
		"REFERENTIEL_THEMES_CUSTOM",
		"DIFFICULTY",
		"EXPLANATION"
	)
}

#' Bornes de la difficulté intrinsèque des graines
#'
#' Échelle du classeur (1 à 10). Distincte de la difficulté adaptative de
#' séance (1 à 5), qui pilote le régleur et le prompt.
#'
#' @return Entiers `min` et `max`.
#' @noRd
bank_difficulte_bornes <- function() {
	c(min = 1L, max = 10L)
}

#' Convertir une difficulté adaptative (1–5) en cible sur l'échelle graine (1–10)
#'
#' @param difficulte Niveau adaptatif.
#'
#' @return Un entier entre 1 et 10.
#' @noRd
bank_difficulte_cible <- function(difficulte) {
	d <- suppressWarnings(as.integer(difficulte))

	if (length(d) != 1L || is.na(d)) {
		return(6L)
	}

	bornes <- bank_difficulte_bornes()
	as.integer(round(
		bornes[["min"]] + (d - 1L) * (bornes[["max"]] - bornes[["min"]]) / 4
	))
}

#' Bande de difficulté graine pour l'affichage statistique
#'
#' @param valeurs Entiers 1–10.
#'
#' @return Facteur ordonné de bandes.
#' @noRd
bank_difficulte_bande <- function(valeurs) {
	v <- suppressWarnings(as.integer(valeurs))
	bandes <- cut(
		v,
		breaks = c(0, 2, 4, 6, 8, 10),
		labels = c("1\u20132", "3\u20134", "5\u20136", "7\u20138", "9\u201310"),
		right = TRUE,
		include.lowest = TRUE
	)
	as.character(bandes)
}

#' Colonnes attendues dans la feuille du référentiel de thèmes
#' @noRd
bank_colonnes_referentiel <- function() {
	c(
		"theme",
		"theme_id",
		"sub_theme",
		"sub_theme_id",
		"explication",
		"full_theme_id"
	)
}

#' Chemin du classeur de la banque de graines
#'
#' @return Chemin absolu vers le fichier Excel embarqué dans le package.
#' @noRd
bank_path <- function() {
	fichier <- cq_config("bank", "file")
	chemin <- app_sys("extdata", fichier)

	if (!nzchar(chemin) || !file.exists(chemin)) {
		rlang::abort(
			c(
				"Le classeur de la banque de questions est introuvable.",
				i = glue::glue("Fichier attendu : `inst/extdata/{fichier}`.")
			),
			class = "candidequizz_bank_absente"
		)
	}

	chemin
}

#' Empreinte du classeur
#'
#' Enregistrée dans chaque séance (`version_banque`) afin de savoir sur quelle
#' version du référentiel une statistique a été produite.
#'
#' @param chemin Chemin du classeur.
#'
#' @return Une empreinte MD5, sous forme de chaîne de caractères.
#' @noRd
bank_version <- function(chemin = bank_path()) {
	unname(tools::md5sum(chemin))
}

#' Vérifier la présence des colonnes attendues
#'
#' @param donnees Un data frame.
#' @param attendues Vecteur des noms de colonnes requis.
#' @param feuille Nom de la feuille, pour le message d'erreur.
#'
#' @return `donnees`, invisiblement.
#' @noRd
bank_valider_colonnes <- function(donnees, attendues, feuille) {
	manquantes <- setdiff(attendues, names(donnees))

	if (length(manquantes)) {
		rlang::abort(
			c(
				glue::glue("Colonnes absentes dans la feuille `{feuille}` du classeur."),
				x = glue::glue("Manquantes : {paste(manquantes, collapse = ', ')}."),
				i = glue::glue("Attendues : {paste(attendues, collapse = ', ')}.")
			),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	invisible(donnees)
}

#' Parser une cellule JSON du classeur
#'
#' @param json Contenu de la cellule.
#' @param seed_id Identifiant de la graine, pour le message d'erreur.
#' @param colonne Nom de la colonne, pour le message d'erreur.
#'
#' @return Un data frame, ou `NULL` si la cellule est vide.
#' @noRd
bank_parser_json <- function(json, seed_id, colonne) {
	if (is.na(json) || !nzchar(trimws(json))) {
		return(NULL)
	}

	parse <- tryCatch(
		jsonlite::fromJSON(json, simplifyDataFrame = TRUE),
		error = function(e) {
			rlang::abort(
				c(
					glue::glue("JSON illisible dans `{colonne}` pour la graine `{seed_id}`."),
					x = conditionMessage(e)
				),
				class = "candidequizz_bank_json_invalide"
			)
		}
	)

	if (!is.data.frame(parse)) {
		rlang::abort(
			c(
				glue::glue("`{colonne}` doit contenir un tableau JSON d'objets."),
				x = glue::glue("Graine concern\u00e9e : `{seed_id}`.")
			),
			class = "candidequizz_bank_json_invalide"
		)
	}

	parse
}

#' Parser les options de réponse d'une graine
#'
#' Les options servent de matière première factuelle aux agents de génération :
#' elles disent ce qui est vrai et ce qui est faux. Ni leur nombre ni leur
#' formulation ne s'imposent aux questions produites.
#'
#' @param json Contenu de la cellule `ANSWER_OPTIONS`.
#' @param seed_id Identifiant de la graine.
#'
#' @return Un tibble `option_id`, `texte`, `is_correct`.
#' @noRd
bank_parser_options <- function(json, seed_id) {
	parse <- bank_parser_json(json, seed_id, "ANSWER_OPTIONS")

	if (is.null(parse)) {
		rlang::abort(
			glue::glue("`ANSWER_OPTIONS` est vide pour la graine `{seed_id}`."),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	bank_valider_colonnes(
		parse,
		c("option_id", "text", "is_correct"),
		glue::glue("ANSWER_OPTIONS / {seed_id}")
	)

	is_correct <- as.logical(parse$is_correct)

	if (anyNA(is_correct)) {
		rlang::abort(
			glue::glue("`is_correct` non bool\u00e9en pour la graine `{seed_id}`."),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	if (nrow(parse) < 2L) {
		rlang::abort(
			glue::glue("Moins de 2 options de r\u00e9ponse pour la graine `{seed_id}`."),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	if (!any(is_correct)) {
		rlang::abort(
			glue::glue("Aucune option correcte pour la graine `{seed_id}`."),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	tibble::tibble(
		option_id = as.character(parse$option_id),
		texte = as.character(parse$text),
		is_correct = is_correct
	)
}

#' Parser les erreurs typiques d'une graine
#'
#' Ces erreurs sont la matière des distracteurs : chaque distracteur généré est
#' rattaché à un `diagnostic_tag`, ce qui permet de savoir pourquoi
#' l'utilisateur s'est trompé et non seulement qu'il s'est trompé.
#'
#' @param json Contenu de la cellule `POSSIBLE_MISTAKES`.
#' @param seed_id Identifiant de la graine.
#'
#' @return Un tibble `mistake_id`, `diagnostic_tag`, `description`, possiblement
#'   sans aucune ligne.
#' @noRd
bank_parser_erreurs <- function(json, seed_id) {
	parse <- bank_parser_json(json, seed_id, "POSSIBLE_MISTAKES")

	vide <- tibble::tibble(
		mistake_id = character(0),
		diagnostic_tag = character(0),
		description = character(0)
	)

	if (is.null(parse) || !nrow(parse)) {
		return(vide)
	}

	bank_valider_colonnes(
		parse,
		c("mistake_id", "diagnostic_tag", "description"),
		glue::glue("POSSIBLE_MISTAKES / {seed_id}")
	)

	tibble::tibble(
		mistake_id = as.character(parse$mistake_id),
		diagnostic_tag = as.character(parse$diagnostic_tag),
		description = as.character(parse$description)
	)
}

#' Lire et valider le classeur de la banque
#'
#' Lecture sans cache. Utiliser [bank_load()] dans l'application.
#'
#' @param chemin Chemin du classeur.
#'
#' @return Une liste : `graines`, `referentiel`, `version`, `chemin`, `lu_le`.
#' @noRd
bank_read <- function(chemin = bank_path()) {
	feuilles <- readxl::excel_sheets(chemin)

	feuille_q <- cq_config("bank", "sheet_questions")
	feuille_r <- cq_config("bank", "sheet_referentiel")
	attendues <- c(feuille_q, feuille_r)
	manquantes <- setdiff(attendues, feuilles)

	if (length(manquantes)) {
		rlang::abort(
			c(
				"Feuilles absentes dans le classeur de la banque.",
				x = glue::glue("Manquantes : {paste(manquantes, collapse = ', ')}."),
				i = glue::glue("Feuilles trouv\u00e9es : {paste(feuilles, collapse = ', ')}.")
			),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	# Tout est lu en texte : le classeur ne contient que du texte et du JSON,
	# et cela évite toute conversion de type implicite par readxl.
	brut_q <- readxl::read_excel(chemin, sheet = feuille_q, col_types = "text")
	brut_r <- readxl::read_excel(chemin, sheet = feuille_r, col_types = "text")

	bank_valider_colonnes(brut_q, bank_colonnes_graines(), feuille_q)
	bank_valider_colonnes(brut_r, bank_colonnes_referentiel(), feuille_r)

	referentiel <- tibble::tibble(
		full_theme_id = as.character(brut_r$full_theme_id),
		theme_id = as.character(brut_r$theme_id),
		theme = as.character(brut_r$theme),
		sub_theme_id = as.character(brut_r$sub_theme_id),
		sub_theme = as.character(brut_r$sub_theme),
		cadre = as.character(brut_r$explication)
	)

	doublons_ref <- referentiel$full_theme_id[duplicated(referentiel$full_theme_id)]

	if (length(doublons_ref)) {
		rlang::abort(
			c(
				"`full_theme_id` doit \u00eatre unique dans le r\u00e9f\u00e9rentiel de th\u00e8mes.",
				x = glue::glue("En double : {paste(unique(doublons_ref), collapse = ', ')}.")
			),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	seed_id <- as.character(brut_q$MAIN_QUESTION_ID)
	doublons_graines <- seed_id[duplicated(seed_id)]

	if (length(doublons_graines)) {
		rlang::abort(
			c(
				"`MAIN_QUESTION_ID` doit \u00eatre unique dans la feuille des graines.",
				x = glue::glue("En double : {paste(unique(doublons_graines), collapse = ', ')}.")
			),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	full_theme_id <- as.character(brut_q$REFERENTIEL_THEMES_CUSTOM)
	orphelines <- setdiff(full_theme_id, referentiel$full_theme_id)

	if (length(orphelines)) {
		rlang::abort(
			c(
				"Des graines r\u00e9f\u00e9rencent un th\u00e8me absent du r\u00e9f\u00e9rentiel.",
				x = glue::glue("Th\u00e8mes inconnus : {paste(orphelines, collapse = ', ')}."),
				i = glue::glue(
					"Graines concern\u00e9es : {paste(utils::head(seed_id[full_theme_id %in% orphelines], 5), collapse = ', ')}."
				)
			),
			class = "candidequizz_bank_integrite_invalide"
		)
	}

	difficulte_graine <- suppressWarnings(as.integer(brut_q$DIFFICULTY))
	bornes <- bank_difficulte_bornes()
	diff_invalides <- is.na(difficulte_graine) |
		difficulte_graine < bornes[["min"]] |
		difficulte_graine > bornes[["max"]]

	if (any(diff_invalides)) {
		rlang::abort(
			c(
				glue::glue(
					"`DIFFICULTY` doit \u00eatre un entier entre {bornes[['min']]} et {bornes[['max']]}."
				),
				x = glue::glue(
					"Graines concern\u00e9es : {paste(utils::head(seed_id[diff_invalides], 5), collapse = ', ')}."
				)
			),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	explication <- trimws(as.character(brut_q$EXPLANATION %||% ""))
	explication[is.na(explication)] <- ""
	sans_explication <- !nzchar(explication)

	if (any(sans_explication)) {
		rlang::abort(
			c(
				"`EXPLANATION` est obligatoire pour chaque graine.",
				x = glue::glue(
					"Graines concern\u00e9es : {paste(utils::head(seed_id[sans_explication], 5), collapse = ', ')}."
				)
			),
			class = "candidequizz_bank_schema_invalide"
		)
	}

	options <- lapply(
		seq_along(seed_id),
		function(i) bank_parser_options(brut_q$ANSWER_OPTIONS[[i]], seed_id[[i]])
	)

	erreurs <- lapply(
		seq_along(seed_id),
		function(i) bank_parser_erreurs(brut_q$POSSIBLE_MISTAKES[[i]], seed_id[[i]])
	)

	correspondance <- match(full_theme_id, referentiel$full_theme_id)

	graines <- tibble::tibble(
		seed_id = seed_id,
		full_theme_id = full_theme_id,
		theme_id = referentiel$theme_id[correspondance],
		theme = referentiel$theme[correspondance],
		sub_theme_id = referentiel$sub_theme_id[correspondance],
		sub_theme = referentiel$sub_theme[correspondance],
		question_ref = as.character(brut_q$MAIN_QUESTION),
		objectif = as.character(brut_q$LEARNING_GOAL),
		explication = explication,
		difficulte_graine = difficulte_graine,
		cadre = referentiel$cadre[correspondance],
		options = options,
		erreurs = erreurs
	)

	list(
		graines = graines,
		referentiel = referentiel,
		version = bank_version(chemin),
		chemin = chemin,
		lu_le = Sys.time()
	)
}

#' Banque de graines, avec cache
#'
#' @param force Relire le classeur même s'il est déjà en cache.
#'
#' @return La liste renvoyée par [bank_read()].
#' @noRd
bank_load <- function(force = FALSE) {
	if (force || is.null(.bank_cache$bank)) {
		.bank_cache$bank <- bank_read()
	}

	.bank_cache$bank
}

#' Vider le cache de la banque
#'
#' @return `NULL`, invisiblement.
#' @noRd
bank_invalidate <- function() {
	.bank_cache$bank <- NULL
	invisible(NULL)
}

#' Thèmes du référentiel
#'
#' @param bank La banque chargée.
#'
#' @return Un tibble `theme_id`, `theme`, `n_sous_themes`, `n_graines`, dans
#'   l'ordre pédagogique du classeur.
#' @noRd
bank_themes <- function(bank = bank_load()) {
	ordre <- unique(bank$referentiel$theme_id)

	tibble::tibble(
		theme_id = ordre,
		theme = bank$referentiel$theme[match(ordre, bank$referentiel$theme_id)],
		n_sous_themes = as.integer(table(
			factor(bank$referentiel$theme_id, levels = ordre)
		)),
		n_graines = as.integer(table(
			factor(bank$graines$theme_id, levels = ordre)
		))
	)
}

#' Sous-thèmes du référentiel
#'
#' @param theme_ids Restreindre à ces thèmes. `NULL` pour tous.
#' @param bank La banque chargée.
#'
#' @return Un tibble décrivant chaque sous-thème, avec son cadre thématique et
#'   son nombre de graines.
#' @noRd
bank_sub_themes <- function(theme_ids = NULL, bank = bank_load()) {
	referentiel <- bank$referentiel

	if (!is.null(theme_ids) && length(theme_ids)) {
		inconnus <- setdiff(theme_ids, referentiel$theme_id)

		if (length(inconnus)) {
			rlang::abort(
				c(
					"Th\u00e8me inconnu dans le r\u00e9f\u00e9rentiel.",
					x = glue::glue("Inconnus : {paste(inconnus, collapse = ', ')}.")
				),
				class = "candidequizz_bank_theme_inconnu"
			)
		}

		referentiel <- referentiel[referentiel$theme_id %in% theme_ids, , drop = FALSE]
	}

	referentiel$n_graines <- as.integer(table(
		factor(bank$graines$full_theme_id, levels = referentiel$full_theme_id)
	))

	referentiel
}

#' Répartir un effectif entre plusieurs sous-thèmes
#'
#' Méthode du plus fort reste : la somme des quotas est exactement `n`, et la
#' répartition respecte les poids sans dériver par arrondis successifs.
#'
#' @param n Effectif total à répartir.
#' @param ids Identifiants entre lesquels répartir.
#' @param poids Vecteur nommé de poids, indexé par `ids`. `NULL` pour une
#'   répartition égale. Les poids manquants ou non positifs valent 1.
#'
#' @return Un vecteur d'entiers nommé par `ids`, de somme `n`.
#' @noRd
bank_quotas <- function(n, ids, poids = NULL) {
	n <- as.integer(n)

	if (!length(ids)) {
		return(stats::setNames(integer(0), character(0)))
	}

	if (is.null(poids)) {
		valeurs <- rep(1, length(ids))
	} else {
		valeurs <- as.numeric(poids[ids])
		valeurs[is.na(valeurs) | valeurs <= 0] <- 1
	}

	proportions <- valeurs / sum(valeurs)
	theorique <- n * proportions
	quotas <- floor(theorique)
	reste <- n - sum(quotas)

	if (reste > 0) {
		# Le reste va aux identifiants dont la part décimale est la plus forte.
		priorite <- order(theorique - quotas, decreasing = TRUE)
		quotas[priorite[seq_len(reste)]] <- quotas[priorite[seq_len(reste)]] + 1
	}

	stats::setNames(as.integer(quotas), ids)
}

#' Échantillonner sans surprise sur les vecteurs de longueur 1
#'
#' `sample()` interprète un vecteur de longueur 1 comme une borne supérieure.
#'
#' @param x Vecteur dans lequel tirer.
#' @param k Nombre d'éléments à tirer, sans remise.
#'
#' @return Un sous-vecteur de `x`.
#' @noRd
bank_echantillon <- function(x, k) {
	if (k <= 0 || !length(x)) {
		return(x[0])
	}

	x[sample.int(length(x), min(k, length(x)))]
}

#' Tirer les graines d'un sous-thème
#'
#' Priorité aux graines non encore vues. Quand le vivier est épuisé, on y
#' repasse : c'est le cas normal d'une longue séance sur peu de sous-thèmes,
#' les agents de génération étant chargés de produire des variantes distinctes.
#' À fraîcheur égale, on privilégie les graines dont la difficulté intrinsèque
#' est la plus proche de la cible adaptative.
#'
#' @param positions Indices des graines candidates.
#' @param ids Identifiants correspondant à `positions`.
#' @param k Nombre de graines à tirer.
#' @param exclude_ids Identifiants déjà utilisés, à éviter en priorité.
#' @param difficultes Difficultés intrinsèques correspondant à `positions`.
#' @param difficulte_cible Cible sur l'échelle 1–10, ou `NULL`.
#'
#' @return Un vecteur d'indices de longueur `k`, avec répétitions possibles.
#' @noRd
bank_tirer <- function(positions,
											 ids,
											 k,
											 exclude_ids = character(),
											 difficultes = NULL,
											 difficulte_cible = NULL) {
	frais <- positions[!ids %in% exclude_ids]
	deja_vues <- positions[ids %in% exclude_ids]

	choix <- bank_echantillon_proche(
		frais,
		k,
		positions = positions,
		difficultes = difficultes,
		difficulte_cible = difficulte_cible
	)
	choix <- c(
		choix,
		bank_echantillon_proche(
			deja_vues,
			k - length(choix),
			positions = positions,
			difficultes = difficultes,
			difficulte_cible = difficulte_cible
		)
	)

	while (length(choix) < k) {
		choix <- c(
			choix,
			bank_echantillon_proche(
				positions,
				k - length(choix),
				positions = positions,
				difficultes = difficultes,
				difficulte_cible = difficulte_cible
			)
		)
	}

	choix
}

#' Échantillonner en privilégiant la proximité de difficulté
#'
#' @param candidats Indices disponibles.
#' @param k Nombre à tirer.
#' @param positions Indices de référence pour lire `difficultes`.
#' @param difficultes Difficultés intrinsèques alignées sur `positions`.
#' @param difficulte_cible Cible 1–10, ou `NULL` pour un tirage uniforme.
#'
#' @return Un sous-vecteur de `candidats`.
#' @noRd
bank_echantillon_proche <- function(candidats,
																		k,
																		positions = candidats,
																		difficultes = NULL,
																		difficulte_cible = NULL) {
	if (k <= 0 || !length(candidats)) {
		return(candidats[0])
	}

	if (is.null(difficulte_cible) || is.null(difficultes) || !length(difficultes)) {
		return(bank_echantillon(candidats, k))
	}

	cible <- as.integer(difficulte_cible)
	rang <- match(candidats, positions)
	distance <- abs(as.integer(difficultes[rang]) - cible)
	ordre <- order(distance, sample.int(length(candidats)))
	candidats[ordre[seq_len(min(k, length(candidats)))]]
}

#' Ordonner les sous-thèmes selon les graines encore inédites
#'
#' Sans cela, `bank_quotas()` à poids égaux sert toujours les premiers
#' sous-thèmes du référentiel. On place d'abord ceux qui ont encore des
#' graines hors de `exclude_ids`, et on mélange à fraîcheur égale.
#'
#' @param ids Identifiants de sous-thèmes.
#' @param graines Table des graines candidates.
#' @param exclude_ids Graines déjà servies.
#'
#' @return `ids` réordonnés.
#' @noRd
bank_priorite_themes <- function(ids, graines, exclude_ids = character()) {
	if (!length(ids)) {
		return(ids)
	}

	# Indicateur oui/non, pas un compte : avec un compte, les sous-thèmes les
	# plus fournis (ex. 19 graines) passaient toujours devant et une séance
	# « tous thèmes » ne servait presque que « Société et culture ».
	fraicheur <- vapply(ids, function(id) {
		as.integer(any(
			graines$full_theme_id == id & !graines$seed_id %in% exclude_ids
		))
	}, integer(1))

	ids[order(-fraicheur, sample.int(length(ids)))]
}

#' Sélectionner les graines d'un lot de questions
#'
#' @param n Nombre de graines à fournir.
#' @param full_theme_ids Sous-thèmes autorisés. `NULL` pour tout le référentiel.
#' @param exclude_ids Graines déjà utilisées, évitées en priorité.
#' @param poids Vecteur nommé par `full_theme_id` permettant de surpondérer les
#'   sous-thèmes fragiles. C'est le levier utilisé par le régleur adaptatif.
#' @param difficulte Difficulté adaptative de séance (1–5). Convertie en cible
#'   sur l'échelle intrinsèque des graines (1–10) pour le tirage.
#' @param bank La banque chargée.
#'
#' @return Un tibble de `n` lignes reprenant les colonnes de `bank$graines`,
#'   dans un ordre mélangé, avec deux colonnes supplémentaires :
#'   `occurrence` (rang d'apparition de la graine dans le lot) et
#'   `occurrences_total` (nombre de fois où elle apparaît dans le lot). Une
#'   graine peut apparaître plusieurs fois : le rédacteur doit alors produire
#'   autant de questions distinctes visant le même objectif pédagogique.
#' @noRd
bank_seeds <- function(n,
											 full_theme_ids = NULL,
											 exclude_ids = character(),
											 poids = NULL,
											 difficulte = NULL,
											 bank = bank_load()) {
	n <- as.integer(n)

	if (is.na(n) || n < 1L) {
		rlang::abort(
			"Le nombre de graines demand\u00e9 doit \u00eatre un entier positif.",
			class = "candidequizz_bank_appel_invalide"
		)
	}

	graines <- bank$graines

	if (!is.null(full_theme_ids) && length(full_theme_ids)) {
		inconnus <- setdiff(full_theme_ids, bank$referentiel$full_theme_id)

		if (length(inconnus)) {
			rlang::abort(
				c(
					"Sous-th\u00e8me inconnu dans le r\u00e9f\u00e9rentiel.",
					x = glue::glue("Inconnus : {paste(inconnus, collapse = ', ')}.")
				),
				class = "candidequizz_bank_theme_inconnu"
			)
		}

		graines <- graines[graines$full_theme_id %in% full_theme_ids, , drop = FALSE]
	}

	if (!nrow(graines)) {
		rlang::abort(
			"Aucune graine ne correspond \u00e0 la s\u00e9lection th\u00e9matique demand\u00e9e.",
			class = "candidequizz_bank_selection_vide"
		)
	}

	ids <- unique(graines$full_theme_id)

	# À poids égaux, le plus fort reste donnait toujours les mêmes sous-thèmes
	# (ordre stable du référentiel). On sert d'abord ceux qui ont encore des
	# graines non vues. Un poids explicite (révision, régleur) reste prioritaire.
	poids_valeurs <- if (is.null(poids)) {
		rep(1, length(ids))
	} else {
		valeurs <- as.numeric(poids[ids])
		valeurs[is.na(valeurs) | valeurs <= 0] <- 1
		valeurs
	}

	if (length(unique(poids_valeurs)) <= 1L) {
		ids <- bank_priorite_themes(ids, graines, exclude_ids)
	}

	quotas <- bank_quotas(n, ids, poids)
	cible <- if (is.null(difficulte)) NULL else bank_difficulte_cible(difficulte)

	lignes <- unlist(
		lapply(ids, function(id) {
			positions <- which(graines$full_theme_id == id)

			bank_tirer(
				positions = positions,
				ids = graines$seed_id[positions],
				k = quotas[[id]],
				exclude_ids = exclude_ids,
				difficultes = graines$difficulte_graine[positions],
				difficulte_cible = cible
			)
		}),
		use.names = FALSE
	)

	# Mélange : sans cela le lot serait servi sous-thème par sous-thème, ce qui
	# nuit à la mémorisation et contredit la règle anti-lassitude.
	lot <- graines[lignes[sample.int(length(lignes))], , drop = FALSE]

	lot$occurrence <- as.integer(
		stats::ave(seq_len(nrow(lot)), lot$seed_id, FUN = seq_along)
	)
	lot$occurrences_total <- as.integer(table(lot$seed_id)[lot$seed_id])

	lot
}
