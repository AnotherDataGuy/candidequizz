# Fabriques d'objets de test pour la chaîne de génération.
#
# Les tests de validation et de notation doivent pouvoir construire un item
# arbitrairement conforme ou fautif sans dépendre du classeur ni du réseau.

#' Une consigne de test
#'
#' @param ... Champs à surcharger.
#'
#' @return Une consigne.
#' @noRd
consigne_test <- function(...) {
	graine <- tibble::tibble(
		seed_id = "TEST-001",
		full_theme_id = "T1-S1",
		theme_id = "T1",
		theme = "Th\u00e8me de test",
		sub_theme_id = "S1",
		sub_theme = "Sous-th\u00e8me de test",
		question_ref = "Quelle est la devise de la R\u00e9publique fran\u00e7aise ?",
		objectif = "Conna\u00eetre la devise de la R\u00e9publique.",
		explication = paste(
			"Libert\u00e9, \u00c9galit\u00e9, Fraternit\u00e9.",
			"",
			"C'est la devise officielle de la R\u00e9publique fran\u00e7aise,",
			"inscrite dans la Constitution."
		),
		cadre = NA_character_,
		difficulte_graine = 4L,
		options = list(tibble::tibble(
			option_id = c("a", "b", "c", "d"),
			texte = c(
				"Libert\u00e9, \u00c9galit\u00e9, Fraternit\u00e9",
				"Travail, Famille, Patrie",
				"Unit\u00e9, Justice, Libert\u00e9",
				"Paix, Ordre, Progr\u00e8s"
			),
			is_correct = c(TRUE, FALSE, FALSE, FALSE)
		)),
		erreurs = list(tibble::tibble(
			mistake_id = "m1",
			diagnostic_tag = "confusion_regime_vichy",
			description = "Confond la devise r\u00e9publicaine avec celle de Vichy."
		)),
		occurrence = 1L,
		occurrences_total = 1L
	)

	utils::modifyList(
		list(
			numero = 1L,
			indice_seance = 1L,
			lot = 1L,
			type = "qcm_single",
			n_options = 3L,
			difficulte = 3L,
			verite_attendue = TRUE,
			graine = graine
		),
		list(...)
	)
}

#' Un item de test
#'
#' @param type Type de question.
#' @param payload Contenu propre au type.
#' @param ... Champs à surcharger.
#'
#' @return Un item.
#' @noRd
item_test <- function(type = "qcm_single", payload = NULL, ...) {
	payloads <- list(
		qcm_single = list(options = list(
			list(texte = "Charles de Gaulle", is_correct = TRUE, diagnostic_tag = ""),
			list(texte = "Jean Moulin", is_correct = FALSE, diagnostic_tag = "confusion_resistance"),
			list(texte = "Philippe P\u00e9tain", is_correct = FALSE, diagnostic_tag = "confusion_vichy")
		)),
		qcm_multiple = list(options = list(
			list(texte = "La libert\u00e9", is_correct = TRUE, diagnostic_tag = ""),
			list(texte = "L'\u00e9galit\u00e9", is_correct = TRUE, diagnostic_tag = ""),
			list(texte = "La prosp\u00e9rit\u00e9", is_correct = FALSE, diagnostic_tag = "confusion_economie"),
			list(texte = "La s\u00e9curit\u00e9", is_correct = FALSE, diagnostic_tag = "confusion_ordre")
		)),
		vrai_faux = list(reponse = TRUE, diagnostic_tag = "confusion_date"),
		ordre = list(elements = list(
			list(texte = "R\u00e9volution fran\u00e7aise", rang_correct = 1L),
			list(texte = "Loi de s\u00e9paration", rang_correct = 2L),
			list(texte = "Droit de vote des femmes", rang_correct = 3L),
			list(texte = "Ve R\u00e9publique", rang_correct = 4L)
		)),
		texte_a_trous = list(
			texte = "La devise de la R\u00e9publique est {{1}}, \u00e9galit\u00e9, fraternit\u00e9.",
			trous = list(list(position = 1L, mot_correct = "libert\u00e9")),
			distracteurs = c("s\u00e9curit\u00e9", "prosp\u00e9rit\u00e9")
		)
	)

	# L'énoncé dépend du type : un vrai-faux est une affirmation, une remise en
	# ordre est une consigne de classement. Un énoncé unique ferait échouer les
	# validateurs pour la mauvaise raison.
	enonces <- c(
		qcm_single = "Qui a lanc\u00e9 l'appel depuis Londres en juin 1940 ?",
		qcm_multiple = "Quelles valeurs figurent dans la devise ? Plusieurs r\u00e9ponses attendues.",
		vrai_faux = "La devise de la R\u00e9publique fran\u00e7aise comporte trois termes.",
		ordre = "Classez ces \u00e9v\u00e9nements du plus ancien au plus r\u00e9cent.",
		texte_a_trous = "Compl\u00e9tez le texte avec les mots propos\u00e9s."
	)

	utils::modifyList(
		list(
			item_id = "TEST-001-L1Q1",
			numero = 1L,
			indice_seance = 1L,
			lot = 1L,
			type = type,
			difficulte = 3L,
			n_options = length(payloads[[type]]$options %||% list()),
			seed_id = "TEST-001",
			full_theme_id = "T1-S1",
			theme = "Th\u00e8me de test",
			sub_theme = "Sous-th\u00e8me de test",
			objectif = "Objectif de test.",
			enonce = unname(enonces[[type]]),
			explication = "C'est le g\u00e9n\u00e9ral qui appelle \u00e0 poursuivre le combat.",
			payload = payload %||% payloads[[type]],
			source = "llm",
			modele = "test",
			genere_le = Sys.time()
		),
		list(...)
	)
}
