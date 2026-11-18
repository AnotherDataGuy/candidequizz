test_that("les param\u00e8tres par d\u00e9faut sont valides", {
	p <- parametres_seance()

	expect_identical(p$mode, "qcm_classique")
	expect_identical(p$types, "qcm_single")
	expect_identical(p$profil, "libre")
	expect_true(p$adaptatif)
	expect_false(p$retour_differe)
	expect_identical(p$chronometre, "aucun")
	expect_true(p$nb_questions %in% cq_config("seance", "nb_questions_possibles"))
})

test_that("l'examen blanc verrouille le format officiel", {
	p <- parametres_seance(
		nb_questions = 10L,
		mode = "autres_types",
		n_options = 2L,
		profil = "examen_blanc"
	)

	expect_identical(p$nb_questions, 40L)
	expect_identical(p$mode, "qcm_classique")
	expect_identical(p$types, "qcm_single")
	expect_identical(p$n_options, 4L)
	expect_null(p$full_theme_ids)
	expect_true(p$retour_differe)
	expect_identical(p$chronometre, "global")
	expect_identical(p$limite_s, as.numeric(cq_config("seance", "examen_limite_s")))
	expect_false(p$adaptatif)
	expect_false(parametres_adaptatif_actif(p))
})

test_that("un profil inconnu est refus\u00e9", {
	expect_error(parametres_seance(profil = "inconnu"), class = "candidequizz_parametres_invalides")
})

test_that("le mode classique impose le QCM \u00e0 option unique", {
	# Même en demandant autre chose : un mode « classique » rempli de textes à
	# trous ne serait pas le mode que l'utilisateur a choisi.
	p <- parametres_seance(mode = "qcm_classique", types = c("ordre", "vrai_faux"))

	expect_identical(p$types, "qcm_single")
})

test_that("le mode autres types accepte tous les types disponibles", {
	p <- parametres_seance(nb_questions = 20, mode = "autres_types")

	expect_setequal(p$types, question_type_ids(mode = "autres_types"))
})

test_that("les param\u00e8tres hors domaine sont refus\u00e9s", {
	expect_error(parametres_seance(nb_questions = 15), class = "candidequizz_parametres_invalides")
	expect_error(parametres_seance(mode = "inconnu"), class = "candidequizz_parametres_invalides")
	expect_error(parametres_seance(n_options = 9), class = "candidequizz_parametres_invalides")
	expect_error(parametres_seance(n_options = 1), class = "candidequizz_parametres_invalides")
	expect_error(
		parametres_seance(mode = "autres_types", types = "inexistant"),
		class = "candidequizz_parametres_invalides"
	)
	expect_error(
		parametres_seance(full_theme_ids = "SOUS-THEME-INEXISTANT"),
		class = "candidequizz_parametres_invalides"
	)
})

test_that("`varie` est accept\u00e9 comme nombre d'options", {
	p <- parametres_seance(n_options = "varie")

	expect_identical(p$n_options, "varie")
})

test_that("un sous-th\u00e8me r\u00e9el est accept\u00e9", {
	bank <- bank_load()
	id <- bank$referentiel$full_theme_id[[1L]]

	p <- parametres_seance(full_theme_ids = id)

	expect_identical(p$full_theme_ids, id)
})

test_that("le nombre d'options demand\u00e9 est report\u00e9 sur les QCM", {
	expect_identical(parametres_n_options("qcm_single", 4L), 4L)
	expect_identical(parametres_n_options("qcm_single", 3L), 3L)
	expect_identical(parametres_n_options("qcm_single", 2L), 2L)

	# Hors bornes du type : la valeur admissible la plus proche.
	expect_identical(parametres_n_options("qcm_multiple", 2L), 4L)
})

test_that("le nombre d'options des QCM ne se transpose pas aux autres types", {
	# Quatre options de QCM ne veulent pas dire quatre trous : un texte de deux
	# phrases trou\u00e9 quatre fois devient illisible.
	expect_identical(parametres_n_options("texte_a_trous", 4L), 2L)
	expect_identical(parametres_n_options("texte_a_trous", 2L), 2L)
	expect_identical(parametres_n_options("ordre", 2L), 4L)

	# Le type sans notion d'options ne renvoie rien.
	expect_true(is.na(parametres_n_options("vrai_faux", 4L)))
})

test_that("`varie` reste dans les bornes du type", {
	for (i in seq_len(20L)) {
		expect_true(parametres_n_options("texte_a_trous", "varie") %in% 1:3)
		expect_true(parametres_n_options("qcm_single", "varie") %in% 2:4)
		expect_true(parametres_n_options("ordre", "varie") %in% 3:6)
	}
})
