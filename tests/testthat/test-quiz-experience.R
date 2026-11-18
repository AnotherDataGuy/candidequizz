test_that("les notes distinguent crédits partiels et réponses entièrement justes", {
	r <- data.frame(score = c(1, .5, 0, 0), is_correct = c(TRUE, FALSE, FALSE, FALSE),
		passee = c(FALSE, FALSE, FALSE, TRUE))
	note <- stats_note_issues(r)
	expect_match(note, "1 justes \\(25 %\\)")
	expect_match(note, "1 partielles, 1 fausses et 1 passées")
	expect_match(stats_note_parcours(r), "38 %")
	expect_match(stats_note_parcours(r), "1 réponse\\(s\\) entièrement juste")
	expect_match(stats_note_issues(r[FALSE, ]), "Aucune")
})

test_that("les notes de catégories gèrent égalités, valeurs absentes et petits effectifs", {
	t <- data.frame(cle = c("Institutions et principes de la République", "Histoire"),
		n = c(2, 8), reussite = c(.5, .5))
	expect_match(stats_note_categories(t), "même score : 50 %")
	expect_match(stats_note_categories(t), "restent à confirmer")
	t$reussite <- c(NA, 0)
	expect_match(stats_note_categories(t), "Histoire.*0 %")
	expect_match(as.character(stats_graphe_lollipop(t)), "Non mesuré")
	expect_match(as.character(stats_graphe_lollipop(t)), t$cle[[1L]], fixed = TRUE)
	t$reussite[] <- NA
	expect_match(stats_note_categories(t), "Aucun score")
})

test_that("les notes d'évolution n'inventent pas de tendance sur une seule séance", {
	e <- data.frame(reussite = c(.9, .7, NA))
	expect_match(stats_note_evolution(e), "-20 point")
	expect_match(stats_note_evolution(e[1, , drop = FALSE]), "Une seule séance")
	expect_match(stats_note_evolution(e[3, , drop = FALSE]), "Aucun score")
})

test_that("les questions du chargement proviennent de la banque sans modifier le tirage", {
	b <- list(graines = data.frame(question_ref = c(NA, "", "Une question ?", "Une question ?", "Autre question ?")))
	set.seed(42)
	avant <- .Random.seed
	expect_identical(quiz_loading_questions(b), c("Une question ?", "Autre question ?"))
	expect_identical(.Random.seed, avant)
	expect_length(quiz_loading_questions(b, limite = 1L), 1L)
	expect_true(all(quiz_loading_questions() %in% bank_load()$graines$question_ref))
	expect_match(as.character(quiz_loading_ui("test")), "<dialog")
})

test_that("le chargement attend le navigateur et ignore les accusés obsolètes", {
	demarrer_capture <- NULL
	appels <- 0L
	testthat::local_mocked_bindings(
		mod_seance_config_server = function(id, demarrer, ...) { demarrer_capture <<- demarrer },
		mod_seance_run_server = function(...) NULL,
		mod_seance_bilan_server = function(...) NULL,
		seances_demarrer = function(parametres, ...) {
			appels <<- appels + 1L
			list(session_id = "test", parametres = parametres)
		},
		store_sauver = function(...) NULL
	)
	compte <- shiny::reactive(list(user_id = "u-test", username = "test"))
	shiny::testServer(mod_seances_server, args = list(utilisateur = compte), {
		demarrer_capture(list(retour_differe = TRUE))
		expect_identical(appels, 0L)
		token <- preparation()$token
		session$setInputs(chargement_pret = "obsolete")
		expect_identical(appels, 0L)
		session$setInputs(chargement_pret = token)
		expect_identical(appels, 1L)
		expect_identical(vue(), "passation")
		expect_true(corrections_differees())
		expect_null(preparation())
		session$setInputs(chargement_pret = token)
		expect_identical(appels, 1L)
	})
})

test_that("un échec libère le chargement et permet une nouvelle tentative", {
	demarrer_capture <- NULL
	erreurs <- 0L
	testthat::local_mocked_bindings(
		mod_seance_config_server = function(id, demarrer, ...) { demarrer_capture <<- demarrer },
		mod_seance_run_server = function(...) NULL,
		mod_seance_bilan_server = function(...) NULL,
		seances_demarrer = function(...) simpleError("échec simulé"),
		seance_run_signaler_echec = function(...) { erreurs <<- erreurs + 1L }
	)
	compte <- shiny::reactive(list(user_id = "u-test", username = "test"))
	shiny::testServer(mod_seances_server, args = list(utilisateur = compte), {
		demarrer_capture(list())
		session$setInputs(chargement_pret = preparation()$token)
		expect_identical(erreurs, 1L)
		expect_identical(vue(), "config")
		expect_null(preparation())
		demarrer_capture(list())
		expect_type(preparation(), "list")
	})
})

test_that("aucune séance ne démarre sans compte connecté", {
	demarrer_capture <- NULL
	appels <- 0L
	testthat::local_mocked_bindings(
		mod_seance_config_server = function(id, demarrer, ...) { demarrer_capture <<- demarrer },
		mod_seance_run_server = function(...) NULL,
		mod_seance_bilan_server = function(...) NULL,
		seances_demarrer = function(...) { appels <<- appels + 1L; list() }
	)
	shiny::testServer(mod_seances_server, {
		demarrer_capture(list())
		expect_null(preparation())
		expect_identical(appels, 0L)
	})
})
