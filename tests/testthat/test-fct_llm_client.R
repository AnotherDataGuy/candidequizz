test_that("la disponibilit\u00e9 de la cl\u00e9 suit l'environnement", {
	withr::with_envvar(c(OPENAI_API_KEY = ""), expect_false(llm_cle_disponible()))
	withr::with_envvar(c(OPENAI_API_KEY = "sk-test"), expect_true(llm_cle_disponible()))
})

test_that("l'absence de cl\u00e9 produit un message actionnable, pas une erreur r\u00e9seau", {
	withr::local_envvar(c(OPENAI_API_KEY = ""))

	expect_error(llm_exiger_cle(), class = "candidequizz_llm_cle_absente")
	expect_error(llm_exiger_cle(), regexp = "\\.Renviron")
})

test_that("aucun appel n'est tent\u00e9 sans cl\u00e9", {
	withr::local_envvar(c(OPENAI_API_KEY = ""))

	expect_error(
		llm_appel_structure(
			agent = "test",
			role = "redacteur",
			systeme = "s",
			invite = "i",
			schema = ellmer::type_object(x = ellmer::type_string())
		),
		class = "candidequizz_llm_cle_absente"
	)
})

test_that("une ligne de journal chiffre le co\u00fbt de l'appel", {
	ligne <- llm_ligne_journal(
		agent = "redacteur",
		role = "redacteur",
		modele = "gpt-4.1-mini",
		tokens_entree = 1e6,
		tokens_sortie = 1e6,
		duree_s = 2
	)

	prix <- cq_llm_prix("gpt-4.1-mini")

	expect_identical(ligne$cout_usd, prix$entree + prix$sortie)
	expect_identical(ligne$statut, "ok")
	expect_s3_class(ligne$horodatage, "POSIXct")
})

test_that("la synth\u00e8se du journal agr\u00e8ge appels, tokens, co\u00fbt et \u00e9checs", {
	journal <- list(
		llm_ligne_journal("a", "redacteur", "gpt-4.1-mini", 1000, 500, 1.5),
		llm_ligne_journal("b", "verificateur", "gpt-4.1-nano", 2000, 100, 0.5),
		llm_ligne_journal(
			"c", "reparateur", "gpt-4.1-mini",
			duree_s = 3, statut = "echec", message = "timeout"
		)
	)

	synthese <- llm_synthese_journal(journal)

	expect_identical(synthese$n_appels, 3L)
	expect_identical(synthese$n_echecs, 1L)
	expect_identical(synthese$tokens_entree, 3000L)
	expect_identical(synthese$tokens_sortie, 600L)
	expect_identical(synthese$duree_s, 5)
	expect_true(synthese$cout_usd > 0)
})

test_that("tous les mod\u00e8les des presets sont tarif\u00e9s", {
	# Sinon le coût affiché dans le rapport de séance serait silencieusement nul.
	for (preset in names(cq_config("llm", "presets"))) {
		for (role in cq_llm_roles()) {
			modele <- cq_llm_model(role, preset = preset)
			prix <- cq_llm_prix(modele)

			expect_true(prix$entree > 0, info = paste(preset, role, modele))
			expect_true(prix$sortie > 0, info = paste(preset, role, modele))
		}
	}
})
