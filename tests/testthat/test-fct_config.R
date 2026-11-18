test_that("cq_config lit une valeur simple et une valeur imbriquée", {
	expect_type(cq_config("app_title"), "character")
	expect_true(is.numeric(cq_config("seance", "taille_lot")))
	expect_type(cq_config("llm", "presets", "equilibre", "roles", "redacteur"), "character")
})

test_that("cq_config échoue explicitement sur une clé absente", {
	expect_error(cq_config(), class = "candidequizz_config_appel_invalide")
	expect_error(
		cq_config("cle_inexistante"),
		class = "candidequizz_config_cle_absente"
	)
	expect_error(
		cq_config("llm", "cle_inexistante"),
		class = "candidequizz_config_cle_absente"
	)
})

test_that("les trois presets couvrent tous les rôles d'agent", {
	presets <- cq_config("llm", "presets")

	expect_setequal(names(presets), c("economique", "equilibre", "qualite"))

	for (preset in names(presets)) {
		expect_setequal(names(presets[[preset]]$roles), cq_llm_roles())
		expect_true(nzchar(cq_llm_effort(preset)))

		for (role in cq_llm_roles()) {
			modele <- cq_llm_model(role, preset = preset)
			expect_true(is.character(modele) && nzchar(modele))
		}
	}
})

test_that("tout modèle référencé dans un preset est tarifé", {
	presets <- cq_config("llm", "presets")
	modeles <- unique(unlist(
		lapply(presets, function(p) unlist(p$roles, use.names = FALSE)),
		use.names = FALSE
	))

	for (modele in modeles) {
		prix <- cq_llm_prix(modele)

		expect_false(
			is.null(prix),
			info = paste("Modèle non tarifé dans golem-config.yml :", modele)
		)
		expect_true(is.numeric(prix$entree) && prix$entree > 0)
		expect_true(is.numeric(prix$sortie) && prix$sortie > 0)
	}
})

test_that("l'effort de raisonnement ne cible que les modèles concernés", {
	expect_true(cq_llm_raisonne("gpt-5-mini"))
	expect_false(cq_llm_raisonne("gpt-4.1-mini"))

	# Preset economique : redacteur gpt-4.1-nano, sans raisonnement.
	sans <- cq_llm_params("redacteur", preset = "economique")
	expect_null(sans$reasoning_effort)

	# Preset qualite : redacteur gpt-5, avec raisonnement.
	avec <- cq_llm_params("redacteur", preset = "qualite")
	expect_identical(avec$reasoning_effort, cq_llm_effort("qualite"))
})

test_that("cq_llm_preset respecte la variable d'environnement", {
	withr::with_envvar(c(CANDIDEQUIZZ_PRESET = "economique"), {
		expect_identical(cq_llm_preset(), "economique")
	})

	withr::with_envvar(c(CANDIDEQUIZZ_PRESET = "inexistant"), {
		expect_error(cq_llm_preset(), class = "candidequizz_preset_inconnu")
	})

	withr::with_envvar(c(CANDIDEQUIZZ_PRESET = ""), {
		expect_identical(cq_llm_preset(), cq_config("llm", "preset"))
	})
})

test_that("cq_llm_cout calcule le coût par million de tokens", {
	# gpt-5-mini : 0,25 USD en entrée et 2,00 USD en sortie par million.
	expect_equal(cq_llm_cout("gpt-5-mini", 1e6, 0), 0.25)
	expect_equal(cq_llm_cout("gpt-5-mini", 0, 1e6), 2)
	expect_equal(cq_llm_cout("gpt-5-mini", 1e6, 1e6), 2.25)
	expect_true(is.na(cq_llm_cout("modele-non-tarife", 1e6, 1e6)))
})

test_that("un rôle d'agent inconnu est rejeté", {
	expect_error(cq_llm_model("agent_imaginaire"))
})
