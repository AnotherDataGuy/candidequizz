# Clé OpenAI collée dans l'application (version à télécharger).

test_that("la forme d'une clé est contrôlée", {
	expect_true(llm_cle_valide("sk-proj-AbCdEfGhIjKlMnOpQrStUv_123"))
	expect_true(llm_cle_valide("  sk-AbCdEfGhIjKlMnOpQrStUvWx  "))
	expect_false(llm_cle_valide(""))
	expect_false(llm_cle_valide(NULL))
	expect_false(llm_cle_valide("sk-court"))
	expect_false(llm_cle_valide("pk-AbCdEfGhIjKlMnOpQrStUvWx"))
	expect_false(llm_cle_valide("sk-AbCdEfGhIj KlMnOpQrStUvWx"))
	expect_false(llm_cle_valide(c("sk-AbCdEfGhIjKlMnOpQrStUvWx", "sk-AbCdEfGhIjKlMnOpQrStUvWx")))
})

test_that("en mode local, une clé valide devient la clé de la session", {
	withr::local_envvar(c(CANDIDEQUIZZ_AUTH_MODE = "local", OPENAI_API_KEY = ""))
	expect_true(llm_cle_saisie_permise())
	expect_false(llm_cle_disponible())

	llm_cle_definir(" sk-proj-AbCdEfGhIjKlMnOpQrStUv_123 ")
	expect_identical(Sys.getenv("OPENAI_API_KEY"), "sk-proj-AbCdEfGhIjKlMnOpQrStUv_123")
	expect_true(llm_cle_disponible())
})

test_that("une clé mal formée est refusée avec un message lisible", {
	withr::local_envvar(c(CANDIDEQUIZZ_AUTH_MODE = "local", OPENAI_API_KEY = ""))
	err <- expect_error(llm_cle_definir("ma cle"), class = "candidequizz_cle_invalide")
	expect_match(cq_message_utilisateur(err), "sk-", fixed = TRUE)
	expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
})

test_that("la version en ligne refuse toute clé collée dans l'application", {
	withr::local_envvar(c(CANDIDEQUIZZ_AUTH_MODE = "comptes", OPENAI_API_KEY = ""))
	expect_false(llm_cle_saisie_permise())
	expect_error(
		llm_cle_definir("sk-proj-AbCdEfGhIjKlMnOpQrStUv_123"),
		class = "candidequizz_cle_refusee"
	)
	expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
})

test_that("la fenêtre d'aide montre les 4 étapes et le schéma", {
	html <- as.character(cq_cle_guide_modal())
	expect_match(html, "cq-cle-schema", fixed = TRUE)
	expect_identical(lengths(regmatches(html, gregexpr("cq-ia-etape-num", html))), 4L)
	expect_match(html, "platform.openai.com/api-keys", fixed = TRUE)
})

test_that("l'écran de composition reçoit la clé et la garde hors du champ", {
	local_store()
	withr::local_envvar(c(CANDIDEQUIZZ_AUTH_MODE = "local", OPENAI_API_KEY = ""))
	compte <- shiny::reactiveVal(auth_utilisateur_local())

	shiny::testServer(mod_seance_config_server, args = list(demarrer = function(...) NULL, utilisateur = compte), {
		session$setInputs(profil = "libre", nb_questions = "10", generation = "ia_on", types = "qcm_single")
		expect_true(cle_manquante())
		expect_match(motif_blocage(), "Collez votre cl", fixed = TRUE)

		session$setInputs(cle_openai = "pas une cle", cle_utiliser = 1)
		expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
		expect_true(cle_manquante())

		session$setInputs(cle_openai = "sk-proj-AbCdEfGhIjKlMnOpQrStUv_123", cle_utiliser = 2)
		expect_identical(Sys.getenv("OPENAI_API_KEY"), "sk-proj-AbCdEfGhIjKlMnOpQrStUv_123")
		expect_false(cle_manquante())
		expect_null(motif_blocage())
	})
})
