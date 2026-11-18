# Version en ligne : l'interface principale n'est envoyée qu'après la connexion.

test_that("en mode comptes, la page d'accueil ne contient pas l'interface", {
	local_store()
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "comptes")

	html <- as.character(htmltools::renderTags(app_ui(NULL))$html)

	expect_match(html, 'id="cq_principale"', fixed = TRUE)
	expect_no_match(html, 'id="cq_section"', fixed = TRUE)
	expect_no_match(html, 'id="cq-dock"', fixed = TRUE)
})

test_that("en mode local, l'interface est dans la page, sans sortie à la demande", {
	local_store()
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "local")

	html <- as.character(htmltools::renderTags(app_ui(NULL))$html)

	expect_match(html, 'id="cq_section"', fixed = TRUE)
	expect_no_match(html, 'id="cq_principale"', fixed = TRUE)
})

test_that("le serveur n'envoie l'interface qu'à un compte connecté", {
	local_store()
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "comptes")
	# Pas de processus d'arrière-plan pendant ce test.
	local_mocked_bindings(async_preparer = function(...) invisible(FALSE))

	shiny::testServer(app_server, {
		# Sans compte : req() arrête le rendu, rien n'est envoyé.
		expect_error(output$cq_principale, class = "shiny.silent.error")

		utilisateur(list(user_id = "u-test", username = "test", is_admin = FALSE, token = NULL))
		session$flushReact()

		expect_match(output$cq_principale$html, 'id="cq_section"', fixed = TRUE)
	})
})
