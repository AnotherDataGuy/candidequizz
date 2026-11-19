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

test_that("boutons de l'en-tête : retours pour les comptes, administration pour l'admin seul", {
	local_store()
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "comptes")
	local_mocked_bindings(async_preparer = function(...) invisible(FALSE))

	shiny::testServer(app_server, {
		expect_error(output$retour_bouton, class = "shiny.silent.error")
		expect_error(output$admin_bouton, class = "shiny.silent.error")

		utilisateur(list(user_id = "u-test", username = "test", is_admin = FALSE, token = NULL))
		session$flushReact()
		expect_match(output$retour_bouton$html, 'id="retour-ouvrir"', fixed = TRUE)
		expect_error(output$admin_bouton, class = "shiny.silent.error")

		utilisateur(list(user_id = "u-admin", username = "admin", is_admin = TRUE, token = NULL))
		session$flushReact()
		expect_match(output$admin_bouton$html, 'id="cq_admin_ouvrir"', fixed = TRUE)
	})
})

test_that("version à télécharger : ni bouton de retours ni administration", {
	local_store()
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "local")
	local_mocked_bindings(async_preparer = function(...) invisible(FALSE))

	shiny::testServer(app_server, {
		session$flushReact()
		expect_error(output$retour_bouton)
		expect_error(output$admin_bouton, class = "shiny.silent.error")
	})
})
