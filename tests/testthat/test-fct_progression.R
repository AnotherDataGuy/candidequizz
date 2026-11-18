# Carnet de progression à débloquer.

test_that("le carnet reste fermé sous le seuil et s'ouvre au seuil", {
	local_store()
	expect_identical(progression_etat()$n, 0L)
	expect_false(progression_etat()$debloque)

	seuil <- progression_seuil()
	expect_identical(seuil, 200L)

	local_mocked_bindings(store_lister = function(user_id = NULL) {
		tibble::tibble(nb_questions = c(20L, 20L), n_repondues = c(20L, seuil - 20L))
	})
	etat <- progression_etat()
	expect_identical(etat$n, seuil)
	expect_true(etat$debloque)
	expect_identical(etat$taille, 20L)
})

test_that("les séances restantes s'arrondissent à la séance supérieure", {
	expect_identical(progression_seances_restantes(136L, 200L, 20L), 4L)
	expect_identical(progression_seances_restantes(190L, 200L, 20L), 1L)
	expect_identical(progression_seances_restantes(250L, 200L, 20L), 0L)
})

test_that("la jauge montre dix cases et le gain de la séance", {
	etat <- list(n = 136L, seuil = 200L, taille = 20L, debloque = FALSE)
	html <- as.character(cq_jauge_ui(etat, gain = 20L))

	expect_identical(lengths(regmatches(html, gregexpr("cq-jauge-case\"", html))), 10L)
	expect_match(html, "Encore", fixed = TRUE)
	expect_match(html, "+20 cette s", fixed = TRUE)
	expect_match(as.character(cq_jauge_ui(etat, ouvert = TRUE)), "is-ouvert", fixed = TRUE)
})

test_that("l'IA s'ouvre au seuil de questions différentes, sans compter répétitions ni passées", {
	# Version à télécharger : pas de seuil (l'utilisateur paie avec sa clé).
	# Version en ligne : 400 questions différentes.
	expect_identical(ia_seuil(), 0L)
	withr::with_envvar(c(GOLEM_CONFIG_ACTIVE = "production"), {
		expect_identical(ia_seuil(), 400L)
	})
	r <- function(seed, passee = FALSE) list(seed_id = seed, passee = passee)

	local_mocked_bindings(
		ia_seuil = function() 3L,
		stats_charger = function(session_ids = NULL, user_id = NULL) {
			list(
				list(reponses = list(r("A"), r("B"), r("A"))),
				list(reponses = list(r("B"), r("C", passee = TRUE)))
			)
		}
	)
	etat <- ia_etat("u")
	expect_identical(etat$n, 2L)
	expect_false(etat$debloque)
	expect_true(ia_etat("u", admin = TRUE)$debloque)
	expect_match(as.character(cq_ia_surprise(list(seuil = 3L))), "IA est ouverte", fixed = TRUE)
})
