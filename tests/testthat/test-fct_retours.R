# Retours des participants, question par question (administration).

seance_retours <- function(id, user_id, justes, commentaire = NULL, note = 7L) {
	s <- list(
		session_id = id, user_id = user_id, parametres = list(profil = "libre"),
		items = list(
			list(item_id = paste0(id, "-q1"), indice_seance = 1L, enonce = "Question A ?"),
			list(item_id = paste0(id, "-q2"), indice_seance = 2L, enonce = "Question B ?")
		),
		reponses = list(
			list(item_id = paste0(id, "-q1"), indice_seance = 1L, seed_id = "GA-001",
					 is_correct = justes[[1L]], passee = FALSE, duree_ms = 8000),
			list(item_id = paste0(id, "-q2"), indice_seance = 2L, seed_id = "GB-002",
					 is_correct = justes[[2L]], passee = FALSE, duree_ms = 12000)
		),
		avis = list()
	)
	avis_enregistrer(s, paste0(id, "-q1"), note, commentaire = commentaire, moment = "finale")
}

test_that("le tableau réunit échecs, difficulté ressentie et commentaires par graine", {
	seances <- list(
		seance_retours("s1", "u1", c(FALSE, TRUE), commentaire = "Les choix se ressemblent", note = 8L),
		seance_retours("s2", "u2", c(FALSE, TRUE), note = 6L),
		seance_retours("s3", "u3", c(TRUE, TRUE))
	)
	t <- retours_tableau(seances, noms = c(u1 = "renard", u2 = "colibri"), statuts = c(GB = "x"), bank = NULL)

	expect_setequal(t$seed_id, c("GA-001", "GB-002"))
	a <- t[t$seed_id == "GA-001", ]
	expect_identical(a$n_reponses, 3L)
	expect_equal(a$taux_echec, 2 / 3)
	expect_equal(a$difficulte_ressentie, 7)
	expect_identical(a$n_commentaires, 1L)
	expect_identical(a$commentaires[[1L]], "renard : Les choix se ressemblent")
	expect_identical(a$question, "Question A ?")
	expect_equal(a$duree_mediane_s, 8)
	expect_identical(a$statut, "a_traiter")

	b <- t[t$seed_id == "GB-002", ]
	expect_identical(b$n_avis, 0L)
	expect_equal(b$taux_echec, 0)
})

test_that("filtres et tris de l'écran", {
	t <- tibble::tibble(
		seed_id = c("A", "B", "C"),
		n_reponses = c(5L, 5L, 1L),
		taux_echec = c(0.2, 0.8, NA),
		n_avis = c(1L, 0L, 2L),
		difficulte_ressentie = c(3, NA, 9),
		n_commentaires = c(0L, 0L, 2L),
		statut = c("a_traiter", "a_traiter", "traite")
	)

	expect_identical(retours_filtrer(t, "a_traiter", "commentaires", TRUE)$seed_id, "A")
	expect_identical(retours_filtrer(t, "tous", "commentaires", TRUE)$seed_id, c("C", "A"))
	expect_identical(retours_filtrer(t, "tous", "echec", FALSE)$seed_id, c("B", "A", "C"))
	expect_identical(retours_filtrer(t, "tous", "difficulte", FALSE)$seed_id, c("C", "A", "B"))
	expect_identical(retours_filtrer(t, "traite", "echec", FALSE)$seed_id, "C")
})

test_that("le statut « traité » est enregistré en base et réversible", {
	repertoire <- withr::local_tempdir()
	withr::local_envvar(c(CANDIDEQUIZZ_STORE_DIR = repertoire, CANDIDEQUIZZ_STORE_BACKEND = "sqlite"))
	store_sqlite_con()

	expect_identical(retours_statuts(), stats::setNames(character(0), character(0)))
	retours_marquer("GA-001", "traite")
	expect_identical(retours_statuts()[["GA-001"]], "traite")
	retours_marquer("GA-001", "a_traiter")
	expect_identical(retours_statuts()[["GA-001"]], "a_traiter")
	expect_error(retours_marquer("", "traite"), class = "candidequizz_retours_invalide")
})

test_that("l'export CSV rend inertes les commentaires", {
	t <- retours_tableau(
		list(seance_retours("s1", "u1", c(TRUE, TRUE), commentaire = "=HYPERLINK(\"x\")")),
		noms = c(u1 = "renard"), statuts = character(0), bank = NULL
	)
	csv <- retours_csv(t)
	expect_false("list" %in% vapply(csv, class, character(1)))
	expect_match(csv$commentaires[csv$seed_id == "GA-001"], "renard : =HYPERLINK", fixed = TRUE)
})

test_that("l'écran des retours n'affiche rien hors administrateur, et échappe les commentaires", {
	repertoire <- withr::local_tempdir()
	withr::local_envvar(c(CANDIDEQUIZZ_STORE_DIR = repertoire, CANDIDEQUIZZ_STORE_BACKEND = "sqlite"))
	store_sqlite_con()

	seances <- list(seance_retours("s1", "u1", c(FALSE, TRUE), commentaire = "<script>alert(1)</script>"))
	local_mocked_bindings(avis_seances_toutes = function() seances)

	compte <- shiny::reactiveVal(list(user_id = "u0", username = "ami", is_admin = FALSE))
	shiny::testServer(mod_admin_retours_server, args = list(utilisateur = compte), {
		expect_error(tableau())
		compte(list(user_id = "u9", username = "admin", is_admin = TRUE))
		session$setInputs(statut = "a_traiter", tri = "commentaires", avec_avis = TRUE)
		html <- output$liste$html
		expect_match(html, "GA-001", fixed = TRUE)
		expect_false(grepl("<script>alert", html, fixed = TRUE))
		expect_match(html, "&lt;script&gt;", fixed = TRUE)

		session$setInputs(marquer = list(seed = "GA-001", statut = "traite"))
		expect_identical(retours_statuts()[["GA-001"]], "traite")
		expect_match(output$liste$html, "Rien", fixed = TRUE)
	})
})
