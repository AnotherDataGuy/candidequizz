test_that("sans historique, la r\u00e9vision cibl\u00e9e est impossible", {
	local_store()

	prep <- revision_preparer()

	expect_false(prep$ok)
	expect_length(prep$full_theme_ids, 0L)
})

test_that("la r\u00e9vision retient les sous-th\u00e8mes les plus fragiles", {
	local_store()
	bank <- bank_load()
	faible <- bank$referentiel$full_theme_id[[1L]]
	fort <- bank$referentiel$full_theme_id[[2L]]

	reponse <- function(id, score) {
		list(
			item_id = paste0(id, "-", score),
			indice_seance = 1L,
			lot = 1L,
			type = "qcm_single",
			difficulte = 3L,
			full_theme_id = id,
			theme = "th\u00e8me",
			sub_theme = id,
			score = score,
			is_correct = score >= 1,
			duree_ms = 2000,
			premiere_interaction_ms = 400,
			n_changements = 0L,
			passee = FALSE
		)
	}

	seance <- seance_test()
	seance$reponses <- list(
		reponse(faible, 0),
		reponse(faible, 0),
		reponse(fort, 1),
		reponse(fort, 1)
	)
	store_sauver(seance_clore(seance, "terminee"))

	prep <- revision_preparer(n = 1L)

	expect_true(prep$ok)
	expect_identical(prep$full_theme_ids, faible)
	expect_true(prep$poids_themes[[faible]] > 1)
})

test_that("les poids de s\u00e9ance l'emportent sur le profil", {
	base <- c(a = 2, b = 2)
	session <- c(a = 0.5)

	fusion <- revision_fusionner_poids(base, session)

	expect_equal(fusion[["a"]], 0.5)
	expect_equal(fusion[["b"]], 2)
	expect_null(revision_fusionner_poids(NULL, NULL))
	expect_identical(revision_fusionner_poids(base, NULL), base)
})
