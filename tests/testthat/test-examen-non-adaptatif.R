# L'examen blanc ne s'adapte pas aux réponses données pendant la tentative.

test_that("seuls les profils d'apprentissage sont adaptatifs", {
	expect_true(parametres_adaptatif_actif(list(profil = "libre")))
	expect_true(parametres_adaptatif_actif(list(profil = "revision_ciblee")))
	expect_false(parametres_adaptatif_actif(list(profil = "examen_blanc")))
	expect_false(parametres_seance(profil = "examen_blanc")$adaptatif)
})

test_that("le régleur ne modifie ni la difficulté ni les types en examen blanc", {
	ctx <- list(
		parametres = list(profil = "examen_blanc"),
		lot = 2L,
		plan = list(
			indices_lots = list(1:2, 3:4),
			types = rep("qcm_single", 4L),
			etat_adaptatif = list(difficulte = 3L)
		),
		historique = list(reponses = list(list(is_correct = FALSE)))
	)

	sortie <- agent_regleur_adaptatif(ctx)

	expect_false(sortie$plan$directive$adaptatif)
	expect_null(sortie$plan$directive$poids_themes)
	expect_identical(sortie$plan$directive$difficulte, 3L)
	expect_identical(sortie$plan$types, ctx$plan$types)
	expect_identical(sortie$plan$etat_adaptatif, ctx$plan$etat_adaptatif)
})
