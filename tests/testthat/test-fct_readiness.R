examen_readiness_test <- function(score, complet = TRUE) {
	n <- if (complet) 40L else 39L
	list(
		parametres = list(profil = "examen_blanc"),
		statut = "terminee",
		reponses = lapply(seq_len(n), function(i) list(is_correct = i <= score))
	)
}

test_that("la préparation n'est pas inventée sans examen blanc complet", {
	training <- examen_readiness_test(39L)
	training$parametres$profil <- "libre"
	incomplet <- examen_readiness_test(35L, complet = FALSE)

	indicateur <- readiness_estimer(list(training, incomplet))

	expect_identical(indicateur$niveau, "absent")
	expect_identical(indicateur$n_examens, 0L)
	expect_false(indicateur$calibre)
})

test_that("un seul examen reste un premier repère", {
	indicateur <- readiness_estimer(list(examen_readiness_test(34L)))

	expect_identical(indicateur$niveau, "premier_repere")
	expect_identical(indicateur$score_recent, 34L)
	expect_match(indicateur$message, "ne suffit pas")
	expect_false(indicateur$calibre)
})

test_that("plusieurs examens décrivent la régularité sans probabilité", {
	regulier <- readiness_estimer(list(
		examen_readiness_test(35L),
		examen_readiness_test(33L)
	))
	fragile <- readiness_estimer(list(
		examen_readiness_test(36L),
		examen_readiness_test(30L)
	))

	expect_identical(regulier$niveau, "regulier")
	expect_identical(regulier$n_reussis, 2L)
	expect_identical(fragile$niveau, "seuil_fragile")
	expect_false(regulier$calibre)
	expect_match(as.character(readiness_ui(regulier)), "non calibré")
})
