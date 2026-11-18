test_that("une condition de cha\u00eene devient une phrase fran\u00e7aise", {
	pipeline <- rlang::error_cnd(
		class = "candidequizz_pipeline_sortie_invalide",
		message = "contrat rompu"
	)
	llm <- rlang::error_cnd(
		class = "candidequizz_llm_indisponible",
		message = "Error in ellmer::chat(): timeout"
	)

	attendu_pipeline <- cq_message_utilisateur(pipeline)
	attendu_llm <- cq_message_utilisateur(llm)

	expect_false(grepl("Error in", attendu_pipeline, fixed = TRUE))
	expect_false(grepl("Error in", attendu_llm, fixed = TRUE))
	expect_match(attendu_pipeline, "pr\u00e9paration", ignore.case = TRUE)
	expect_match(attendu_llm, "g\u00e9n\u00e9ration", ignore.case = TRUE)
})

test_that("une trace R brute est remplac\u00e9e par le repli", {
	message <- cq_message_utilisateur(simpleError("object 'x' not found"))

	expect_identical(message, cq_message_repli())
	expect_false(grepl("object 'x'", message, fixed = TRUE))
})

test_that("un param\u00e8tre invalide garde son motif fran\u00e7ais", {
	err <- tryCatch(
		parametres_seance(nb_questions = 15),
		error = function(e) e
	)

	message <- cq_message_utilisateur(err)

	expect_match(message, "Nombre de questions", fixed = TRUE)
	expect_false(grepl("Error in", message, fixed = TRUE))
})

test_that("un texte d\u00e9j\u00e0 propre est conserv\u00e9", {
	expect_identical(
		cq_message_utilisateur("Choisissez une r\u00e9ponse."),
		"Choisissez une r\u00e9ponse."
	)
})
