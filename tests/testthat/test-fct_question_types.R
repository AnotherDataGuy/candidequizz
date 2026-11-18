test_that("les cinq types annonc\u00e9s au cahier des charges sont enregistr\u00e9s", {
	expect_setequal(
		question_type_ids(),
		c("qcm_single", "qcm_multiple", "vrai_faux", "ordre", "texte_a_trous")
	)
})

test_that("le mode classique n'expose que le QCM \u00e0 option unique", {
	expect_identical(question_type_ids(mode = "qcm_classique"), "qcm_single")
	expect_true(length(question_type_ids(mode = "autres_types")) >= 5L)
})

test_that("chaque type respecte le contrat du registre", {
	for (id in question_type_ids()) {
		type <- question_type(id)

		expect_true(nzchar(type$libelle), info = id)
		expect_true(all(type$modes %in% cq_modes()), info = id)
		expect_true(is.function(type$schema), info = id)
		expect_true(is.function(type$valider), info = id)
		expect_true(is.function(type$noter), info = id)
		expect_true(is.numeric(type$temps_ref_s) && type$temps_ref_s > 0, info = id)

		# Le schéma doit être constructible pour toutes les valeurs annoncées.
		valeurs <- type$n_options %||% list(NULL)

		for (n in valeurs) {
			expect_s3_class(type$schema(n), "ellmer::TypeObject")
		}
	}
})

test_that("un type inconnu \u00e9choue avec un message qui liste les types connus", {
	expect_error(question_type("inexistant"), class = "candidequizz_qtype_inconnu")
	expect_error(question_type("inexistant"), regexp = "qcm_single")
})

test_that("le registre refuse un type au contrat incomplet", {
	withr::defer({
		question_type_reset()
		enregistrer_types_questions()
	})

	expect_error(
		register_question_type(
			id = "faux",
			libelle = "Faux",
			modes = "autres_types",
			schema = function(n_options) NULL,
			consigne = function(consigne) "",
			valider = function(item) NULL, # `consigne` manquant
			noter = function(item, reponse) NULL
		),
		class = "candidequizz_qtype_contrat_invalide"
	)

	expect_error(
		register_question_type(
			id = "faux",
			libelle = "Faux",
			modes = "mode_inexistant",
			schema = function(n_options) NULL,
			consigne = function(consigne) "",
			valider = function(item, consigne) NULL,
			noter = function(item, reponse) NULL
		),
		class = "candidequizz_qtype_contrat_invalide"
	)
})

test_that("les libell\u00e9s de choix sont exploitables par un input Shiny", {
	choix <- question_type_choix(mode = "autres_types")

	expect_true(all(nzchar(names(choix))))
	expect_setequal(unname(choix), question_type_ids(mode = "autres_types"))
})

test_that("un type jouable expose une interface et un serveur", {
	# Garde-fou de planification : chaque lot qui rend un type jouable doit
	# l'ajouter ici. Le Lot 4 y ajoutera les quatre autres types.
	expect_setequal(question_type_ids(jouable = TRUE), question_type_ids())

	for (id in question_type_ids(jouable = TRUE)) {
		type <- question_type(id)

		expect_true(is.function(type$ui), info = id)
		expect_true(is.function(type$server), info = id)
	}
})
