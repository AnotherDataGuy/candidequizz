# Enregistrement et agrégation des réponses.

test_that("une bonne réponse est notée et chronométrée", {
	item <- item_test()
	affiche <- Sys.time() - 12

	reponse <- reponse_nouvelle(
		item = item,
		reponse = "1",
		affiche_le = affiche,
		premiere_interaction_ms = 4200,
		n_changements = 2L
	)

	expect_true(reponse$is_correct)
	expect_identical(reponse$score, 1)
	expect_gt(reponse$duree_ms, 11000)
	expect_identical(reponse$premiere_interaction_ms, 4200)
	expect_identical(reponse$n_changements, 2L)
	expect_false(reponse$passee)
})

test_that("une mauvaise réponse remonte la confusion touchée", {
	reponse <- reponse_nouvelle(
		item = item_test(),
		reponse = "2",
		affiche_le = Sys.time()
	)

	expect_false(reponse$is_correct)
	expect_identical(reponse$score, 0)
	expect_identical(reponse$diagnostic_tags, "confusion_resistance")
})

test_that("une question passée vaut zéro sans appeler la notation du type", {
	# La notation des types refuserait une réponse `NULL` : une question passée
	# doit donc court-circuiter le calcul, pas s'y risquer.
	reponse <- reponse_nouvelle(
		item = item_test(),
		reponse = NULL,
		affiche_le = Sys.time(),
		passee = TRUE
	)

	expect_true(reponse$passee)
	expect_identical(reponse$score, 0)
	expect_false(reponse$is_correct)
	expect_identical(reponse$diagnostic_tags, character(0))
})

test_that("reponses_tibble renvoie un tableau vide mais typé sans réponse", {
	tableau <- reponses_tibble(list())

	expect_identical(nrow(tableau), 0L)
	expect_true(is.numeric(tableau$score))
	expect_true(is.logical(tableau$is_correct))
})

test_that("reponses_tibble convertit les durées en secondes", {
	reponses <- list(
		reponse_nouvelle(item_test(), "1", Sys.time() - 10, premiere_interaction_ms = 2000),
		reponse_nouvelle(item_test(numero = 2L), "2", Sys.time() - 4)
	)

	tableau <- reponses_tibble(reponses)

	expect_identical(nrow(tableau), 2L)
	expect_gt(tableau$duree_s[[1L]], 9)
	expect_identical(tableau$latence_s[[1L]], 2)
	expect_identical(tableau$is_correct, c(TRUE, FALSE))
})

test_that("les confusions sont comptées et triées par fréquence", {
	reponses <- list(
		reponse_nouvelle(item_test(), "2", Sys.time()),
		reponse_nouvelle(item_test(numero = 2L), "2", Sys.time()),
		reponse_nouvelle(item_test(numero = 3L), "3", Sys.time())
	)

	confusions <- reponses_confusions(reponses)

	expect_identical(confusions$diagnostic_tag[[1L]], "confusion_resistance")
	expect_identical(confusions$n[[1L]], 2L)
})

test_that("le bilan par sous-thème va du moins réussi au mieux réussi", {
	reponses <- list(
		reponse_nouvelle(item_test(sub_theme = "Institutions"), "1", Sys.time()),
		reponse_nouvelle(item_test(sub_theme = "Histoire", numero = 2L), "2", Sys.time()),
		reponse_nouvelle(item_test(sub_theme = "Histoire", numero = 3L), "3", Sys.time())
	)

	par_theme <- seance_bilan_par_theme(reponses_tibble(reponses))

	expect_identical(par_theme$sub_theme, c("Histoire", "Institutions"))
	expect_identical(par_theme$reussite, c(0, 1))
	expect_identical(par_theme$n, c(2L, 1L))
})

test_that("les durées du bilan sont lisibles", {
	expect_identical(as.character(seance_bilan_duree(42)), "42 s")
	expect_identical(as.character(seance_bilan_duree(125)), "2 min 5 s")
	expect_identical(seance_bilan_duree(NA_real_), "\u2014")
})

test_that("le libell\u00e9 d'une r\u00e9ponse reste lisible dans le bilan", {
	item <- item_test()
	juste <- reponse_nouvelle(item, reponse = "1", affiche_le = Sys.time())
	passee <- reponse_nouvelle(item, reponse = NULL, affiche_le = Sys.time(), passee = TRUE)

	expect_identical(reponse_libelle(juste), "1")
	expect_identical(reponse_libelle(passee), "Question pass\u00e9e")
})

test_that("l'export de s\u00e9ance ne contient que les cinq colonnes p\u00e9dagogiques", {
	item <- item_test(
		enonce = "Qui a lanc\u00e9 l'appel depuis Londres ?",
		theme = "Histoire",
		sub_theme = "1939-1945",
		explication = "Le g\u00e9n\u00e9ral de Gaulle."
	)
	reponse <- reponse_nouvelle(item, reponse = "1", affiche_le = Sys.time() - 5)
	seance <- list(items = list(item), reponses = list(reponse))

	tab <- seance_export_tableau(seance)

	expect_identical(
		names(tab),
		c(
			"La question",
			"Le th\u00e8me",
			"Les options de r\u00e9ponse",
			"R\u00e9ponse correcte",
			"Explication"
		)
	)
	expect_equal(nrow(tab), 1L)
	expect_identical(tab[["La question"]][[1L]], item$enonce)
	expect_identical(tab[["Le th\u00e8me"]][[1L]], "Histoire / 1939-1945")
	expect_match(tab[["Les options de r\u00e9ponse"]][[1L]], "Charles de Gaulle")
	expect_match(tab[["R\u00e9ponse correcte"]][[1L]], "Charles de Gaulle")
	expect_identical(tab[["Explication"]][[1L]], item$explication)

	# Aucune colonne technique / structure banque
	interdits <- c(
		"item_id", "seed_id", "full_theme_id", "difficulte", "score",
		"MAIN_QUESTION", "LEARNING_GOAL", "ANSWER_OPTIONS"
	)
	expect_length(intersect(names(tab), interdits), 0L)
})

test_that("item_options_texte couvre les types principaux", {
	expect_match(item_options_texte(item_test("qcm_single")), "Charles de Gaulle")
	expect_identical(item_options_texte(item_test("vrai_faux")), "1. Vrai | 2. Faux")
	expect_match(item_options_texte(item_test("ordre")), "R\u00e9volution")
	expect_match(item_options_texte(item_test("texte_a_trous")), "libert")
})
