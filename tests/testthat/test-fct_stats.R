# Agrégations du Lot 6, hors interface.

test_that("un p\u00e9rim\u00e8tre vide ne casse aucune agr\u00e9gation", {
	local_store()

	expect_length(stats_charger(), 0L)
	expect_identical(nrow(stats_reponses(list())), 0L)
	expect_identical(stats_synthese(stats_reponses(list()), 0L)$n_questions, 0L)
	expect_identical(nrow(stats_par(stats_reponses(list()), "theme")), 0L)
	expect_identical(nrow(stats_evolution(list())), 0L)
})

test_that("la synth\u00e8se et le d\u00e9coupage suivent les r\u00e9ponses enregistr\u00e9es", {
	local_store()

	seance <- seance_test(nb_questions = 10L)
	seance$items <- list(
		item_test(type = "qcm_single", item_id = "a", theme = "Histoire", sub_theme = "Dates"),
		item_test(type = "vrai_faux", item_id = "b", theme = "Institutions", sub_theme = "Pr\u00e9sident")
	)
	seance$items[[2L]]$type <- "vrai_faux"
	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(seance$items[[1L]], reponse = "1", affiche_le = Sys.time() - 20)
	)
	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(seance$items[[2L]], reponse = TRUE, affiche_le = Sys.time() - 8)
	)
	store_sauver(seance)

	chargees <- stats_charger(seance$session_id)
	reponses <- stats_reponses(chargees)

	expect_length(chargees, 1L)
	expect_identical(nrow(reponses), 2L)

	synthese <- stats_synthese(reponses, n_seances = 1L)

	expect_identical(synthese$n_seances, 1L)
	expect_identical(synthese$n_questions, 2L)
	expect_true(synthese$reussite >= 0 && synthese$reussite <= 1)

	par_theme <- stats_par(reponses, "theme")

	expect_setequal(par_theme$cle, c("Histoire", "Institutions"))
	expect_true(all(par_theme$n == 1L))

	evo <- stats_evolution(chargees)

	expect_identical(nrow(evo), 1L)
	expect_identical(evo$session_id[[1L]], seance$session_id)
})

test_that("le snapshot de s\u00e9ance ne porte aucune r\u00e9ponse brute", {
	local_store()

	seance <- seance_test()
	item <- item_test(item_id = "a", theme = "Histoire", sub_theme = "Dates")
	seance$items <- list(item)
	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(item, reponse = "1", affiche_le = Sys.time() - 12)
	)

	snapshot <- stats_snapshot_seance(seance)

	expect_false("reponse" %in% names(snapshot))
	expect_identical(snapshot$n_questions, 1L)
	expect_true(nzchar(stats_snapshot_texte(snapshot)))
})

test_that("le repli du synth\u00e9tiseur produit trois points de chaque c\u00f4t\u00e9", {
	seance <- seance_test()
	item <- item_test(item_id = "a", theme = "Histoire", sub_theme = "Dates")
	seance$items <- list(item)
	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(item, reponse = "1", affiche_le = Sys.time() - 12)
	)

	syn <- synthetiseur_repli(stats_snapshot_seance(seance))

	expect_length(syn$points_forts, 3L)
	expect_length(syn$points_a_travailler, 3L)
	expect_true(nzchar(syn$diagnostic))
	expect_identical(syn$source, "repli")
})

test_that("les exports CSV et HTML s'\u00e9crivent", {
	reponses <- reponses_tibble(list(reponse_nouvelle(
		item_test(),
		reponse = "1",
		affiche_le = Sys.time() - 10
	)))
	reponses$session_id <- "s1"
	reponses$cree_le <- Sys.time()

	csv <- tempfile(fileext = ".csv")
	html <- tempfile(fileext = ".html")
	xlsx <- tempfile(fileext = ".xlsx")

	stats_export_csv(reponses, csv)
	stats_export_html(
		titre = "Test",
		synthese = stats_synthese(reponses, 1L),
		par_theme = stats_par(reponses, "theme"),
		par_sous_theme = stats_par(reponses, "sub_theme"),
		par_type = stats_par(reponses, "type"),
		chemin = html
	)
	stats_export_xlsx(reponses, xlsx)

	expect_true(file.exists(csv))
	expect_true(file.exists(html))
	expect_true(file.exists(xlsx))
	expect_match(paste(readLines(html, encoding = "UTF-8"), collapse = "\n"), "Test")
})

test_that("la lecture d\u00e9gage un point faible et un \u00e9cart", {
	local_store()

	expect_identical(stats_chaleur_ton(NA_real_), "vide")
	expect_identical(stats_chaleur_ton(0.2), "bas")
	expect_identical(stats_chaleur_ton(0.9), "haut")

	vide <- stats_lecture(stats_reponses(list()), list())
	expect_true(is.na(vide$delta))
	expect_true(is.na(vide$tendance))
	expect_null(vide$faible)
	expect_null(vide$progres)
	expect_identical(vide$n_justes, 0L)
	expect_null(stats_progres(stats_reponses(list())))
})

test_that("le progr\u00e8s retient le sous-th\u00e8me qui monte le plus", {
	local_store()

	item_a <- item_test(item_id = "a", theme = "Histoire", sub_theme = "Dates")
	item_b <- item_test(item_id = "b", theme = "Histoire", sub_theme = "Dates")
	premiere <- seance_test()
	premiere$items <- list(item_a)
	premiere$cree_le <- Sys.time() - 3600
	premiere <- seance_ajouter_reponse(
		premiere,
		reponse_nouvelle(item_a, reponse = "2", affiche_le = Sys.time() - 3700)
	)
	store_sauver(premiere)

	seconde <- seance_test()
	seconde$items <- list(item_b)
	seconde$cree_le <- Sys.time()
	seconde <- seance_ajouter_reponse(
		seconde,
		reponse_nouvelle(item_b, reponse = "1", affiche_le = Sys.time() - 12)
	)
	store_sauver(seconde)

	chargees <- stats_charger()
	reponses <- stats_reponses(chargees)
	lecture <- stats_lecture(reponses, chargees)
	progres <- stats_progres(reponses)

	expect_false(is.null(progres))
	expect_identical(progres$cle[[1L]], "Dates")
	expect_true(progres$delta[[1L]] > 0)
	expect_identical(lecture$n_justes + lecture$n_fausses + lecture$n_partielles + lecture$n_passees, nrow(reponses))
})

test_that("l'\u00e9chelle et les issues des graphiques restent stables", {
	expect_equal(stats_echelle(0, c(0, 1), c(0, 100)), 0)
	expect_equal(stats_echelle(1, c(0, 1), c(0, 100)), 100)
	expect_equal(stats_echelle(0.5, c(0, 1), c(10, 30)), 20)
	expect_equal(stats_echelle(3, c(3, 3), c(0, 10)), 5)

	reponses <- tibble::tibble(
		is_correct = c(TRUE, FALSE, FALSE, FALSE),
		score = c(1, 0.5, 0, 0),
		passee = c(FALSE, FALSE, FALSE, TRUE)
	)

	expect_identical(
		stats_issue(reponses),
		c("juste", "partielle", "fausse", "passee")
	)
	expect_identical(stats_date_courte(as.POSIXct("2026-09-14 10:00:00")), "14/09")
	expect_identical(stats_ton_couleur(0.2), "#e9bcab")
	expect_identical(stats_tronque("abc"), "abc")
	expect_true(grepl("\u2026$", stats_tronque(strrep("a", 30), 10L)))
})
