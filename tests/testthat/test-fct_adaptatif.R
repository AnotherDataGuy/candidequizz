reponses <- function(...) {
	lapply(c(...), function(juste) list(is_correct = juste))
}

test_that("l'\u00e9tat initial part de la difficult\u00e9 configur\u00e9e", {
	etat <- adapt_etat_initial()

	expect_identical(etat$difficulte, as.integer(cq_config("adaptatif", "difficulte_initiale")))
	expect_identical(etat$erreurs_consecutives, 0L)
	expect_identical(etat$confiance_restantes, 0L)
})

test_that("un lot r\u00e9ussi fait monter d'un cran, un lot rat\u00e9 fait descendre", {
	depart <- adapt_etat_initial()

	monte <- adapt_maj(depart, reponses(TRUE, TRUE, TRUE, TRUE, TRUE))
	expect_identical(monte$difficulte, depart$difficulte + 1L)

	descend <- adapt_maj(depart, reponses(TRUE, FALSE, FALSE, FALSE, TRUE))
	expect_true(descend$difficulte < depart$difficulte)
})

test_that("un taux interm\u00e9diaire laisse la difficult\u00e9 inchang\u00e9e", {
	depart <- adapt_etat_initial()

	# 70 % de réussite, erreurs dispersées : entre le seuil de descente et celui
	# de montée, et sans série d'échecs qui déclencherait la remise en confiance.
	stable <- adapt_maj(
		depart,
		reponses(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
	)

	expect_identical(stable$difficulte, depart$difficulte)
	expect_identical(stable$confiance_restantes, 0L)
})

test_that("la difficult\u00e9 reste born\u00e9e par la configuration", {
	regles <- adapt_regles()
	etat <- adapt_etat_initial()

	for (i in seq_len(10L)) {
		etat <- adapt_maj(etat, reponses(TRUE, TRUE, TRUE, TRUE))
	}

	expect_identical(etat$difficulte, as.integer(regles$difficulte_max))

	for (i in seq_len(10L)) {
		etat <- adapt_maj(etat, reponses(FALSE, FALSE, FALSE, FALSE))
	}

	expect_identical(etat$difficulte, as.integer(regles$difficulte_min))
})

test_that("une s\u00e9rie d'erreurs cons\u00e9cutives d\u00e9clenche la remise en confiance", {
	regles <- adapt_regles()
	etat <- adapt_etat_initial()

	apres <- adapt_maj(etat, reponses(rep(FALSE, regles$erreurs_consecutives_max)))

	expect_identical(apres$difficulte, etat$difficulte - 1L)
	expect_identical(apres$confiance_restantes, as.integer(regles$questions_remise_en_confiance))
	expect_identical(apres$erreurs_consecutives, 0L)
})

test_that("la s\u00e9rie d'erreurs prime sur un taux de r\u00e9ussite honorable", {
	etat <- adapt_etat_initial()

	# 70 % de réussite, mais les trois dernières réponses sont fausses : le
	# découragement doit l'emporter sur la moyenne du lot.
	apres <- adapt_maj(
		etat,
		reponses(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
	)

	expect_true(apres$difficulte < etat$difficulte)
	expect_true(apres$confiance_restantes > 0L)
})

test_that("la difficult\u00e9 ne bouge pas pendant la remise en confiance", {
	etat <- adapt_etat_initial()
	etat$confiance_restantes <- 2L

	apres <- adapt_maj(etat, reponses(TRUE, TRUE, TRUE, TRUE, TRUE))

	expect_identical(apres$difficulte, etat$difficulte)
	expect_identical(apres$confiance_restantes, 0L)
})

test_that("le lot porte une question sonde un cran au-dessus", {
	etat <- adapt_etat_initial()
	difficultes <- adapt_difficulte_lot(etat, 5L)

	expect_length(difficultes, 5L)
	expect_true(all(difficultes[1:4] == etat$difficulte))
	expect_identical(difficultes[[5L]], etat$difficulte + 1L)
})

test_that("la remise en confiance place les questions faciles en t\u00eate", {
	etat <- adapt_etat_initial()
	etat$confiance_restantes <- 2L

	difficultes <- adapt_difficulte_lot(etat, 5L)

	expect_true(all(difficultes[1:2] < etat$difficulte))
	expect_true(all(difficultes[3:5] == etat$difficulte))
})

test_that("un \u00e9chantillon trop petit ne fait pas bouger le niveau, mais n'est pas perdu", {
	depart <- adapt_etat_initial()

	# 2 sur 2 ne prouve rien : le niveau attend.
	apres <- adapt_maj(depart, reponses(TRUE, TRUE))
	expect_identical(apres$difficulte, depart$difficulte)
	expect_length(apres$fenetre, 2L)

	# Les deux réponses comptent à la décision suivante.
	apres <- adapt_maj(apres, reponses(TRUE, TRUE))
	expect_identical(apres$difficulte, depart$difficulte + 1L)
	expect_length(apres$fenetre, 0L)
})

test_that("le score partiel compte dans le taux", {
	expect_equal(adapt_score(list(is_correct = FALSE, score = 0.5)), 0.5)
	expect_equal(adapt_score(list(is_correct = TRUE)), 1)
	expect_equal(adapt_score(list(is_correct = FALSE)), 0)

	# Quatre réponses justes et une à moitié juste : 90 %, le niveau monte.
	depart <- adapt_etat_initial()
	lot <- c(
		lapply(1:4, function(i) list(is_correct = TRUE, score = 1)),
		list(list(is_correct = FALSE, score = 0.5))
	)
	expect_identical(adapt_maj(depart, lot)$difficulte, depart$difficulte + 1L)
})

test_that("la palette suit le rang dans la s\u00e9ance, pas la taille de la tranche", {
	etat <- adapt_etat_initial()
	difficultes <- adapt_difficulte_lot(etat, 10L)

	expect_identical(difficultes[c(5L, 10L)], rep(etat$difficulte + 1L, 2L))
	expect_identical(difficultes[[7L]], etat$difficulte - 1L)
	expect_identical(sum(difficultes == etat$difficulte), 7L)

	# Les mêmes rangs découpés en tranches de 3 donnent la même palette.
	par_tranches <- c(
		adapt_difficulte_lot(etat, 3L, indices = 1:3),
		adapt_difficulte_lot(etat, 3L, indices = 4:6),
		adapt_difficulte_lot(etat, 4L, indices = 7:10)
	)
	expect_identical(par_tranches, difficultes)
})

test_that("l'aiguillage avance la question facile apr\u00e8s deux erreurs", {
	choix <- adapt_aiguiller(
		difficultes = c(3L, 3L, 2L, 4L),
		sous_themes = c("A", "B", "C", "D"),
		resultats = c(TRUE, FALSE, FALSE)
	)

	expect_identical(choix$rang, 3L)
	expect_identical(choix$motif, "facile")
})

test_that("l'aiguillage avance la sonde apr\u00e8s trois r\u00e9ussites", {
	choix <- adapt_aiguiller(
		difficultes = c(3L, 2L, 3L, 4L),
		sous_themes = c("A", "B", "C", "D"),
		resultats = c(TRUE, TRUE, TRUE)
	)

	expect_identical(choix$rang, 4L)
	expect_identical(choix$motif, "difficile")
})

test_that("l'aiguillage garde l'ordre pr\u00e9vu sans signal net", {
	expect_identical(
		adapt_aiguiller(c(3L, 2L, 4L), c("A", "B", "C"), c(TRUE, FALSE))$rang,
		1L
	)
	expect_identical(
		adapt_aiguiller(c(2L, 3L, 2L), c("A", "B", "C"), c(FALSE, FALSE))$rang,
		1L
	)
	expect_identical(adapt_aiguiller(3L, "A", c(FALSE, FALSE))$rang, 1L)
})

test_that("l'aiguillage \u00e9vite de reprendre le m\u00eame sous-th\u00e8me", {
	choix <- adapt_aiguiller(
		difficultes = c(3L, 2L, 2L),
		sous_themes = c("A", "B", "C"),
		resultats = c(FALSE, FALSE),
		dernier_sous_theme = "B"
	)

	expect_identical(choix$rang, 3L)
})

test_that("les poids th\u00e9matiques oublient un sous-th\u00e8me rattrap\u00e9", {
	base <- as.POSIXct("2026-09-01 10:00:00", tz = "UTC")
	anciennes <- lapply(seq_len(4L), function(i) {
		list(full_theme_id = "T1-S1", is_correct = FALSE, repondu_le = base + i)
	})
	recentes <- lapply(seq_len(3L), function(i) {
		list(full_theme_id = "T1-S1", is_correct = TRUE, repondu_le = base + 100 + i)
	})

	# Les récentes d'abord, comme les renvoie le stockage : l'ordre est rétabli.
	expect_identical(unname(adapt_poids_themes(c(recentes, anciennes), fenetre = 3L)[["T1-S1"]]), 1)
	expect_identical(unname(adapt_poids_themes(c(recentes, anciennes), fenetre = 7L)[["T1-S1"]]), 2)
})

test_that("un lot trop court ne porte pas de sonde", {
	etat <- adapt_etat_initial()

	expect_identical(adapt_difficulte_lot(etat, 2L), rep(etat$difficulte, 2L))
	expect_identical(adapt_difficulte_lot(etat, 0L), integer(0))
})

test_that("l'ordre anti-lassitude limite les r\u00e9p\u00e9titions de sous-th\u00e8me", {
	sous_themes <- c("A", "A", "A", "A", "B", "B", "C", "C")
	ordre <- adapt_ordre_anti_lassitude(sous_themes, maxi = 2L)

	expect_setequal(ordre, seq_along(sous_themes))

	suite <- sous_themes[ordre]
	series <- rle(suite)$lengths

	expect_true(max(series) <= 2L)
})

test_that("un sous-th\u00e8me rat\u00e9 est surpond\u00e9r\u00e9, un sous-th\u00e8me ma\u00eetris\u00e9 vite est d\u00e9pond\u00e9r\u00e9", {
	reponses <- list(
		list(
			full_theme_id = "fragile",
			is_correct = FALSE,
			type = "qcm_single",
			duree_ms = 20000
		),
		list(
			full_theme_id = "fragile",
			is_correct = FALSE,
			type = "qcm_single",
			duree_ms = 25000
		),
		list(
			full_theme_id = "maitrise",
			is_correct = TRUE,
			type = "qcm_single",
			duree_ms = 4000
		),
		list(
			full_theme_id = "maitrise",
			is_correct = TRUE,
			type = "qcm_single",
			duree_ms = 5000
		)
	)

	poids <- adapt_poids_themes(reponses)

	expect_identical(unname(poids[["fragile"]]), 2)
	expect_identical(unname(poids[["maitrise"]]), 0.5)
	expect_null(adapt_poids_themes(list(list(is_correct = TRUE))))
})

test_that("les types exigents n'apparaissent qu'en mode vari\u00e9 et \u00e0 haut niveau", {
	etat <- adapt_etat_initial()
	etat$difficulte <- 4L

	classique <- adapt_ajuster_types(
		c("qcm_single", "qcm_single"),
		etat,
		"qcm_single"
	)

	expect_identical(classique, c("qcm_single", "qcm_single"))

	varie <- adapt_ajuster_types(
		c("vrai_faux", "qcm_single"),
		etat,
		c("vrai_faux", "qcm_single", "ordre")
	)

	expect_true("ordre" %in% varie)

	etat$confiance_restantes <- 2L
	doux <- adapt_ajuster_types(
		c("ordre", "qcm_multiple"),
		etat,
		c("vrai_faux", "ordre", "qcm_multiple")
	)

	expect_true("vrai_faux" %in% doux)
})

test_that("l'ordre anti-lassitude n'\u00e9choue pas quand la r\u00e9p\u00e9tition est in\u00e9vitable", {
	# Un seul sous-thème : la contrainte ne peut pas être respectée, la fonction
	# doit renvoyer une permutation complète plutôt que de s'arrêter.
	sous_themes <- rep("A", 5L)
	ordre <- adapt_ordre_anti_lassitude(sous_themes, maxi = 2L)

	expect_setequal(ordre, seq_along(sous_themes))
	expect_length(ordre, 5L)
})

test_that("une confusion répétée renforce le sous-thème", {
	reponses <- lapply(seq_len(4L), function(i) {
		list(
			is_correct = i > 2L,
			full_theme_id = "T1-S1",
			type = "qcm_single",
			duree_ms = 25000,
			diagnostic_tags = if (i <= 2L) "confusion_institutions" else character(0)
		)
	})

	expect_identical(unname(adapt_poids_themes(reponses)[["T1-S1"]]), 2.5)

	reponses[[2L]]$diagnostic_tags <- "autre_confusion"
	expect_identical(unname(adapt_poids_themes(reponses)[["T1-S1"]]), 2)
})
