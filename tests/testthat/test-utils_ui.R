test_that("les sections de la navbar sont bien définies", {
	sections <- cq_sections()

	expect_length(sections, 4L)

	ids <- vapply(sections, function(s) s$id, character(1))

	expect_identical(ids, c("accueil", "seances", "statistiques", "ressources"))
	expect_false(any(duplicated(ids)))

	for (section in sections) {
		expect_true(nzchar(section$libelle))
		expect_true(nzchar(section$icone))
	}
})

test_that("le dock expose un bouton par section, avec une seule section active", {
	html <- as.character(cq_bottom_nav(active = "seances"))

	for (section in cq_sections()) {
		expect_match(html, cq_nav_input_id(section$id), fixed = TRUE)
	}

	expect_identical(
		lengths(regmatches(html, gregexpr("action-button", html))),
		4L
	)
	expect_identical(
		lengths(regmatches(html, gregexpr("is-actif", html))),
		1L
	)
	expect_match(html, "data-section=\"seances\"", fixed = TRUE)
})

test_that("l'interface complète se construit sans erreur", {
	expect_no_error(html <- as.character(app_ui(NULL)))

	expect_identical(
		lengths(regmatches(html, gregexpr("data-section", html))),
		4L
	)
})

test_that("le thème est un thème bslib de Bootstrap 5", {
	theme <- cq_theme()

	expect_s3_class(theme, "bs_theme")
	expect_identical(bslib::theme_version(theme), "5")
})

test_that("l'en-tête expose le bouton de thème", {
	html <- as.character(cq_entete())

	expect_match(html, "cq-theme-toggle", fixed = TRUE)
	expect_match(html, "theme-toggle", fixed = TRUE)
	expect_match(html, "i-moon", fixed = TRUE)
	expect_match(html, "i-sun", fixed = TRUE)
	expect_match(html, "Pourquoi Candide", fixed = TRUE)
	expect_match(html, "wordmark", fixed = TRUE)
	expect_match(html, "candide", fixed = TRUE)

	# Le bouton thème est à gauche de « Pourquoi Candide »
	pos_theme <- regexpr("cq-theme-toggle", html, fixed = TRUE)[[1L]]
	pos_why <- regexpr("Pourquoi Candide", html, fixed = TRUE)[[1L]]
	expect_true(pos_theme > 0L && pos_why > pos_theme)
})

test_that("chaque thème de la banque a une illustration", {
	themes <- bank_themes()
	for (i in seq_len(nrow(themes))) {
		src <- cq_theme_art_src(theme_id = themes$theme_id[[i]], theme = themes$theme[[i]])
		expect_true(nzchar(src %||% ""), info = themes$theme_id[[i]])
		chemin <- app_sys("app/www", sub("^www/", "", src))
		expect_true(file.exists(chemin), info = src)
	}
})
