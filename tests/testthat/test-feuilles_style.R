# Feuilles de style : une par écran, numérotées, chargées dans l'ordre.

test_that("les feuilles de style sont numérotées et courtes", {
	fichiers <- cq_feuilles_style()

	expect_gt(length(fichiers), 1L)
	expect_identical(fichiers[[1L]], "01-jetons.css")
	expect_true(all(grepl("^[0-9]{2}-[a-z0-9-]+\\.css$", fichiers)))

	lignes <- vapply(
		fichiers,
		function(f) length(readLines(app_sys("app/www/css", f), warn = FALSE)),
		integer(1)
	)
	expect_true(all(lignes <= 1000L), info = paste(names(lignes)[lignes > 1000L], collapse = ", "))
})

test_that("la page charge toutes les feuilles, dans l'ordre", {
	local_store()
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "local")

	rendu <- htmltools::renderTags(app_ui(NULL))
	html <- paste(rendu$head, rendu$html)
	positions <- vapply(
		cq_feuilles_style(),
		function(f) regexpr(paste0("www/css/", f), html, fixed = TRUE)[[1L]],
		integer(1)
	)

	expect_true(all(positions > 0L))
	expect_false(is.unsorted(positions))
})
