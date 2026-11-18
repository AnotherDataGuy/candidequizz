test_that("le fichier des liens utiles est présent et lisible", {
	expect_true(file.exists(ressources_path()))

	groupes <- ressources_read()

	expect_gt(length(groupes), 0L)
})

test_that("chaque groupe et chaque lien sont complets", {
	groupes <- ressources_read()
	ids <- vapply(groupes, function(g) g$id, character(1))

	expect_false(any(duplicated(ids)))

	for (groupe in groupes) {
		expect_true(nzchar(groupe$titre))
		expect_gt(length(groupe$liens), 0L)

		for (lien in groupe$liens) {
			expect_true(nzchar(lien$titre))
			# Les ressources officielles doivent être servies en HTTPS.
			expect_match(lien$url, "^https://", info = lien$titre)
		}
	}
})

test_that("ressources_read signale un fichier invalide", {
	fichier <- withr::local_tempfile(fileext = ".yml")

	writeLines("autre_cle: 1", fichier)
	expect_error(
		ressources_read(fichier),
		class = "candidequizz_ressources_invalides"
	)

	writeLines(
		c("groupes:", "  - id: essai", "    titre: Essai"),
		fichier
	)
	expect_error(
		ressources_read(fichier),
		class = "candidequizz_ressources_invalides"
	)

	writeLines(
		c(
			"groupes:",
			"  - id: essai",
			"    titre: Essai",
			"    liens:",
			"      - titre: Sans url"
		),
		fichier
	)
	expect_error(
		ressources_read(fichier),
		class = "candidequizz_ressources_invalides"
	)
})

test_that("le rendu d'un lien produit une ancre sécurisée", {
	html <- as.character(ressources_lien_ui(list(
		titre = "Essai",
		url = "https://example.org",
		source = "source",
		description = "description"
	)))

	expect_match(html, "https://example.org", fixed = TRUE)
	expect_match(html, "noopener", fixed = TRUE)
	expect_match(html, "_blank", fixed = TRUE)
	expect_match(html, "res-sceau", fixed = TRUE)
	expect_identical(ressources_domaine("https://www.service-public.gouv.fr/x"), "service-public.gouv.fr")
	expect_identical(ressources_sigle("https://www.legifrance.gouv.fr/"), "lf.")
})

test_that("les liens essentiels passent en tête et quittent leur dossier", {
	groupes <- ressources_read()
	liens <- unlist(lapply(groupes, function(g) g$liens), recursive = FALSE)
	expect_gt(sum(vapply(liens, function(l) isTRUE(l$essentiel), logical(1))), 0L)

	html <- as.character(ressources_groupe_ui(groupes[[1L]], 1L))
	expect_no_match(html, "F39426", fixed = TRUE)
	expect_identical(ressources_sceau("https://www.legifrance.gouv.fr/"), "LF")
})
