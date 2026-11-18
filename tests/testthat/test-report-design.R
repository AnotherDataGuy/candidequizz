test_that("le bilan reprend le chrome visuel de Progression", {
	ui <- as.character(mod_seance_bilan_ui("bilan"))

	expect_match(ui, "cq-bilan cq-stats", fixed = TRUE)
	expect_match(ui, "bilan-rapport", fixed = TRUE)
})

test_that("la bande KPI accepte une mesure propre au bilan", {
	ui <- as.character(stats_report_kpis(
		score_pct = 80L,
		n_justes = 8L,
		n_reponses = 10L,
		n_seances = 1L,
		temps_median_s = 14,
		troisieme_valeur = "3 min 20 s",
		troisieme_libelle = "Dur\u00e9e"
	))

	expect_match(ui, "3 min 20 s", fixed = TRUE)
	expect_match(ui, "Dur\u00e9e", fixed = TRUE)
	expect_match(ui, "Temps m\u00e9dian", fixed = TRUE)
})
