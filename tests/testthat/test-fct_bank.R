bank <- bank_load()

test_that("le classeur est embarqué dans le package", {
	expect_true(file.exists(bank_path()))
	expect_match(bank_version(), "^[0-9a-f]{32}$")
})

test_that("le classeur respecte le schéma attendu", {
	expect_true(all(
		c("graines", "referentiel", "version", "chemin", "lu_le") %in% names(bank)
	))

	expect_true(all(
		c(
			"seed_id", "full_theme_id", "theme_id", "theme", "sub_theme_id",
			"sub_theme", "question_ref", "objectif", "explication",
			"difficulte_graine", "cadre", "options", "erreurs"
		) %in% names(bank$graines)
	))

	expect_true(all(bank$graines$difficulte_graine %in% 1:10))
	expect_true(all(nzchar(trimws(bank$graines$explication))))
	expect_identical(nrow(bank$graines), 584L)
	expect_true(all(bank$referentiel$full_theme_id %in% bank$graines$full_theme_id))

	expect_true(all(
		c("full_theme_id", "theme_id", "theme", "sub_theme_id", "sub_theme", "cadre") %in%
			names(bank$referentiel)
	))
})

test_that("les identifiants sont uniques et l'intégrité référentielle est assurée", {
	expect_false(any(duplicated(bank$graines$seed_id)))
	expect_false(any(duplicated(bank$referentiel$full_theme_id)))
	expect_true(all(bank$graines$full_theme_id %in% bank$referentiel$full_theme_id))
})

test_that("aucun champ pédagogique n'est vide", {
	for (colonne in c("seed_id", "question_ref", "objectif", "explication", "cadre")) {
		valeurs <- bank$graines[[colonne]]
		expect_false(anyNA(valeurs), info = colonne)
		expect_true(all(nzchar(trimws(valeurs))), info = colonne)
	}
})

test_that("chaque graine porte des options exploitables", {
	for (i in seq_len(nrow(bank$graines))) {
		options <- bank$graines$options[[i]]
		seed_id <- bank$graines$seed_id[[i]]

		expect_identical(nrow(options), 4L, info = seed_id)
		expect_identical(sum(options$is_correct), 1L, info = seed_id)
		expect_false(any(duplicated(options$texte)), info = seed_id)
		expect_identical(nrow(bank$graines$erreurs[[i]]), 3L, info = seed_id)
	}
})

test_that("le repli de la banque affiche et note quatre options", {
	consigne <- consigne_test(n_options = 4L)
	consigne$graine <- bank$graines[1L, ]
	item <- item_depuis_graine(consigne)
	options <- item$payload$options
	corrects <- vld_corrects(options)

	expect_length(options, 4L)
	expect_length(qtype_qcm_single_choix(item)$noms, 4L)
	expect_setequal(qtype_qcm_single_choix(item)$valeurs, as.character(1:4))
	expect_true(qtype_qcm_single_valider(item, consigne)$ok)
	expect_identical(qtype_qcm_single_noter(item, which(corrects))$score, 1)
	expect_identical(qtype_qcm_single_noter(item, which(!corrects)[[1L]])$score, 0)
})

test_that("bank_themes et bank_sub_themes sont cohérents avec les graines", {
	themes <- bank_themes(bank)

	expect_identical(sum(themes$n_sous_themes), nrow(bank$referentiel))
	expect_identical(sum(themes$n_graines), nrow(bank$graines))
	expect_false(any(duplicated(themes$theme_id)))

	sous_themes <- bank_sub_themes(bank = bank)

	expect_identical(nrow(sous_themes), nrow(bank$referentiel))
	expect_identical(sum(sous_themes$n_graines), nrow(bank$graines))

	un_theme <- bank_sub_themes(themes$theme_id[[1]], bank = bank)

	expect_identical(nrow(un_theme), themes$n_sous_themes[[1]])
	expect_error(
		bank_sub_themes("THEME_IMAGINAIRE", bank = bank),
		class = "candidequizz_bank_theme_inconnu"
	)
})

test_that("bank_valider_colonnes signale les colonnes manquantes", {
	expect_error(
		bank_valider_colonnes(data.frame(a = 1), c("a", "b"), "feuille"),
		class = "candidequizz_bank_schema_invalide"
	)
	expect_silent(bank_valider_colonnes(data.frame(a = 1), "a", "feuille"))
})

test_that("bank_parser_options valide le contenu JSON", {
	valide <- '[{"option_id":"A","text":"Oui","is_correct":true},
	            {"option_id":"B","text":"Non","is_correct":false}]'

	options <- bank_parser_options(valide, "TEST-001")

	expect_identical(nrow(options), 2L)
	expect_identical(names(options), c("option_id", "texte", "is_correct"))
	expect_true(options$is_correct[[1]])

	expect_error(
		bank_parser_options("{ ceci n'est pas du json", "TEST-002"),
		class = "candidequizz_bank_json_invalide"
	)
	expect_error(
		bank_parser_options(NA_character_, "TEST-003"),
		class = "candidequizz_bank_schema_invalide"
	)
	expect_error(
		bank_parser_options('[{"option_id":"A","text":"Seule","is_correct":true}]', "TEST-004"),
		class = "candidequizz_bank_schema_invalide"
	)
	expect_error(
		bank_parser_options(
			'[{"option_id":"A","text":"Oui","is_correct":false},
			  {"option_id":"B","text":"Non","is_correct":false}]',
			"TEST-005"
		),
		class = "candidequizz_bank_schema_invalide"
	)
})

test_that("bank_parser_erreurs tolère une cellule vide", {
	vide <- bank_parser_erreurs(NA_character_, "TEST-001")

	expect_identical(nrow(vide), 0L)
	expect_identical(names(vide), c("mistake_id", "diagnostic_tag", "description"))
})

test_that("bank_quotas répartit exactement l'effectif demandé", {
	ids <- c("A", "B", "C")

	for (n in c(1L, 5L, 10L, 60L)) {
		quotas <- bank_quotas(n, ids)

		expect_identical(sum(quotas), n)
		expect_identical(names(quotas), ids)
		expect_true(all(quotas >= 0))
		# Répartition égale : l'écart entre sous-thèmes ne dépasse jamais 1.
		expect_lte(max(quotas) - min(quotas), 1L)
	}

	expect_length(bank_quotas(10, character(0)), 0L)
})

test_that("bank_quotas surpondère les sous-thèmes fragiles", {
	quotas <- bank_quotas(10, c("A", "B", "C"), c(A = 1, B = 1, C = 3))

	expect_identical(sum(quotas), 10L)
	expect_gt(quotas[["C"]], quotas[["A"]])

	# Un poids absent ou aberrant ne doit pas casser la répartition.
	quotas_partiels <- bank_quotas(10, c("A", "B"), c(A = 2))

	expect_identical(sum(quotas_partiels), 10L)
})

test_that("bank_seeds fournit exactement le nombre de graines demandé", {
	for (n in c(1L, 5L, 10L, 20L, 40L, 60L)) {
		lot <- bank_seeds(n, bank = bank)

		expect_identical(nrow(lot), n)
		expect_true(all(lot$seed_id %in% bank$graines$seed_id))
		expect_true(all(c("occurrence", "occurrences_total") %in% names(lot)))
	}
})

test_that("bank_seeds respecte la sélection thématique", {
	lot <- bank_seeds(12, full_theme_ids = c("REHF-RECH", "GEOG-REMI"), bank = bank)

	expect_identical(nrow(lot), 12L)
	expect_setequal(unique(lot$full_theme_id), c("REHF-RECH", "GEOG-REMI"))

	expect_error(
		bank_seeds(5, full_theme_ids = "SOUS-THEME-IMAGINAIRE", bank = bank),
		class = "candidequizz_bank_theme_inconnu"
	)
})

test_that("bank_seeds réutilise les graines quand le vivier est trop petit", {
	# Cas limite dimensionnant : 60 questions sur un sous-thème de 8 graines.
	petite_banque <- bank
	graines <- bank$graines[bank$graines$full_theme_id == "REHF-RECH", ]
	petite_banque$graines <- graines[seq_len(8L), ]
	lot <- bank_seeds(60, full_theme_ids = "REHF-RECH", bank = petite_banque)

	expect_identical(nrow(lot), 60L)
	expect_identical(length(unique(lot$seed_id)), 8L)
	expect_identical(max(lot$occurrence), 8L)

	# occurrence numérote bien les répétitions d'une même graine.
	par_graine <- split(lot$occurrence, lot$seed_id)

	for (occurrences in par_graine) {
		expect_setequal(occurrences, seq_along(occurrences))
	}

	expect_true(all(lot$occurrences_total %in% 7:8))
})

test_that("bank_seeds privilégie les graines non encore vues", {
	ids <- bank$graines$seed_id[bank$graines$full_theme_id == "REHF-RECH"]
	deja_vues <- setdiff(ids, "REHF-RECH-004")

	lot <- bank_seeds(
		1,
		full_theme_ids = "REHF-RECH",
		exclude_ids = deja_vues,
		bank = bank
	)

	expect_identical(lot$seed_id, "REHF-RECH-004")
	expect_false(lot$seed_id %in% deja_vues)
})

test_that("bank_seeds tourne vers les sous-th\u00e8mes encore frais", {
	ids <- unique(bank$graines$full_theme_id)
	expect_gte(length(ids), 12L)

	premiers <- ids[seq_len(10L)]
	vus <- bank$graines$seed_id[bank$graines$full_theme_id %in% premiers]

	lot <- bank_seeds(10, exclude_ids = vus, bank = bank)

	expect_false(any(lot$full_theme_id %in% premiers))
	expect_identical(nrow(lot), 10L)
})

test_that("bank_seeds privil\u00e9gie les graines proches de la difficult\u00e9 cible", {
	lot <- bank_seeds(
		4,
		full_theme_ids = "REHF-RECH",
		difficulte = 1L,
		bank = bank
	)

	expect_lte(median(lot$difficulte_graine), 4)

	lot_haut <- bank_seeds(
		4,
		full_theme_ids = "REHF-RECH",
		difficulte = 5L,
		bank = bank
	)

	expect_gte(median(lot_haut$difficulte_graine), 6)
	expect_identical(bank_difficulte_cible(1L), 1L)
	expect_identical(bank_difficulte_cible(3L), 6L)
	expect_identical(bank_difficulte_cible(5L), 10L)
})

test_that("l'explication de la banque est privil\u00e9gi\u00e9e apr\u00e8s assemblage", {
	item <- item_depuis_graine(consigne_test())

	expect_match(item$explication, "Libert")
	expect_false(identical(item$explication, item$objectif))

	llm <- item_depuis_llm(
		brut = list(
			enonce = "Quelle devise ?",
			explication = "Texte invent\u00e9 par le mod\u00e8le.",
			options = list(
				list(texte = "A", is_correct = TRUE, diagnostic_tag = ""),
				list(texte = "B", is_correct = FALSE, diagnostic_tag = "x")
			)
		),
		consigne = consigne_test()
	)

	expect_match(llm$explication, "Libert")
	expect_false(grepl("invent", llm$explication, fixed = TRUE))
})

test_that("bank_seeds rejette les appels invalides", {
	expect_error(bank_seeds(0, bank = bank), class = "candidequizz_bank_appel_invalide")
	expect_error(bank_seeds(-1, bank = bank), class = "candidequizz_bank_appel_invalide")
})

test_that("le cache de la banque est réutilisé puis invalidable", {
	expect_identical(bank_load()$version, bank$version)

	bank_invalidate()

	expect_null(.bank_cache$bank)
	expect_identical(bank_load()$version, bank$version)
})
