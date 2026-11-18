test_that("tous les gabarits attendus sont livr\u00e9s avec le package", {
	gabarits <- c(
		"socle",
		"redacteur_systeme", "redacteur_invite",
		"verificateur_systeme", "verificateur_invite",
		"reparateur_systeme", "reparateur_invite",
		"synthetiseur_systeme", "synthetiseur_invite"
	)

	for (nom in gabarits) {
		expect_true(file.exists(prompt_path(nom)), info = nom)
		expect_true(nchar(prompt_lire(nom)) > 50L, info = nom)
	}
})

test_that("un gabarit absent \u00e9choue avec un message explicite", {
	expect_error(prompt_path("inexistant"), class = "candidequizz_prompt_absent")
})

test_that("une substitution oubli\u00e9e est d\u00e9tect\u00e9e", {
	# Un `<<socle>>` non remplacé partirait tel quel au modèle : mieux vaut
	# échouer bruyamment que d'envoyer un prompt tronqué.
	expect_error(
		prompt_rendre("redacteur_systeme"),
		class = "candidequizz_prompt_incomplet"
	)

	expect_error(prompt_rendre("redacteur_systeme"), regexp = "socle")
})

test_that("les prompts syst\u00e8me int\u00e8grent le socle commun", {
	for (nom in c("redacteur_systeme", "verificateur_systeme", "reparateur_systeme", "synthetiseur_systeme")) {
		rendu <- prompt_rendre(nom, socle = prompt_socle())

		expect_false(grepl("<<", rendu, fixed = TRUE), info = nom)
		expect_match(rendu, "naturalisation", info = nom)
	}
})

test_that("le socle \u00e9nonce les libert\u00e9s et les interdits de la section 2.2", {
	socle <- prompt_socle()

	expect_match(socle, "guide")
	expect_match(socle, "[Rr]eformuler")
	expect_match(socle, "objectif d'apprentissage")
})

test_that("le bloc de graine transmet la mati\u00e8re factuelle au mod\u00e8le", {
	consigne <- consigne_test()
	bloc <- prompt_bloc_graine(consigne$graine)

	expect_match(bloc, "TEST-001", fixed = TRUE)
	expect_match(bloc, consigne$graine$objectif, fixed = TRUE)
	expect_match(bloc, "Libert\u00e9, \u00c9galit\u00e9, Fraternit\u00e9", fixed = TRUE)
	expect_match(bloc, "CORRECTE", fixed = TRUE)

	# Les erreurs typiques documentées doivent parvenir au rédacteur : ce sont
	# elles qui rattachent chaque distracteur à une confusion identifiée.
	expect_match(bloc, "confusion_regime_vichy", fixed = TRUE)
})

test_that("les options de r\u00e9f\u00e9rence sont m\u00e9lang\u00e9es dans le bloc de graine", {
	consigne <- consigne_test()
	textes <- consigne$graine$options[[1L]]$texte
	expect_true(length(textes) >= 3L)

	ordres <- replicate(40L, {
		bloc <- prompt_bloc_graine(consigne$graine)
		lignes <- grep("^  - ", strsplit(bloc, "\n", fixed = TRUE)[[1L]], value = TRUE)
		vapply(textes, function(t) which(grepl(t, lignes, fixed = TRUE))[[1L]], integer(1))
	})

	# Toutes les options restent présentes à chaque tirage.
	expect_true(all(apply(ordres, 2L, function(o) setequal(o, seq_along(textes)))))
	# Et l'ordre du classeur n'est pas figé.
	expect_true(length(unique(apply(ordres, 2L, paste, collapse = "-"))) > 1L)
})

test_that("le bloc de consignes num\u00e9rote et d\u00e9crit chaque item attendu", {
	consignes <- list(
		consigne_test(numero = 1L, type = "qcm_single", n_options = 4L),
		consigne_test(numero = 2L, type = "qcm_single", n_options = 4L)
	)

	bloc <- prompt_bloc_consignes(consignes)

	expect_match(bloc, "Consigne num\u00e9ro 1")
	expect_match(bloc, "Consigne num\u00e9ro 2")
	expect_match(bloc, "4 options")
})

test_that("l'invite du r\u00e9dacteur est compl\u00e8te et sans marqueur r\u00e9siduel", {
	consignes <- list(consigne_test())

	invite <- prompt_rendre(
		"redacteur_invite",
		nb_items = 1L,
		type_libelle = question_type("qcm_single")$libelle,
		rappel_variete = "",
		bloc_consignes = prompt_bloc_consignes(consignes)
	)

	expect_false(grepl("<<", invite, fixed = TRUE))
	expect_match(invite, "QCM \u00e0 option unique", fixed = TRUE)
})

test_that("le rappel de vari\u00e9t\u00e9 n'appara\u00eet que si une graine est r\u00e9p\u00e9t\u00e9e", {
	unique_graine <- list(consigne_test())
	expect_identical(redacteur_rappel_variete(unique_graine), "")

	repetee <- consigne_test()
	repetee$graine$occurrences_total <- 2L

	expect_match(redacteur_rappel_variete(list(repetee)), "change nettement d'angle")
})

test_that("le bloc de v\u00e9rification pr\u00e9sente l'item et son objectif", {
	bloc <- prompt_bloc_item(item_test("qcm_single"))

	expect_match(bloc, "Item num\u00e9ro 1")
	expect_match(bloc, "Objectif de la graine")
	expect_match(bloc, "Charles de Gaulle", fixed = TRUE)
})

test_that("le bloc de r\u00e9paration transmet le motif du rejet", {
	rejet <- conformite_rejet(
		item = item_test("qcm_single"),
		consigne = consigne_test(n_options = 4L),
		motifs = "3 options fournies au lieu de 4.",
		origine = "conformite"
	)

	bloc <- prompt_bloc_rejet(rejet)

	expect_match(bloc, "Motif du rejet")
	expect_match(bloc, "3 options fournies au lieu de 4", fixed = TRUE)
	expect_match(bloc, "4 options")
})
