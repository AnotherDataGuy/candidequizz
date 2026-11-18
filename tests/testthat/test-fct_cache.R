# Cache de questions vérifiées.

item_verifie <- function(consigne, enonce = "Qui a lanc\u00e9 l'appel du 18 juin 1940 ?") {
	item <- item_test(
		"qcm_single",
		numero = consigne$numero,
		difficulte = consigne$difficulte,
		enonce = enonce,
		seed_id = consigne$graine$seed_id,
		n_options = 3L
	)
	item$empreinte_graine <- cache_empreinte(consigne$graine)
	item$verifie <- TRUE
	item
}

test_that("seules les questions r\u00e9dig\u00e9es et v\u00e9rifi\u00e9es entrent en cache", {
	local_store()
	consigne <- consigne_test(numero = 1L, difficulte = 3L, n_options = 3L)
	verifie <- item_verifie(consigne)
	non_relu <- item_verifie(consigne, enonce = "Autre formulation non relue ?")
	non_relu$verifie <- NULL
	repli <- item_depuis_graine(consigne)

	cache_integrer(list(verifie, non_relu, repli))
	entrees <- cache_lire()

	expect_length(entrees, 1L)
	expect_identical(entrees[[1L]]$item$enonce, verifie$enonce)
	expect_identical(entrees[[1L]]$n_servi, 1L)
})

test_that("une question en cache est resservie, et son usage compt\u00e9", {
	local_store()
	consigne <- consigne_test(numero = 1L, difficulte = 3L, n_options = 3L)
	cache_integrer(list(item_verifie(consigne)))
	entrees <- cache_lire()

	# Une seule version, déjà servie : on en écrit d'abord une seconde.
	expect_null(cache_choisir(entrees, consigne, variantes_max = 2L))

	# Une fois les versions constituées, elles tournent.
	choisie <- cache_choisir(entrees, consigne, variantes_max = 1L)
	expect_false(is.null(choisie))

	autre <- utils::modifyList(consigne, list(numero = 4L, indice_seance = 9L, lot = 3L))
	item <- item_depuis_cache(choisie, autre)
	expect_identical(item$source, "cache")
	expect_identical(item$numero, 4L)
	expect_identical(item$indice_seance, 9L)
	expect_true(item$verifie)

	cache_integrer(list(item))
	expect_identical(cache_lire()[[1L]]$n_servi, 2L)
})

test_that("une graine modifi\u00e9e dans le classeur rend ses questions invisibles", {
	local_store()
	consigne <- consigne_test(numero = 1L, difficulte = 3L, n_options = 3L)
	cache_integrer(list(item_verifie(consigne)))

	modifiee <- consigne
	modifiee$graine$explication <- "Nouvelle explication de r\u00e9f\u00e9rence."

	expect_null(cache_choisir(cache_lire(), modifiee, variantes_max = 1L))
})

test_that("le cache respecte type, options, v\u00e9rit\u00e9, difficult\u00e9 et exclusions", {
	local_store()
	consigne <- consigne_test(numero = 1L, difficulte = 3L, n_options = 3L)
	cache_integrer(list(item_verifie(consigne)))
	entrees <- cache_lire()

	expect_null(cache_choisir(entrees, utils::modifyList(consigne, list(n_options = 4L)), variantes_max = 1L))
	expect_null(cache_choisir(entrees, utils::modifyList(consigne, list(type = "vrai_faux")), variantes_max = 1L))
	expect_null(cache_choisir(entrees, utils::modifyList(consigne, list(difficulte = 5L)), variantes_max = 1L))
	expect_false(is.null(cache_choisir(entrees, utils::modifyList(consigne, list(difficulte = 4L)), variantes_max = 1L)))
	expect_null(cache_choisir(entrees, consigne, exclure_ids = entrees[[1L]]$cache_id, variantes_max = 1L))
})

test_that("\u00e0 n_servi \u00e9gal, le cache tire au sort parmi les variantes", {
	local_store()
	consigne <- consigne_test(numero = 1L, difficulte = 3L, n_options = 3L)
	cache_integrer(list(
		item_verifie(consigne, enonce = "Qui a lanc\u00e9 l'appel du 18 juin 1940 ?"),
		item_verifie(consigne, enonce = "Qui a appel\u00e9 \u00e0 la r\u00e9sistance depuis Londres ?")
	))
	entrees <- cache_lire()
	expect_length(entrees, 2L)
	expect_identical(
		vapply(entrees, function(e) as.integer(e$n_servi), integer(1)),
		c(1L, 1L)
	)

	ids <- replicate(30L, cache_choisir(entrees, consigne, variantes_max = 1L)$cache_id)
	expect_true(length(unique(ids)) > 1L)
})

test_that("le r\u00e9dacteur n'\u00e9crit que ce que le cache n'a pas couvert", {
	local_store()
	withr::local_envvar(c(OPENAI_API_KEY = "test"))

	ctx <- pipeline_ctx(parametres = parametres_seance(nb_questions = 10, generation = "ia_on"), cache = cache_parametres())
	ctx$consignes <- lapply(1:2, function(i) {
		consigne_test(numero = i, indice_seance = i, difficulte = 3L, n_options = 3L)
	})
	ctx$consignes[[2L]]$graine$seed_id <- "TEST-002"

	# Deux versions en cache pour la première graine : assez pour les faire tourner.
	cache_integrer(list(
		item_verifie(ctx$consignes[[1L]]),
		item_verifie(ctx$consignes[[1L]], enonce = "Qui a appel\u00e9 \u00e0 la r\u00e9sistance depuis Londres ?")
	))

	invites <- character(0)
	testthat::local_mocked_bindings(
		llm_appel_structure = function(agent, role, systeme, invite, schema, ...) {
			invites <<- c(invites, invite)
			list(
				donnees = list(items = list(list(
					numero = 2L,
					enonce = "Quelle est la devise de la R\u00e9publique ?",
					options = list(
						list(texte = "Libert\u00e9, \u00c9galit\u00e9, Fraternit\u00e9", is_correct = TRUE, diagnostic_tag = ""),
						list(texte = "Travail, Famille, Patrie", is_correct = FALSE, diagnostic_tag = "confusion_vichy"),
						list(texte = "Unit\u00e9, Justice, Libert\u00e9", is_correct = FALSE, diagnostic_tag = "confusion")
					),
					explication = "Explication simul\u00e9e suffisamment longue pour passer."
				))),
				ok = TRUE,
				message = NA_character_,
				journal = llm_ligne_journal(agent = agent, role = role, modele = "gpt-4.1-mini")
			)
		}
	)

	ctx <- run_agent("cache_questions", ctx)
	expect_length(ctx$items, 1L)

	ctx <- run_agent("redacteur", ctx)

	expect_length(invites, 1L)
	expect_false(grepl("num\u00e9ro 1", invites[[1L]], fixed = TRUE))
	expect_identical(vapply(ctx$items, function(i) i$source, character(1)), c("cache", "llm"))
})
