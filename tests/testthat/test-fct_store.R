# Persistance des séances. Les fabriques `local_store()` et `seance_test()`
# vivent dans helper-store.R.

test_that("store_dir respecte la variable d'environnement", {
	repertoire <- local_store()

	expect_identical(store_dir(), repertoire)
})

test_that("une séance enregistrée est relue à l'identique", {
	local_store()

	seance <- seance_test()
	store_sauver(seance)

	relue <- store_charger(seance$session_id)

	expect_identical(relue$session_id, seance$session_id)
	expect_identical(relue$parametres$nb_questions, seance$parametres$nb_questions)
	expect_identical(relue$statut, "en_cours")
})

test_that("charger une séance inexistante échoue avec un message clair", {
	local_store()

	expect_error(
		store_charger("inexistante"),
		class = "candidequizz_store_seance_absente"
	)
})

test_that("une séance venue d'un schéma futur est refusée", {
	local_store()

	seance <- seance_test()
	seance$version_schema <- store_version_schema() + 1L
	store_sauver(seance)

	expect_error(
		store_charger(seance$session_id),
		class = "candidequizz_store_schema_futur"
	)
})

test_that("store_lister trie de la plus récente à la plus ancienne", {
	local_store()

	ancienne <- seance_test()
	ancienne$cree_le <- Sys.time() - 3600
	store_sauver(ancienne)

	recente <- seance_test()
	store_sauver(recente)

	liste <- store_lister()

	expect_identical(nrow(liste), 2L)
	expect_identical(liste$session_id[[1L]], recente$session_id)
})

test_that("store_lister ignore un fichier corrompu sans perdre les autres", {
	repertoire <- local_store()

	seance <- seance_test()
	store_sauver(seance)
	writeLines("ceci n'est pas un rds", file.path(repertoire, "corrompue.rds"))

	liste <- store_lister()

	expect_identical(nrow(liste), 1L)
	expect_identical(liste$session_id[[1L]], seance$session_id)
})

test_that("store_supprimer renvoie FALSE sur une séance absente", {
	local_store()

	expect_false(store_supprimer("inexistante"))
})

test_that("une réponse rejouée remplace la précédente au lieu de s'ajouter", {
	local_store()

	seance <- seance_test()
	item <- item_test(numero = 1L)
	item$item_id <- "item-1"

	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(item, reponse = "1", affiche_le = Sys.time())
	)
	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(item, reponse = "2", affiche_le = Sys.time())
	)

	expect_length(seance$reponses, 1L)
	expect_identical(seance$reponses[[1L]]$reponse, "2")
})

test_that("la reprise se place sur la première question sans réponse", {
	local_store()

	seance <- seance_test()

	expect_identical(seance_position_reprise(seance), 1L)
	expect_false(seance_achevee(seance))

	item <- item_test(numero = 1L)
	item$item_id <- "item-1"
	seance <- seance_ajouter_reponse(
		seance,
		reponse_nouvelle(item, reponse = "1", affiche_le = Sys.time())
	)

	expect_identical(seance_position_reprise(seance), 2L)
	expect_false(seance_achevee(seance))
	expect_false(seance_closee(seance))

	abandonnee <- seance_clore(seance, "abandonnee")
	expect_false(seance_achevee(abandonnee))
	expect_true(seance_closee(abandonnee))
})

test_that("le chronom\u00e8tre expire les questions non r\u00e9pondues", {
	local_store()

	seance <- seance_test()
	seance$parametres$chronometre <- "global"
	seance$parametres$limite_s <- 60
	item <- item_test(numero = 1L)
	item$item_id <- "item-expire"
	seance$items <- list(item)

	expect_true(seance_chrono_actif(seance))
	expect_identical(seance_chrono_texte(75), "1:15")

	seance$chrono_ecoule_s <- 0
	seance$chrono_marque_le <- Sys.time() - 5
	seance <- seance_chrono_tick(seance, maintenant = Sys.time())
	expect_true(seance$chrono_ecoule_s >= 4)

	expiree <- seance_expirer_chrono(seance)
	expect_true(expiree$chrono_epuise)
	expect_identical(expiree$statut, "terminee")
	expect_true(expiree$reponses[[1L]]$passee)
})

test_that("seule une séance en cours et incomplète est proposée à la reprise", {
	local_store()

	expect_null(store_seance_reprenable())

	seance <- seance_test()
	store_sauver(seance)

	expect_identical(store_seance_reprenable()$session_id, seance$session_id)

	store_sauver(seance_clore(seance, "terminee"))

	expect_null(store_seance_reprenable())
})

test_that("les graines des s\u00e9ances r\u00e9centes sont \u00e9vit\u00e9es au red\u00e9marrage", {
	local_store()

	expect_identical(store_seeds_historiques(), character(0))
	expect_identical(seance_seeds_a_eviter(), character(0))

	ancienne <- seance_test()
	ancienne$cree_le <- Sys.time() - 3600
	ancienne$items <- list(
		list(seed_id = "REHF-RECH-001"),
		list(seed_id = "REHF-RECH-002")
	)
	store_sauver(ancienne)

	recente <- seance_test()
	recente$items <- list(list(seed_id = "PRVS-DEVI-001"))
	store_sauver(recente)

	expect_setequal(
		store_seeds_historiques(),
		c("REHF-RECH-001", "REHF-RECH-002", "PRVS-DEVI-001")
	)
	expect_identical(store_seeds_historiques(n_seances = 1L), "PRVS-DEVI-001")
	expect_identical(store_seeds_historiques(n_seances = 0L), character(0))
	expect_setequal(
		store_seeds_historiques(sauf_session_id = recente$session_id),
		c("REHF-RECH-001", "REHF-RECH-002")
	)

	courante <- seance_test()
	courante$items <- list(list(seed_id = "GEOG-CAP-001"))

	expect_setequal(
		seance_seeds_a_eviter(courante),
		c("REHF-RECH-001", "REHF-RECH-002", "PRVS-DEVI-001", "GEOG-CAP-001")
	)
})

test_that("une graine vue dans une s\u00e9ance pr\u00e9c\u00e9dente n'est pas ressaisie tant qu'il en reste", {
	local_store()

	deja <- seance_test()
	deja$items <- list(
		list(seed_id = "REHF-RECH-001"),
		list(seed_id = "REHF-RECH-002"),
		list(seed_id = "REHF-RECH-003")
	)
	store_sauver(deja)

	lot <- bank_seeds(
		1,
		full_theme_ids = "REHF-RECH",
		exclude_ids = seance_seeds_a_eviter()
	)

	# Le tirage est aléatoire parmi toutes les graines encore disponibles.
	expect_length(lot$seed_id, 1L)
	expect_false(lot$seed_id %in% seance_seeds_a_eviter())
	expect_true(lot$seed_id %in% bank_load()$graines$seed_id)
})
