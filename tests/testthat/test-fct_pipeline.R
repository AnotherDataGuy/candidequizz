# Registre d'agents, contrats et exécution de la chaîne.

test_that("les huit agents du cahier des charges sont enregistr\u00e9s", {
	expect_setequal(
		agent_ids(),
		c(
			"planificateur", "regleur_adaptatif", "selecteur_graines", "cache_questions", "redacteur",
			"conformite", "verificateur_factuel", "reparateur", "synthetiseur"
		)
	)
	expect_identical(pipeline_lot()[[1L]], "regleur_adaptatif")
	expect_false("synthetiseur" %in% pipeline_lot())
})

test_that("chaque agent LLM r\u00e9f\u00e9rence un r\u00f4le connu et un mod\u00e8le tarif\u00e9", {
	synthese <- agent_synthese()
	llm <- synthese[synthese$nature == "llm", ]

	expect_true(nrow(llm) >= 3L)
	expect_true(all(llm$role %in% cq_llm_roles()))
	expect_true(all(!is.na(llm$modele)))
	expect_true(all(vapply(llm$modele, function(m) cq_llm_prix(m)$entree > 0, logical(1))))

	deterministes <- synthese[synthese$nature == "deterministe", ]
	expect_true(all(is.na(deterministes$modele)))
})

test_that("la cha\u00eene de lot est ordonn\u00e9e de fa\u00e7on coh\u00e9rente avec les contrats", {
	# Chaque agent doit trouver dans le contexte ce qu'il exige, compte tenu de
	# ce que les agents précédents produisent. C'est la relecture du schéma
	# d'agents, faite par le code.
	disponibles <- c("parametres", "bank", "lot", "plan")

	for (id in c(pipeline_seance(), pipeline_lot())) {
		a <- agent(id)

		expect_true(
			all(a$requiert %in% disponibles),
			info = glue::glue(
				"{id} exige {paste(setdiff(a$requiert, disponibles), collapse = ', ')}"
			)
		)

		disponibles <- union(disponibles, a$produit)
	}
})

test_that("le registre refuse un agent au contrat invalide", {
	withr::defer({
		agent_reset()
		enregistrer_agents()
	})

	expect_error(
		register_agent("x", "X", nature = "magique", fn = function(ctx) ctx),
		class = "candidequizz_agent_contrat_invalide"
	)

	expect_error(
		register_agent("x", "X", nature = "deterministe", fn = function(contexte) contexte),
		class = "candidequizz_agent_contrat_invalide"
	)

	expect_error(
		register_agent(
			"x", "X",
			nature = "llm", role = "role_inexistant", fn = function(ctx) ctx
		),
		class = "candidequizz_agent_contrat_invalide"
	)
})

test_that("un contrat d'entr\u00e9e non satisfait \u00e9choue imm\u00e9diatement", {
	withr::defer({
		agent_reset()
		enregistrer_agents()
	})

	register_agent(
		id = "exigeant",
		libelle = "Exigeant",
		nature = "deterministe",
		fn = function(ctx) ctx,
		requiert = "champ_absent"
	)

	ctx <- pipeline_ctx(parametres = parametres_seance())

	expect_error(
		run_agent("exigeant", ctx),
		class = "candidequizz_pipeline_contrat_non_satisfait"
	)
})

test_that("un contrat de sortie non tenu \u00e9choue", {
	withr::defer({
		agent_reset()
		enregistrer_agents()
	})

	register_agent(
		id = "oublieux",
		libelle = "Oublieux",
		nature = "deterministe",
		fn = function(ctx) ctx,
		produit = "plan"
	)

	ctx <- pipeline_ctx(parametres = parametres_seance())

	expect_error(
		run_agent("oublieux", ctx),
		class = "candidequizz_pipeline_sortie_invalide"
	)
})

test_that("un agent inconnu \u00e9choue avec la liste des agents connus", {
	expect_error(agent("inexistant"), class = "candidequizz_agent_inconnu")
	expect_error(agent("inexistant"), regexp = "redacteur")
})

test_that("la trace compte une ligne par agent ex\u00e9cut\u00e9", {
	ctx <- pipeline_ctx(parametres = parametres_seance())
	ctx <- run_pipeline(ctx, pipeline_seance())

	trace <- pipeline_trace(ctx)

	expect_identical(nrow(trace), 1L)
	expect_identical(trace$agent, "planificateur")
	expect_true(trace$duree_s >= 0)
})

# --- Planificateur ---------------------------------------------------------

test_that("le d\u00e9coupage en lots somme au nombre de questions demand\u00e9", {
	for (n in cq_config("seance", "nb_questions_possibles")) {
		tailles <- plan_tailles_lots(n)

		expect_identical(sum(tailles), as.integer(n), info = as.character(n))
		expect_true(all(tailles > 0L), info = as.character(n))
	}
})

test_that("la s\u00e9ance s'\u00e9crit par petites tranches, pour d\u00e9marrer vite", {
	tailles <- plan_tailles_lots(40)

	expect_identical(tailles[[1L]], as.integer(cq_config("seance", "taille_premier_lot")))
	expect_identical(tailles, as.integer(c(rep(3L, 12L), 4L)))
	expect_identical(plan_tailles_lots(40, premier = 5, courant = 10, minimum = 3), c(5L, 10L, 10L, 10L, 5L))
})

test_that("un reliquat trop petit est absorb\u00e9 plut\u00f4t que servi seul", {
	minimum <- as.integer(cq_config("seance", "taille_lot_min"))

	for (n in c(6, 7, 12, 23, 31)) {
		tailles <- plan_tailles_lots(n)

		expect_identical(sum(tailles), as.integer(n), info = as.character(n))
		expect_true(all(tailles >= minimum), info = as.character(n))
	}
})

test_that("le plan couvre exactement toutes les questions de la s\u00e9ance", {
	ctx <- pipeline_ctx(parametres = parametres_seance(nb_questions = 20, mode = "autres_types"))
	ctx <- run_agent("planificateur", ctx)
	plan <- ctx$plan

	expect_identical(sum(plan$tailles_lots), 20L)
	expect_setequal(unlist(plan$indices_lots), 1:20)
	expect_length(plan$types, 20L)
	expect_length(plan$n_options, 20L)
})

test_that("chaque type demand\u00e9 appara\u00eet dans la r\u00e9partition", {
	types <- question_type_ids(mode = "autres_types")
	repartition <- plan_repartir_types(20L, types)

	expect_length(repartition, 20L)
	expect_setequal(unique(repartition), types)

	# Un seul type demandé : toutes les questions le portent.
	expect_identical(unique(plan_repartir_types(10L, "qcm_single")), "qcm_single")
})

# --- Sélecteur de graines -------------------------------------------------

test_that("le s\u00e9lecteur produit une consigne compl\u00e8te par question du lot", {
	ctx <- pipeline_ctx(parametres = parametres_seance(nb_questions = 20, mode = "autres_types"))
	ctx <- run_pipeline(ctx, c("planificateur", "selecteur_graines"))

	expect_length(ctx$consignes, ctx$plan$tailles_lots[[1L]])

	for (consigne in ctx$consignes) {
		expect_true(consigne$type %in% question_type_ids())
		expect_true(consigne$difficulte >= 1L && consigne$difficulte <= 5L)
		expect_identical(nrow(consigne$graine), 1L)
		expect_true(nzchar(consigne$graine$seed_id))
		expect_true(nzchar(consigne$graine$objectif))
	}

	numeros <- vapply(ctx$consignes, function(c) c$numero, integer(1))
	expect_setequal(numeros, seq_along(ctx$consignes))
})

test_that("le s\u00e9lecteur \u00e9quilibre les valeurs de v\u00e9rit\u00e9 des vrai-faux", {
	# Sans cette alternance, le générateur produit surtout des affirmations
	# vraies et l'utilisateur apprend à répondre « vrai » sans réfléchir.
	verites <- selecteur_verites(1:20)

	expect_identical(sum(verites), 10L)
})

test_that("le s\u00e9lecteur refuse un num\u00e9ro de lot hors du plan", {
	ctx <- pipeline_ctx(parametres = parametres_seance(nb_questions = 10), lot = 99L)
	ctx <- run_agent("planificateur", ctx)

	expect_error(
		run_agent("selecteur_graines", ctx),
		class = "candidequizz_pipeline_lot_inconnu"
	)
})

test_that("les consignes se regroupent par type sans perte", {
	consignes <- list(
		consigne_test(numero = 1L, type = "qcm_single"),
		consigne_test(numero = 2L, type = "vrai_faux"),
		consigne_test(numero = 3L, type = "qcm_single")
	)

	groupes <- consignes_par_type(consignes)

	expect_setequal(names(groupes), c("qcm_single", "vrai_faux"))
	expect_length(groupes$qcm_single, 2L)
	expect_identical(sum(lengths(groupes)), 3L)
})

# --- Chaîne complète en mode dégradé --------------------------------------

test_that("sans cl\u00e9 d'API, la cha\u00eene produit un lot complet \u00e0 partir des graines", {
	withr::local_envvar(c(OPENAI_API_KEY = ""))

	parametres <- parametres_seance(nb_questions = 10)
	ctx <- generer_lot(parametres, lot = 1L)

	expect_true(ctx$degrade)
	expect_length(ctx$items, ctx$plan$tailles_lots[[1L]])
	expect_true(all(vapply(ctx$items, function(i) i$source == "graine_repli", logical(1))))
	expect_length(ctx$rejets, 0L)

	# Mode dégradé : aucun appel réseau, donc aucun coût.
	expect_identical(llm_synthese_journal(ctx$journal_llm)$n_appels, 0L)

	# Les items de repli doivent être conformes, sinon la séance s'arrêterait.
	for (item in ctx$items) {
		consigne <- conformite_consigne(ctx$consignes, item$numero)
		verdict <- conformite_valider(item, consigne)

		expect_true(verdict$ok, info = paste(verdict$motifs, collapse = " "))
	}
})

test_that("des r\u00e9ponses justes font monter la difficult\u00e9, une fois l'\u00e9chantillon suffisant", {
	withr::local_envvar(c(OPENAI_API_KEY = ""))

	reussies <- function(items) {
		lapply(items, function(it) {
			list(
				item_id = it$item_id,
				lot = it$lot,
				indice_seance = it$indice_seance,
				is_correct = TRUE,
				full_theme_id = it$full_theme_id,
				type = it$type,
				duree_ms = 20000
			)
		})
	}
	seeds <- function(items) vapply(items, function(i) i$seed_id, character(1))

	parametres <- parametres_seance(nb_questions = 10)
	premier <- generer_lot(parametres, lot = 1L)
	depart <- premier$plan$etat_adaptatif$difficulte

	# Trois réponses : trop peu pour bouger le niveau.
	second <- generer_lot(
		parametres,
		plan = premier$plan,
		lot = 2L,
		exclure_seed_ids = seeds(premier$items),
		historique = list(reponses = reussies(premier$items))
	)
	expect_identical(second$plan$etat_adaptatif$difficulte, depart)

	troisieme <- generer_lot(
		parametres,
		plan = second$plan,
		lot = 3L,
		exclure_seed_ids = c(seeds(premier$items), seeds(second$items)),
		historique = list(reponses = reussies(c(premier$items, second$items)))
	)
	expect_identical(troisieme$plan$etat_adaptatif$difficulte, depart + 1L)
	expect_identical(troisieme$plan$directive$lot, 3L)
	expect_true(max(vapply(troisieme$consignes, function(c) c$difficulte, integer(1))) >= depart + 1L)
})

test_that("la cha\u00eene d\u00e9grad\u00e9e ne ressert pas une graine d\u00e9j\u00e0 vue", {
	withr::local_envvar(c(OPENAI_API_KEY = ""))

	parametres <- parametres_seance(nb_questions = 10)
	premier <- generer_lot(parametres, lot = 1L)
	servies <- vapply(premier$items, function(i) i$seed_id, character(1))

	second <- generer_lot(
		parametres,
		plan = premier$plan,
		lot = 2L,
		exclure_seed_ids = servies
	)

	expect_length(intersect(vapply(second$items, function(i) i$seed_id, character(1)), servies), 0L)
})

test_that("les items portent les m\u00e9tadonn\u00e9es de leur graine", {
	withr::local_envvar(c(OPENAI_API_KEY = ""))

	ctx <- generer_lot(parametres_seance(nb_questions = 10), lot = 1L)

	for (item in ctx$items) {
		expect_true(nzchar(item$item_id))
		expect_true(nzchar(item$full_theme_id))
		expect_true(nzchar(item$theme))
		expect_true(nzchar(item$sub_theme))
		expect_true(nzchar(item$objectif))
	}

	tableau <- items_tibble(ctx$items)

	expect_identical(nrow(tableau), length(ctx$items))
	expect_false(anyNA(tableau$enonce))
})

test_that("un lot vide se met en tableau sans \u00e9chouer", {
	expect_identical(nrow(items_tibble(list())), 0L)
	expect_identical(nrow(llm_synthese_journal(list())), 1L)
	expect_identical(llm_synthese_journal(list())$n_appels, 0L)
})
