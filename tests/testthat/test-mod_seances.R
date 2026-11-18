# Logique des modules de séance, hors interface.
#
# Les fonctions testées ici sont celles qui décident quelque chose : quels
# modes sont proposés, comment la progression est calculée, comment la boucle
# adaptative avance. Le rendu des tags n'est pas testé, il est vérifié à
# l'écran.

test_that("un mode n'est jouable que si tous ses types ont une interface", {
	jouables <- seance_config_modes_jouables()

	expect_true("qcm_classique" %in% jouables)
	expect_true("autres_types" %in% jouables)
	expect_false("examen_blanc" %in% cq_modes())
	expect_setequal(unname(seance_config_profils()), cq_profils())
})

test_that("aucun module d'interface ne branche sur le type de question", {
	# T-21 : ajouter un type ne doit pas obliger à toucher aux modules.
	fichiers <- list.files(
		system.file(package = "candidequizz", "R"),
		pattern = "^mod_.*\\.R$",
		full.names = TRUE
	)

	# `system.file(..., "R")` est vide pour un package chargé par pkgload :
	# on relit alors les sources du projet.
	if (!length(fichiers)) {
		fichiers <- list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)
	}

	# Sous `R CMD check`, le paquet installé n'a plus ses sources `R/*.R` :
	# rien à relire, le test est sauté (il tourne avec `devtools::test()`).
	skip_if(length(fichiers) == 0L, "Sources R absentes (R CMD check)")

	fautifs <- character(0)

	for (fichier in fichiers) {
		lignes <- readLines(fichier, warn = FALSE)
		if (any(grepl("switch\\s*\\(\\s*[^)]*\\$type|if\\s*\\([^)]*\\$type\\s*==", lignes))) {
			fautifs <- c(fautifs, basename(fichier))
		}
	}

	expect_identical(fautifs, character(0))
})

test_that("les sous-thèmes proposés sont groupés par thème", {
	choix <- seance_config_choix_themes()

	expect_true(length(choix) > 1L)
	expect_true(all(vapply(choix, function(g) length(g) > 0L, logical(1))))
	expect_true(all(grepl("-", unlist(choix, use.names = FALSE))))
})

test_that("la progression part de zéro et non d'une question déjà faite", {
	entete <- as.character(seance_run_progression(position = 1L, total = 10L))

	expect_match(entete, "Question 1 sur 10", fixed = TRUE)
	expect_match(entete, 'aria-valuenow="0"', fixed = TRUE)
	expect_match(entete, "cq-talon--courant", fixed = TRUE)

	fin <- as.character(seance_run_progression(position = 10L, total = 10L))

	expect_match(fin, 'aria-valuenow="90"', fixed = TRUE)
})

test_that("les talons suivent les résultats, sans étiquette de thème", {
	entete <- as.character(seance_run_progression(
		position = 3L,
		total = 5L,
		resultats = c("1" = "juste", "2" = "faux", "3" = "passee")
	))

	expect_match(entete, "cq-talon--juste", fixed = TRUE)
	expect_match(entete, "cq-talon--faux", fixed = TRUE)
	expect_match(entete, "cq-talon--passee is-nouveau", fixed = TRUE)
	expect_match(entete, "N\u00b0 ", fixed = TRUE)
})

test_that("le tampon dit Correct, Partiel ou À revoir", {
	expect_match(as.character(seance_run_tampon(list(is_correct = TRUE, score = 1))), "Correct", fixed = TRUE)
	expect_match(as.character(seance_run_tampon(list(is_correct = FALSE, score = 0.5))), "Partiel", fixed = TRUE)
	expect_match(as.character(seance_run_tampon(list(is_correct = FALSE, score = 0))), "\u00c0 revoir", fixed = TRUE)
})

test_that("une s\u00e9ance achev\u00e9e doit ouvrir le bilan", {
	seance <- seance_test()
	expect_false(seance_run_vers_bilan(seance, 1L))

	seance <- seance_clore(seance, "terminee")
	expect_true(seance_run_vers_bilan(seance, 1L))
	expect_false(seance_run_vers_bilan(NULL, 1L))
})

test_that("l'état adaptatif avance avec les réponses du lot précédent", {
	parametres <- parametres_seance(
		nb_questions = 20L,
		mode = "qcm_classique",
		n_options = 4L
	)

	ctx <- pipeline_ctx(parametres = parametres, lot = 2L)
	ctx <- run_agent("planificateur", ctx)

	depart <- ctx$plan$etat_adaptatif$difficulte

	# Un premier lot entièrement réussi doit faire monter d'un cran.
	ctx$historique <- list(reponses = lapply(seq_len(5L), function(i) {
		list(lot = 1L, is_correct = TRUE)
	}))

	etat <- selecteur_etat_adaptatif(ctx)

	expect_identical(etat$difficulte, depart + 1L)
	expect_identical(etat$dernier_lot_integre, 1L)
})

test_that("les r\u00e9ponses arriv\u00e9es apr\u00e8s le pr\u00e9chargement comptent au lot suivant", {
	parametres <- parametres_seance(
		nb_questions = 20L,
		mode = "qcm_classique",
		n_options = 4L
	)
	ctx <- pipeline_ctx(parametres = parametres, lot = 2L)
	ctx <- run_agent("planificateur", ctx)
	depart <- ctx$plan$etat_adaptatif$difficulte
	reponse <- function(i, lot, juste) {
		list(item_id = paste0("i", i), indice_seance = i, lot = lot, is_correct = juste)
	}

	# Le lot 2 se prépare alors que deux réponses seulement du lot 1 existent.
	ctx$historique <- list(reponses_session = list(reponse(1L, 1L, TRUE), reponse(2L, 1L, TRUE)))
	etat <- regleur_etat_adaptatif(ctx)
	expect_identical(etat$difficulte, depart)
	expect_identical(etat$reponses_integrees, c("i1", "i2"))

	# Au lot 3, les trois dernières du lot 1 s'ajoutent à celles du lot 2.
	ctx$plan$etat_adaptatif <- etat
	ctx$lot <- 3L
	ctx$historique <- list(reponses_session = c(
		lapply(1:5, reponse, lot = 1L, juste = TRUE),
		lapply(6:9, reponse, lot = 2L, juste = TRUE)
	))
	etat <- regleur_etat_adaptatif(ctx)
	expect_identical(etat$difficulte, depart + 1L)
	expect_length(etat$reponses_integrees, 9L)

	# Relancer la même génération n'applique pas deux fois les réponses.
	ctx$plan$etat_adaptatif <- etat
	expect_identical(regleur_etat_adaptatif(ctx), etat)
})

test_that("l'aiguillage r\u00e9ordonne seulement les questions pas encore pos\u00e9es", {
	seance <- seance_test(nb_questions = 10L)
	seance$plan$tailles_lots <- c(5L, 5L)
	seance$plan$n_lots <- 2L
	seance$plan$indices_lots <- list(1:5, 6:10)
	seance$items <- lapply(1:10, function(k) {
		item_test(
			item_id = paste0("i", k),
			indice_seance = k,
			lot = if (k <= 5L) 1L else 2L,
			difficulte = if (k %in% c(5L, 6L)) 2L else 3L
		)
	})
	seance$reponses <- list(
		list(item_id = "i1", is_correct = FALSE, sub_theme = "X"),
		list(item_id = "i2", is_correct = FALSE, sub_theme = "X")
	)

	apres <- seance_aiguiller(seance, 2L)

	# La première question facile passe devant ; les questions posées ne bougent pas.
	expect_identical(seance_item(apres, 3L)$item_id, "i5")
	expect_identical(seance_item(apres, 4L)$item_id, "i3")
	expect_identical(seance_item(apres, 1L)$item_id, "i1")
	expect_identical(seance_item(apres, 6L)$item_id, "i6")
	expect_identical(apres$aiguillages[[1L]]$motif, "facile")

	# L'examen blanc garde son ordre.
	seance$parametres$profil <- "examen_blanc"
	expect_identical(seance_aiguiller(seance, 2L), seance)
})

test_that("l'examen blanc ignore les réponses des lots précédents", {
	parametres <- parametres_seance(profil = "examen_blanc")
	ctx <- pipeline_ctx(parametres = parametres, lot = 2L)
	ctx <- run_agent("planificateur", ctx)
	depart <- ctx$plan$etat_adaptatif
	ctx$historique <- list(reponses = lapply(seq_len(5L), function(i) {
		list(
			lot = 1L,
			is_correct = FALSE,
			full_theme_id = "T1-S1",
			diagnostic_tags = "confusion_repetee"
		)
	}))

	ctx <- agent_regleur_adaptatif(ctx)

	expect_false(ctx$plan$directive$adaptatif)
	expect_null(ctx$plan$directive$poids_themes)
	expect_identical(ctx$plan$etat_adaptatif, depart)
	expect_identical(
		ctx$plan$directive$types,
		ctx$plan$types[ctx$plan$indices_lots[[2L]]]
	)
})

test_that("l'historique ancien cible les thèmes sans modifier la difficulté courante", {
	parametres <- parametres_seance(
		nb_questions = 20L,
		mode = "qcm_classique",
		n_options = 4L
	)
	ctx <- pipeline_ctx(parametres = parametres, lot = 2L)
	ctx <- run_agent("planificateur", ctx)
	depart <- ctx$plan$etat_adaptatif
	ctx$historique <- list(
		reponses = lapply(seq_len(5L), function(i) {
			list(lot = 1L, is_correct = FALSE, full_theme_id = "T1-S1")
		}),
		reponses_session = list()
	)

	ctx <- agent_regleur_adaptatif(ctx)

	expect_identical(ctx$plan$etat_adaptatif, depart)
	expect_identical(unname(ctx$plan$directive$poids_themes[["T1-S1"]]), 2)
})

test_that("le diagnostic d'une erreur reste formulé comme une hypothèse", {
	verdict <- list(
		is_correct = FALSE,
		detail = list(diagnostic_tags = "confusion_resistance")
	)
	texte <- as.character(seance_run_diagnostic_ui(verdict))

	expect_match(texte, "Piège possible")
	expect_match(texte, "confirmée seulement si elle se répète")
	expect_null(seance_run_diagnostic_ui(list(is_correct = TRUE)))
})

test_that("un lot déjà intégré n'est pas compté deux fois", {
	# Sans ce garde-fou, une génération relancée après un échec réseau
	# appliquerait deux fois les mêmes réponses.
	parametres <- parametres_seance(
		nb_questions = 20L,
		mode = "qcm_classique",
		n_options = 4L
	)

	ctx <- pipeline_ctx(parametres = parametres, lot = 2L)
	ctx <- run_agent("planificateur", ctx)
	ctx$historique <- list(reponses = lapply(seq_len(5L), function(i) {
		list(lot = 1L, is_correct = FALSE)
	}))

	premier <- selecteur_etat_adaptatif(ctx)

	ctx$plan$etat_adaptatif <- premier
	second <- selecteur_etat_adaptatif(ctx)

	expect_identical(second$difficulte, premier$difficulte)
})

test_that("la tranche suivante part d\u00e8s qu'il ne reste que trois questions pr\u00eates", {
	plan <- list(
		tailles_lots = c(3L, 3L, 3L, 3L),
		n_lots = 4L,
		indices_lots = list(1:3, 4:6, 7:9, 10:12)
	)
	seance <- list(plan = plan, lots_generes = 1L)

	expect_identical(seance_dernier_rang_pret(seance), 3L)
	expect_true(seance_doit_precharger(seance, 1L))

	# Deux tranches prêtes : six questions devant soi à la première, on attend.
	seance$lots_generes <- 2L
	expect_false(seance_doit_precharger(seance, 1L))
	expect_false(seance_doit_precharger(seance, 3L))
	expect_true(seance_doit_precharger(seance, 4L))

	# Question non encore écrite : on prépare tout de suite.
	expect_true(seance_doit_precharger(seance, 8L))

	# Toutes les tranches prêtes : plus rien à préparer.
	seance$lots_generes <- 4L
	expect_false(seance_doit_precharger(seance, 11L))
})

test_that("un lot pr\u00e9charg\u00e9 n'est int\u00e9gr\u00e9 que s'il est le suivant", {
	parametres <- parametres_seance(nb_questions = 10L)
	seance <- seance_nouvelle(parametres, plan = list(n_lots = 2L))
	seance$lots_generes <- 1L

	ignore <- seance_run_integrer_lot(seance, list(lot = 1L, items = list("x")))
	expect_identical(ignore$lots_generes, 1L)

	accepte <- seance_run_integrer_lot(
		seance,
		list(lot = 2L, items = list("y"), journal_llm = list(), trace = list(), plan = seance$plan)
	)
	expect_identical(accepte$lots_generes, 2L)
	expect_identical(accepte$items[[1L]], "y")
})

test_that("l'\u00e9cran de transition distingue pr\u00eat, attente et erreur", {
	pret <- as.character(seance_run_transition(2L, 3L, pret = TRUE))
	attente <- as.character(seance_run_transition(2L, 3L, pret = FALSE))
	echec <- as.character(seance_run_transition(2L, 3L, pret = FALSE, erreur = "panne"))

	expect_match(pret, "pr\u00eates")
	expect_match(attente, "Pr\u00e9paration des questions suivantes")
	expect_match(attente, "cq-attente-jauge", fixed = TRUE)
	expect_match(echec, "panne", fixed = TRUE)
})

test_that("le premier lot ne consulte aucun historique", {
	parametres <- parametres_seance(
		nb_questions = 10L,
		mode = "qcm_classique",
		n_options = 4L
	)

	ctx <- pipeline_ctx(parametres = parametres, lot = 1L)
	ctx <- run_agent("planificateur", ctx)

	expect_identical(
		selecteur_etat_adaptatif(ctx)$difficulte,
		ctx$plan$etat_adaptatif$difficulte
	)
})
