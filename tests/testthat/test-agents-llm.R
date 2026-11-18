# Agents LLM, client simulé.
#
# Les agents A-3, A-5 et A-6 ont une logique propre qui ne doit rien au réseau :
# appariement des réponses, gestion des manques, boucle de réparation, repli.
# On simule donc le client pour tester cette logique, la chaîne réelle étant
# vérifiée séparément en console.

#' Réponse simulée du client LLM
#'
#' @param donnees Charge utile renvoyée.
#' @param ok Succès de l'appel.
#' @param modele Modèle annoncé.
#'
#' @return La même structure que [llm_appel_structure()].
#' @noRd
appel_simule <- function(donnees, ok = TRUE, modele = "gpt-4.1-mini") {
	list(
		donnees = donnees,
		ok = ok,
		message = if (ok) NA_character_ else "panne simul\u00e9e",
		journal = llm_ligne_journal(
			agent = "test",
			role = "redacteur",
			modele = modele,
			tokens_entree = 100,
			tokens_sortie = 50,
			duree_s = 0.1,
			statut = if (ok) "ok" else "echec"
		)
	)
}

#' Un item brut tel que le modèle le renverrait
#'
#' @param numero Numéro de la consigne.
#' @param n_options Nombre d'options.
#'
#' @return Une liste.
#' @noRd
brut_qcm <- function(numero, n_options = 3L) {
	options <- lapply(seq_len(n_options), function(i) {
		list(
			texte = paste("Option", i),
			is_correct = i == 1L,
			diagnostic_tag = if (i == 1L) "" else paste0("confusion_", i)
		)
	})

	list(
		numero = numero,
		enonce = glue::glue("Question simul\u00e9e num\u00e9ro {numero} sur la R\u00e9publique ?"),
		options = options,
		explication = "Explication simul\u00e9e suffisamment longue pour \u00eatre accept\u00e9e."
	)
}

test_that("un modele qui insiste sur trois choix finit en repli a quatre", {
	ctx <- pipeline_ctx(parametres = parametres_seance(verifier_faits = FALSE))
	ctx$degrade <- FALSE
	ctx$consignes <- list(consigne_test(n_options = 4L))
	ctx$items <- list(item_test(numero = 1L))
	ctx <- agent_conformite(ctx)
	expect_length(ctx$rejets, 1L)
	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) appel_simule(list(items = list(brut_qcm(1L, n_options = 3L))))
	)
	ctx <- agent_reparateur(ctx)
	expect_length(ctx$items, 1L)
	expect_identical(ctx$items[[1L]]$source, "graine_repli")
	expect_length(ctx$items[[1L]]$payload$options, 4L)
	expect_true(qtype_qcm_single_valider(ctx$items[[1L]], ctx$consignes[[1L]])$ok)
})

contexte_consignes <- function(n = 3L, type = "qcm_single", n_options = 3L) {
	# Séance avec IA demandée explicitement : ces tests simulent le modèle et ne
	# doivent dépendre ni d'une vraie clé ni du réglage `llm.par_defaut`.
	withr::local_envvar(c(OPENAI_API_KEY = "sk-test"))
	ctx <- pipeline_ctx(parametres = parametres_seance(nb_questions = 10, generation = "ia_on"))
	ctx$degrade <- FALSE
	ctx$consignes <- lapply(seq_len(n), function(i) {
		consigne_test(numero = i, indice_seance = i, type = type, n_options = n_options)
	})

	ctx
}

test_that("l'appariement suit le num\u00e9ro et non la position", {
	consignes <- lapply(1:3, function(i) consigne_test(numero = i))

	# Le modèle renvoie les items dans le désordre : un appariement positionnel
	# rattacherait chaque question à la mauvaise graine et fausserait les
	# statistiques par thème.
	bruts <- list(brut_qcm(3L), brut_qcm(1L), brut_qcm(2L))
	apparies <- redacteur_apparier(bruts, consignes)

	expect_length(apparies, 3L)
	expect_identical(vapply(apparies, function(b) b$numero, integer(1)), 1:3)
})

test_that("l'appariement signale un num\u00e9ro absent ou invent\u00e9", {
	consignes <- lapply(1:3, function(i) consigne_test(numero = i))
	apparies <- redacteur_apparier(list(brut_qcm(1L), brut_qcm(99L)), consignes)

	expect_false(is.null(apparies[[1L]]))
	expect_null(apparies[[2L]])
	expect_null(apparies[[3L]])
})

test_that("le r\u00e9dacteur conserve l'ordre des consignes malgr\u00e9 le regroupement par type", {
	ctx <- contexte_consignes(n = 4L)
	ctx$consignes[[2L]]$type <- "vrai_faux"
	ctx$consignes[[2L]]$n_options <- NA_integer_
	ctx$consignes[[4L]]$type <- "vrai_faux"
	ctx$consignes[[4L]]$n_options <- NA_integer_

	testthat::local_mocked_bindings(
		llm_appel_structure = function(agent, role, systeme, invite, schema, ...) {
			if (grepl("Vrai ou faux", invite, fixed = TRUE)) {
				return(appel_simule(list(items = list(
					list(
						numero = 2L,
						enonce = "La R\u00e9publique fran\u00e7aise est indivisible.",
						reponse = TRUE,
						diagnostic_tag = "confusion_federalisme",
						explication = "Explication simul\u00e9e suffisamment longue pour passer."
					),
					list(
						numero = 4L,
						enonce = "La R\u00e9publique fran\u00e7aise compte deux langues officielles.",
						reponse = FALSE,
						diagnostic_tag = "confusion_langues",
						explication = "Explication simul\u00e9e suffisamment longue pour passer."
					)
				))))
			}

			appel_simule(list(items = list(brut_qcm(1L), brut_qcm(3L))))
		}
	)

	ctx <- run_agent("redacteur", ctx)

	expect_identical(vapply(ctx$items, function(i) i$numero, integer(1)), 1:4)
	expect_identical(
		vapply(ctx$items, function(i) i$type, character(1)),
		c("qcm_single", "vrai_faux", "qcm_single", "vrai_faux")
	)

	# Un appel par type, et non un appel par item.
	expect_identical(llm_synthese_journal(ctx$journal_llm)$n_appels, 2L)
})

test_that("un item manquant dans la r\u00e9ponse du mod\u00e8le bascule sur la graine", {
	ctx <- contexte_consignes(n = 3L)

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) {
			appel_simule(list(items = list(brut_qcm(1L), brut_qcm(2L))))
		}
	)

	ctx <- run_agent("redacteur", ctx)

	sources <- vapply(ctx$items, function(i) i$source, character(1))

	expect_identical(sources, c("llm", "llm", "graine_repli"))
	expect_match(ctx$items[[3L]]$motif_repli, "absent de la r\u00e9ponse")
})

test_that("une panne du mod\u00e8le n'interrompt pas le lot", {
	ctx <- contexte_consignes(n = 3L)

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) appel_simule(NULL, ok = FALSE)
	)

	ctx <- run_agent("redacteur", ctx)

	expect_length(ctx$items, 3L)
	expect_true(all(vapply(ctx$items, function(i) i$source == "graine_repli", logical(1))))
	expect_match(ctx$items[[1L]]$motif_repli, "\u00e9chec du mod\u00e8le")
})

test_that("le contr\u00f4leur de conformit\u00e9 s\u00e9pare les items conformes des rejets", {
	ctx <- contexte_consignes(n = 2L)
	ctx$items <- list(
		item_test("qcm_single", numero = 1L),
		item_test("qcm_single", numero = 2L, explication = "")
	)

	ctx <- run_agent("conformite", ctx)

	expect_length(ctx$items, 1L)
	expect_length(ctx$rejets, 1L)
	expect_identical(ctx$rejets[[1L]]$item$numero, 2L)
	expect_identical(ctx$rejets[[1L]]$origine, "conformite")
	expect_match(paste(ctx$rejets[[1L]]$motifs, collapse = " "), "Explication")
})

test_that("le v\u00e9rificateur factuel d\u00e9place les items jug\u00e9s inexacts vers les rejets", {
	ctx <- contexte_consignes(n = 2L)
	ctx$items <- list(item_test("qcm_single", numero = 1L), item_test("qcm_single", numero = 2L))
	ctx$rejets <- list()

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) {
			appel_simule(list(verdicts = list(
				list(numero = 1L, valide = TRUE, motif = ""),
				list(numero = 2L, valide = FALSE, motif = "La date annonc\u00e9e est inexacte.")
			)))
		}
	)

	ctx <- run_agent("verificateur_factuel", ctx)

	expect_length(ctx$items, 1L)
	expect_identical(ctx$items[[1L]]$numero, 1L)
	expect_length(ctx$rejets, 1L)
	expect_identical(ctx$rejets[[1L]]$origine, "verification_factuelle")
	expect_match(ctx$rejets[[1L]]$motifs, "date annonc\u00e9e")
})

test_that("le v\u00e9rificateur est ignor\u00e9 quand la s\u00e9ance ne le demande pas", {
	ctx <- contexte_consignes(n = 1L)
	ctx$parametres$verifier_faits <- FALSE
	ctx$items <- list(item_test("qcm_single", numero = 1L))
	ctx$rejets <- list()

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) stop("le v\u00e9rificateur ne doit pas \u00eatre appel\u00e9")
	)

	ctx <- run_agent("verificateur_factuel", ctx)

	expect_length(ctx$items, 1L)
	expect_identical(llm_synthese_journal(ctx$journal_llm)$n_appels, 0L)
})

test_that("le v\u00e9rificateur ne relit pas les items de repli", {
	# Ils reprennent mot pour mot une graine déjà relue : les soumettre
	# reviendrait à payer pour douter de la banque.
	ctx <- contexte_consignes(n = 1L)
	ctx$items <- list(item_test("qcm_single", numero = 1L, source = "graine_repli"))
	ctx$rejets <- list()

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) stop("aucun appel attendu")
	)

	ctx <- run_agent("verificateur_factuel", ctx)

	expect_length(ctx$items, 1L)
	expect_identical(llm_synthese_journal(ctx$journal_llm)$n_appels, 0L)
})

test_that("un v\u00e9rificateur en panne remplace les items non relus par leur graine", {
	ctx <- contexte_consignes(n = 1L)
	ctx$items <- list(item_test("qcm_single", numero = 1L))
	ctx$rejets <- list()

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) appel_simule(NULL, ok = FALSE)
	)

	ctx <- run_agent("verificateur_factuel", ctx)

	expect_length(ctx$items, 1L)
	expect_identical(ctx$items[[1L]]$source, "graine_repli")
	expect_length(ctx$rejets, 0L)

	trace <- pipeline_trace(ctx)
	expect_true(any(vapply(
		ctx$trace,
		function(t) isTRUE(t$verification_indisponible),
		logical(1)
	)))
})

test_that("le r\u00e9parateur corrige un item rejet\u00e9 et le remet dans le lot", {
	ctx <- contexte_consignes(n = 2L, n_options = 3L)
	ctx$items <- list(item_test("qcm_single", numero = 1L))
	ctx$rejets <- list(conformite_rejet(
		item = item_test("qcm_single", numero = 2L, explication = ""),
		consigne = ctx$consignes[[2L]],
		motifs = "Explication absente ou trop courte.",
		origine = "conformite"
	))

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) appel_simule(list(items = list(brut_qcm(2L))))
	)

	ctx <- run_agent("reparateur", ctx)

	expect_length(ctx$items, 2L)
	expect_identical(vapply(ctx$items, function(i) i$numero, integer(1)), 1:2)
	expect_true(all(vapply(ctx$items, function(i) i$source == "llm", logical(1))))
	expect_length(ctx$rejets, 0L)
})

test_that("le r\u00e9parateur abandonne sur la graine apr\u00e8s deux passes infructueuses", {
	ctx <- contexte_consignes(n = 1L, n_options = 3L)
	ctx$items <- list()
	ctx$rejets <- list(conformite_rejet(
		item = item_test("qcm_single", numero = 1L, explication = ""),
		consigne = ctx$consignes[[1L]],
		motifs = "Explication absente ou trop courte.",
		origine = "conformite"
	))

	appels <- 0L

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) {
			appels <<- appels + 1L

			# Le modèle s'obstine : il renvoie toujours un item non conforme.
			brut <- brut_qcm(1L)
			# L'explication peut être reprise de la banque : un énoncé vide,
			# lui, doit toujours être rejeté par la conformité.
			brut$enonce <- ""

			appel_simule(list(items = list(brut)))
		}
	)

	ctx <- run_agent("reparateur", ctx)

	expect_identical(appels, reparateur_passes_max())
	expect_length(ctx$items, 1L)
	expect_identical(ctx$items[[1L]]$source, "graine_repli")
	expect_match(ctx$items[[1L]]$motif_repli, "non r\u00e9parable")
	expect_length(ctx$rejets, 0L)
})

test_that("un item r\u00e9par\u00e9 apr\u00e8s un rejet factuel est relu, et remplac\u00e9 s'il reste faux", {
	ctx <- contexte_consignes(n = 1L, n_options = 3L)
	ctx$items <- list()
	ctx$rejets <- list(conformite_rejet(
		item = item_test("qcm_single", numero = 1L),
		consigne = ctx$consignes[[1L]],
		motifs = "Date inexacte.",
		origine = "verification_factuelle"
	))
	agents <- character(0)

	testthat::local_mocked_bindings(
		llm_appel_structure = function(agent, ...) {
			agents <<- c(agents, agent)

			if (identical(agent, "verificateur_factuel")) {
				return(appel_simule(list(verdicts = list(
					list(numero = 1L, valide = FALSE, motif = "Toujours inexact.")
				))))
			}

			appel_simule(list(items = list(brut_qcm(1L))))
		}
	)

	ctx <- run_agent("reparateur", ctx)

	expect_identical(agents, c("reparateur", "verificateur_factuel"))
	expect_length(ctx$items, 1L)
	expect_identical(ctx$items[[1L]]$source, "graine_repli")
})

test_that("une panne du mod\u00e8le coupe le circuit pour le reste du lot", {
	ctx <- contexte_consignes(n = 2L)
	ctx$consignes[[2L]]$type <- "vrai_faux"
	ctx$consignes[[2L]]$n_options <- NA_integer_
	appels <- 0L

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) {
			appels <<- appels + 1L
			appel_simule(NULL, ok = FALSE)
		}
	)

	ctx <- run_agent("redacteur", ctx)

	# Deux groupes de types, mais un seul appel : le second passe sur la graine.
	expect_identical(appels, 1L)
	expect_true(ctx$degrade)
	expect_length(ctx$items, 2L)
	expect_match(ctx$items[[2L]]$motif_repli, "indisponible")
})

test_that("apr\u00e8s le coupe-circuit, aucun item r\u00e9dig\u00e9 n'est servi sans relecture", {
	ctx <- contexte_consignes(n = 2L)
	ctx$items <- list(
		item_test("qcm_single", numero = 1L),
		item_test("qcm_single", numero = 2L, source = "graine_repli")
	)
	ctx$rejets <- list()
	ctx$degrade <- TRUE
	ctx$coupure_llm <- list(agent = "redacteur", message = "panne")

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) stop("aucun appel attendu")
	)

	ctx <- run_agent("verificateur_factuel", ctx)

	expect_true(all(vapply(ctx$items, function(i) i$source == "graine_repli", logical(1))))
	expect_match(ctx$items[[1L]]$motif_repli, "relecture factuelle impossible")
})

test_that("un item r\u00e9par\u00e9 pour un d\u00e9faut de forme est aussi relu sur le fond", {
	ctx <- contexte_consignes(n = 1L, n_options = 3L)
	ctx$items <- list()
	ctx$rejets <- list(conformite_rejet(
		item = item_test("qcm_single", numero = 1L, explication = ""),
		consigne = ctx$consignes[[1L]],
		motifs = "Explication absente ou trop courte.",
		origine = "conformite"
	))
	agents <- character(0)

	testthat::local_mocked_bindings(
		llm_appel_structure = function(agent, ...) {
			agents <<- c(agents, agent)

			if (identical(agent, "verificateur_factuel")) {
				return(appel_simule(list(verdicts = list(list(numero = 1L, valide = TRUE, motif = "")))))
			}

			appel_simule(list(items = list(brut_qcm(1L))))
		}
	)

	ctx <- run_agent("reparateur", ctx)

	expect_identical(agents, c("reparateur", "verificateur_factuel"))
	expect_identical(ctx$items[[1L]]$source, "llm")
})

test_that("une requ\u00eate refus\u00e9e n'est pas reprise", {
	expect_false(llm_erreur_reprenable(structure(
		class = c("httr2_http_401", "httr2_http", "httr2_error", "error", "condition"),
		list(message = "401", call = NULL)
	)))
	expect_true(llm_erreur_reprenable(simpleError("r\u00e9ponse illisible")))
})

test_that("le r\u00e9parateur n'appelle rien s'il n'y a aucun rejet", {
	ctx <- contexte_consignes(n = 1L)
	ctx$items <- list(item_test("qcm_single", numero = 1L))
	ctx$rejets <- list()

	testthat::local_mocked_bindings(
		llm_appel_structure = function(...) stop("aucun appel attendu")
	)

	ctx <- run_agent("reparateur", ctx)

	expect_length(ctx$items, 1L)
	expect_identical(llm_synthese_journal(ctx$journal_llm)$n_appels, 0L)
})

test_that("la cha\u00eene compl\u00e8te livre le compte exact d'items avec un mod\u00e8le simul\u00e9", {
	# Invariant central : quoi qu'il arrive au modèle, le lot livre autant
	# d'items que de consignes. Sans cela une séance de 20 questions pourrait
	# s'arrêter à 17.
	parametres <- parametres_seance(nb_questions = 10)

	testthat::local_mocked_bindings(
		llm_appel_structure = function(agent, role, systeme, invite, schema, ...) {
			if (identical(agent, "verificateur_factuel")) {
				# Le vérificateur rejette le premier item du lot.
				return(appel_simule(list(verdicts = list(
					list(numero = 1L, valide = FALSE, motif = "Fait inexact.")
				))))
			}

			numeros <- as.integer(unlist(regmatches(
				invite,
				gregexpr("(?<=num\u00e9ro )\\d+", invite, perl = TRUE)
			)))

			appel_simule(list(items = lapply(unique(numeros), brut_qcm, n_options = 4L)))
		}
	)

	ctx <- generer_lot(parametres, lot = 1L)

	expect_length(ctx$items, ctx$plan$tailles_lots[[1L]])
	expect_setequal(
		vapply(ctx$items, function(i) i$numero, integer(1)),
		seq_len(ctx$plan$tailles_lots[[1L]])
	)
	expect_length(ctx$rejets, 0L)
})
