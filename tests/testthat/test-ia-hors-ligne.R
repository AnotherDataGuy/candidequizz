# Chemin de l'IA sans jamais appeler OpenAI : faux client {ellmer} pour la
# couche d'appel, vrai processus d'arrière-plan {future} pour la génération
# des lots (sans IA).

# Faux objet Chat : renvoie ou lève ce qu'on lui donne, compte des tokens.
faux_chat <- function(reponses) {
	etat <- new.env()
	etat$n <- 0L
	fabrique <- function(...) {
		list(
			chat_structured = function(invite, type, convert) {
				etat$n <- etat$n + 1L
				r <- reponses[[min(etat$n, length(reponses))]]
				if (inherits(r, "condition")) stop(r)
				r
			},
			get_tokens = function() data.frame(input = 100L, output = 20L)
		)
	}
	list(fabrique = fabrique, etat = etat)
}

erreur_http <- function(code) {
	structure(
		class = c(paste0("httr2_http_", code), "httr2_http", "error", "condition"),
		list(message = paste("HTTP", code), call = NULL)
	)
}

appel_test <- function(max_tentatives = 3L) {
	llm_appel_structure(
		agent = "test", role = "redacteur", systeme = "s", invite = "i",
		schema = ellmer::type_object(x = ellmer::type_string()),
		max_tentatives = max_tentatives
	)
}

test_that("appel réussi : données, journal, tokens", {
	withr::local_envvar(OPENAI_API_KEY = "sk-test")
	f <- faux_chat(list(list(x = "bonjour")))
	local_mocked_bindings(chat_openai = f$fabrique, .package = "ellmer")

	r <- appel_test()
	expect_true(r$ok)
	expect_identical(r$donnees$x, "bonjour")
	expect_identical(r$journal$tentatives, 1L)
	expect_identical(r$journal$tokens_entree, 100L)
	expect_identical(r$journal$statut, "ok")
})

test_that("erreur passagère : nouvelle tentative, tokens des deux appels comptés", {
	withr::local_envvar(OPENAI_API_KEY = "sk-test")
	f <- faux_chat(list(simpleError("délai dépassé"), list(x = "ok")))
	local_mocked_bindings(chat_openai = f$fabrique, .package = "ellmer")

	r <- appel_test()
	expect_true(r$ok)
	expect_identical(r$journal$tentatives, 2L)
	expect_identical(r$journal$tokens_entree, 200L)
})

test_that("clé refusée (401) : pas de nouvelle tentative, échec rendu à l'agent", {
	withr::local_envvar(OPENAI_API_KEY = "sk-test")
	f <- faux_chat(list(erreur_http(401)))
	local_mocked_bindings(chat_openai = f$fabrique, .package = "ellmer")

	r <- appel_test()
	expect_false(r$ok)
	expect_null(r$donnees)
	expect_identical(f$etat$n, 1L)
	expect_match(r$message, "401")
	expect_identical(r$journal$statut, "echec")
})

test_that("échecs répétés : s'arrête au nombre de tentatives prévu", {
	withr::local_envvar(OPENAI_API_KEY = "sk-test")
	f <- faux_chat(list(simpleError("panne")))
	local_mocked_bindings(chat_openai = f$fabrique, .package = "ellmer")

	r <- appel_test(max_tentatives = 2L)
	expect_false(r$ok)
	expect_identical(f$etat$n, 2L)
	expect_identical(r$journal$tentatives, 2L)
})

test_that("génération de lot : synchrone quand l'arrière-plan est éteint", {
	local_store()
	future::plan(future::sequential)
	p <- parametres_seance(nb_questions = 10L, generation = "ia_off", verifier_faits = FALSE)
	ctx <- async_generer_lot(list(parametres = p, lot = 1L))
	expect_true(length(ctx$items) >= 1L)
	expect_false(is.null(ctx$plan))
})

# Attendre une promesse en dehors de Shiny.
attendre <- function(promesse, delai_s = 90) {
	valeur <- NULL
	erreur <- NULL
	fini <- FALSE
	promises::then(
		promesse,
		onFulfilled = function(v) { valeur <<- v; fini <<- TRUE },
		onRejected = function(e) { erreur <<- e; fini <<- TRUE }
	)
	limite <- Sys.time() + delai_s
	while (!fini && Sys.time() < limite) {
		later::run_now(0.1)
	}
	if (!fini) stop("Délai dépassé en attendant le travailleur.")
	if (!is.null(erreur)) stop(erreur)
	valeur
}

test_that("vrai processus d'arrière-plan : lot généré, clé transmise", {
	skip_on_cran()
	local_store()
	withr::local_envvar(OPENAI_API_KEY = "sk-cle-de-test-transmise")
	withr::defer(future::plan(future::sequential))
	future::plan(future::multisession, workers = 2L)

	p <- parametres_seance(nb_questions = 10L, generation = "ia_off", verifier_faits = FALSE)
	ctx <- attendre(async_appeler("generer_lot", list(parametres = p, lot = 1L)))
	expect_true(length(ctx$items) >= 1L)

	# La clé collée dans l'application arrive bien jusqu'au travailleur.
	expect_true(attendre(async_appeler("llm_cle_disponible", list())))
})

test_that("travailleur en panne : la séance continue en synchrone", {
	local_store()
	withr::defer(future::plan(future::sequential))
	future::plan(future::multisession, workers = 2L)
	local_mocked_bindings(
		async_active = function() TRUE,
		async_appeler = function(...) {
			promises::promise_reject(structure(
				class = c("FutureError", "error", "condition"),
				list(message = "travailleur disparu", call = NULL)
			))
		}
	)

	p <- parametres_seance(nb_questions = 10L, generation = "ia_off", verifier_faits = FALSE)
	ctx <- attendre(async_generer_lot(list(parametres = p, lot = 1L)))
	expect_true(length(ctx$items) >= 1L)
})

test_that("version en ligne : pas de processus d'arrière-plan (mémoire de Connect Cloud)", {
	withr::local_envvar(GOLEM_CONFIG_ACTIVE = "production")
	expect_false(isTRUE(cq_config("seance", "prefetch_async")))
	expect_false(async_active())
	# Les autres réglages de séance restent ceux par défaut.
	expect_identical(as.integer(cq_config("seance", "examen_nb_questions")), 40L)
})
