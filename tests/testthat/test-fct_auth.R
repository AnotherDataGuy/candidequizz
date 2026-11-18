# Comptes et isolement des séances. Le backend .rds des autres tests n'est pas touché.

seance_minimale <- function(session_id = "seance-test") {
	list(
		session_id = session_id,
		libelle = "S\u00e9ance test",
		cree_le = Sys.time(),
		termine_le = NA,
		statut = "en_cours",
		parametres = list(nb_questions = 10L, generation = "ia_off"),
		plan = list(),
		items = list(),
		reponses = list(),
		lots_generes = 0L,
		journal_llm = list(),
		trace = list(),
		generation_etat = "ia_off",
		version_schema = store_version_schema(),
		version_banque = "test"
	)
}

local_sqlite <- function(env = parent.frame()) {
	repertoire <- withr::local_tempdir(.local_envir = env)
	withr::local_envvar(
		c(
			CANDIDEQUIZZ_STORE_DIR = repertoire,
			CANDIDEQUIZZ_STORE_BACKEND = "sqlite"
		),
		.local_envir = env
	)
	repertoire
}

test_that("deux comptes ne partagent pas leurs séances", {
	local_sqlite()

	alice <- auth_creer("alice_test", "motdepasse", admin = TRUE)
	bob <- auth_creer("bob_test", "motdepasse2", admin = TRUE)
	seance <- seance_minimale()
	seance$user_id <- alice$user_id
	store_sauver(seance, alice$user_id)

	expect_identical(store_lister(alice$user_id)$session_id, seance$session_id)
	expect_equal(nrow(store_lister(bob$user_id)), 0L)
	expect_error(
		store_charger(seance$session_id, bob$user_id),
		class = "candidequizz_store_seance_absente"
	)

	relue <- store_charger(seance$session_id, alice$user_id)
	expect_identical(relue$libelle, seance$libelle)
	expect_identical(relue$user_id, alice$user_id)
})

test_that("un jeton retrouve le compte et disparaît à la déconnexion", {
	local_sqlite()
	auth_creer("camille_test", "motdepasse", admin = TRUE)

	expect_error(
		auth_connecter("camille_test", "mauvaisxx"),
		class = "candidequizz_auth_invalide"
	)

	compte <- auth_connecter("camille_test", "motdepasse")
	resolu <- auth_token_resoudre(compte$token)
	expect_identical(resolu$username, "camille_test")

	auth_token_revoquer(compte$token)
	expect_null(auth_token_resoudre(compte$token))
})

test_that("sans IA, la génération est dégradée", {
	p <- parametres_seance(generation = "ia_off")
	expect_identical(p$generation, "ia_off")
	expect_false(p$verifier_faits)
	expect_true(pipeline_ctx(parametres = p)$degrade)
})

test_that("une coupure reste dégradée sauf relance explicite", {
	p <- parametres_seance(generation = "ia_on")
	p$generation <- "ia_on"
	p$generation_etat <- "ia_coupure"
	expect_true(generation_degradee(p))
	p$generation_retenter <- TRUE
	expect_identical(generation_degradee(p), !llm_cle_disponible())
})

test_that("l'administrateur supprime un compte et toutes ses données", {
	local_sqlite()
	admin <- auth_creer("admin_test", "motdepasse", admin = TRUE)
	ami <- auth_creer("renard", "motdepasse", par_admin = TRUE)
	autre <- auth_creer("colibri", "motdepasse", par_admin = TRUE)

	for (id in c("s-ami-1", "s-ami-2")) {
		seance <- seance_minimale(id)
		seance$user_id <- ami$user_id
		store_sauver(seance, ami$user_id)
	}
	garde <- seance_minimale("s-autre")
	garde$user_id <- autre$user_id
	store_sauver(garde, autre$user_id)
	jeton <- auth_connecter("renard", "motdepasse")$token

	expect_identical(auth_supprimer("Renard "), 2L)
	expect_null(auth_trouver("renard"))
	expect_null(auth_token_resoudre(jeton))
	expect_equal(nrow(store_lister(ami$user_id)), 0L)
	expect_identical(store_lister(autre$user_id)$session_id, "s-autre")

	expect_error(auth_supprimer("renard"), class = "candidequizz_auth_invalide")
	expect_error(auth_supprimer("admin_test"), class = "candidequizz_auth_invalide")
	expect_false(is.null(auth_trouver("admin_test")))
})
