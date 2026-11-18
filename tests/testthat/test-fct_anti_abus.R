# Garde-fous contre l'usage abusif (R/fct_anti_abus.R), sur une vraie base
# SQLite temporaire, en mode « comptes ».

local_anti_abus <- function(env = parent.frame()) {
	repertoire <- withr::local_tempdir(.local_envir = env)
	withr::local_envvar(
		c(
			CANDIDEQUIZZ_STORE_DIR = repertoire,
			CANDIDEQUIZZ_STORE_BACKEND = "sqlite",
			CANDIDEQUIZZ_AUTH_MODE = "comptes"
		),
		.local_envir = env
	)
	repertoire
}

seance_datee <- function(user_id, il_y_a_s = 0, generation = "ia_off", cout = 0) {
	list(
		session_id = paste0("s-", sample.int(1e9, 1)),
		libelle = "S\u00e9ance test",
		cree_le = Sys.time() - il_y_a_s,
		termine_le = NA,
		statut = "en_cours",
		parametres = list(nb_questions = 10L, generation = generation),
		plan = list(), items = list(), reponses = list(), lots_generes = 0L,
		journal_llm = if (cout > 0) list(list(cout_usd = cout)) else list(),
		trace = list(), generation_etat = generation,
		version_schema = store_version_schema(), version_banque = "test",
		user_id = user_id
	)
}

test_that("le rythme des séances est limité par compte, pas pour l'administrateur", {
	local_anti_abus()
	u <- auth_creer("rythme_test", "motdepasse", admin = TRUE)

	expect_true(anti_abus_seance_autorisee(u$user_id)$ok)

	# Une séance il y a 3 s : délai minimal non respecté.
	store_sauver(seance_datee(u$user_id, il_y_a_s = 3), u$user_id)
	refus <- anti_abus_seance_autorisee(u$user_id)
	expect_false(refus$ok)
	expect_match(refus$message, "Patientez")
	expect_true(anti_abus_seance_autorisee(u$user_id, admin = TRUE)$ok)

	# 20 séances dans l'heure (assez anciennes pour le délai) : plafond atteint.
	for (i in 1:19) store_sauver(seance_datee(u$user_id, il_y_a_s = 60 + i), u$user_id)
	refus <- anti_abus_seance_autorisee(u$user_id)
	expect_false(refus$ok)
	expect_match(refus$message, "dans l'heure")

	# Un autre compte n'est pas concerné.
	v <- auth_creer("autre_test", "motdepasse", admin = TRUE)
	expect_true(anti_abus_seance_autorisee(v$user_id)$ok)
})

test_that("l'IA est plafonnée par compte et par budget global du jour", {
	local_anti_abus()
	u <- auth_creer("ia_test", "motdepasse", admin = TRUE)
	v <- auth_creer("ia_autre", "motdepasse", admin = TRUE)

	expect_true(anti_abus_ia_autorisee(u$user_id)$ok)

	# 15 séances avec IA aujourd'hui pour u : quota atteint, pas pour v.
	for (i in 1:15) store_sauver(seance_datee(u$user_id, il_y_a_s = i, generation = "ia_on"), u$user_id)
	expect_false(anti_abus_ia_autorisee(u$user_id)$ok)
	expect_true(anti_abus_ia_autorisee(v$user_id)$ok)
	expect_true(anti_abus_ia_autorisee(u$user_id, admin = TRUE)$ok)

	# Budget global (2 $) atteint par v : l'IA s'arrête pour tous, admin compris.
	store_sauver(seance_datee(v$user_id, generation = "ia_on", cout = 2.5), v$user_id)
	refus <- anti_abus_ia_autorisee(v$user_id, admin = TRUE)
	expect_false(refus$ok)
	expect_match(refus$message, "budget")
})

test_that("une réponse trop rapide ne fait pas avancer le déblocage de l'IA", {
	local_anti_abus()
	expect_true(anti_abus_reponse_comptee(list(duree_ms = 5000)))
	expect_false(anti_abus_reponse_comptee(list(duree_ms = 800)))
	expect_true(anti_abus_reponse_comptee(list()))

	r <- function(seed, ms) list(seed_id = seed, passee = FALSE, duree_ms = ms)
	local_mocked_bindings(
		ia_seuil = function() 2L,
		stats_charger = function(session_ids = NULL, user_id = NULL) {
			list(list(reponses = list(r("A", 6000), r("B", 500), r("C", 300))))
		}
	)
	expect_identical(ia_etat("u")$n, 1L)
})

test_that("les garde-fous sont inactifs dans la version à télécharger", {
	withr::local_envvar(CANDIDEQUIZZ_AUTH_MODE = "local")
	expect_false(anti_abus_actif())
	expect_true(anti_abus_seance_autorisee("local")$ok)
	expect_true(anti_abus_ia_autorisee("local")$ok)
	expect_true(anti_abus_reponse_comptee(list(duree_ms = 100)))
})

test_that("un compte ne prépare qu'une séance à la fois", {
	expect_true(anti_abus_verrouiller("verrou_test"))
	expect_false(anti_abus_verrouiller("verrou_test"))
	anti_abus_liberer("verrou_test")
	expect_true(anti_abus_verrouiller("verrou_test"))
	anti_abus_liberer("verrou_test")
})
