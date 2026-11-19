# Retours sur l'application (bouton « Un problème ? Une suggestion ? »).

local_sqlite_retours <- function(env = parent.frame()) {
	repertoire <- withr::local_tempdir(.local_envir = env)
	withr::local_envvar(
		c(CANDIDEQUIZZ_STORE_DIR = repertoire, CANDIDEQUIZZ_STORE_BACKEND = "sqlite"),
		.local_envir = env
	)
	repertoire
}

test_that("validation : genre, partie, pr\u00e9cision pour \u00ab Autre \u00bb, longueur", {
	ok <- retour_appli_valider("probleme", "bilan", texte = "  Le graphique ne s'affiche pas.  ")
	expect_identical(ok$texte, "Le graphique ne s'affiche pas.")
	expect_true(is.na(ok$partie_autre))

	autre <- retour_appli_valider("suggestion", "autre", "le pied de page", "Ajouter un lien vers le site officiel.")
	expect_identical(autre$partie_autre, "le pied de page")

	invalide <- "candidequizz_retour_appli_invalide"
	expect_error(retour_appli_valider("", "bilan", texte = "assez long texte"), class = invalide)
	expect_error(retour_appli_valider("probleme", "cuisine", texte = "assez long texte"), class = invalide)
	expect_error(retour_appli_valider("probleme", "autre", "", "assez long texte"), class = invalide)
	expect_error(retour_appli_valider("probleme", "bilan", texte = "court"), class = invalide)
	expect_error(retour_appli_valider("probleme", "bilan", texte = strrep("a", 2001)), class = invalide)
	expect_error(retour_appli_valider(NULL, NULL, NULL, NULL), class = invalide)
	expect_error(retour_appli_valider(c("probleme", "suggestion"), "bilan", texte = "assez long texte"), class = invalide)
})

test_that("partie propos\u00e9e d'office selon la section affich\u00e9e", {
	expect_identical(retour_appli_partie_depuis("statistiques"), "progression")
	expect_identical(retour_appli_partie_depuis("seances"), "composer")
	expect_null(retour_appli_partie_depuis("admin"))
	expect_null(retour_appli_partie_depuis(NULL))
	expect_true(all(c("autre", "accueil") %in% names(retour_appli_parties())))
	expect_identical(utils::tail(names(retour_appli_parties()), 1L), "autre")
})

test_that("contexte : JSON born\u00e9, navigateur lisible", {
	ctx <- jsonlite::fromJSON(retour_appli_contexte(
		list(ecran = "390x844", navigateur = strrep("x", 500), theme = "dark"),
		section = "accueil"
	))
	expect_identical(ctx$section, "accueil")
	expect_identical(nchar(ctx$navigateur), 60L)
	expect_identical(jsonlite::fromJSON(retour_appli_contexte(NULL))$ecran, "")

	ua_android <- "Mozilla/5.0 (Linux; Android 14) AppleWebKit/537.36 Chrome/131.0 Mobile Safari/537.36"
	expect_identical(retour_appli_navigateur(ua_android), "Chrome \u00b7 Android")
	ua_iphone <- "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 Version/18.0 Mobile/15E148 Safari/604.1"
	expect_identical(retour_appli_navigateur(ua_iphone), "Safari \u00b7 iOS")
	expect_identical(retour_appli_navigateur(NULL), "Autre navigateur \u00b7 syst\u00e8me inconnu")
})

test_that("enregistrer, lister, marquer ; supprim\u00e9 avec le compte", {
	local_sqlite_retours()
	auth_creer("admin_test", "motdepasse", admin = TRUE)
	alice <- auth_creer("alice_test", "motdepasse", par_admin = TRUE)

	r <- retour_appli_valider("probleme", "bilan", texte = "Le bouton Suivant ne r\u00e9pond pas.")
	id <- retour_appli_enregistrer(alice$user_id, r, retour_appli_contexte(list(ecran = "1280x800")))
	expect_true(is.integer(id) && id >= 1L)

	l <- retour_appli_lister()
	expect_identical(nrow(l), 1L)
	expect_identical(l$username, "alice_test")
	expect_identical(l$statut, "nouveau")
	expect_identical(l$partie, "bilan")
	expect_true(is.na(l$partie_autre))

	retour_appli_marquer(id, "traite")
	expect_identical(retour_appli_lister()$statut, "traite")
	expect_error(retour_appli_marquer("abc"), class = "candidequizz_retour_appli_invalide")

	auth_supprimer("alice_test")
	expect_identical(nrow(retour_appli_lister()), 0L)
})

test_that("compte inconnu refus\u00e9 ; limite par jour", {
	local_sqlite_retours()
	bob <- auth_creer("bob_test", "motdepasse", admin = TRUE)
	r <- retour_appli_valider("suggestion", "accueil", texte = "Un mode nuit plus contrast\u00e9.")

	expect_error(retour_appli_enregistrer("inconnu", r), class = "candidequizz_retour_appli_invalide")

	for (i in seq_len(retour_appli_max_jour())) retour_appli_enregistrer(bob$user_id, r)
	expect_error(retour_appli_enregistrer(bob$user_id, r), class = "candidequizz_retour_appli_invalide")
	# Le lendemain, c'est reparti.
	expect_silent(retour_appli_enregistrer(bob$user_id, r, maintenant = Sys.time() + 2 * 86400))
})

test_that("hors base SQL : pas d'enregistrement, liste vide", {
	withr::local_envvar(CANDIDEQUIZZ_STORE_BACKEND = "rds")
	r <- retour_appli_valider("suggestion", "accueil", texte = "Un mode nuit plus contrast\u00e9.")
	expect_error(retour_appli_enregistrer("u1", r), class = "candidequizz_retour_appli_invalide")
	expect_identical(nrow(retour_appli_lister()), 0L)
})
