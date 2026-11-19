# Écrans : construits avec des séances réalistes (helper-fixtures.R) et
# pilotés comme par un utilisateur (shiny::testServer). On vérifie les
# éléments clés, pas tout le HTML : une retouche de texte ne doit pas casser
# les tests.

html_de <- function(sortie) as.character(sortie$html %||% sortie)

# --- Passation ---------------------------------------------------------------

test_that("passation : répondre juste, faux, passer, jusqu'à la fin", {
	local_store()
	s0 <- fixture_seance(n_repondues = 0L, graine = 11L)
	seance <- shiny::reactiveVal(s0)
	fini <- 0L

	shiny::testServer(
		mod_seance_run_server,
		args = list(
			seance = seance,
			retour_differe = shiny::reactive(FALSE),
			terminer = function() fini <<- fini + 1L
		),
		{
			repondre <- function(juste) {
				it <- seance_item(seance(), position())
				opts <- it$payload$options
				bonne <- which(vapply(opts, function(o) isTRUE(o$is_correct), logical(1)))[[1L]]
				choix <- if (juste) bonne else setdiff(seq_along(opts), bonne)[[1L]]
				champ <- paste0(seance_run_saisie_id(it$type), "-choix_", it$item_id)
				do.call(session$setInputs, stats::setNames(list(as.character(choix)), champ))
				session$setInputs(valider = stats::runif(1))
			}

			expect_match(html_de(output$entete), "1")
			repondre(TRUE)
			expect_true(seance()$reponses[[1L]]$is_correct)
			session$setInputs(suivant = 1)

			repondre(FALSE)
			expect_false(seance()$reponses[[2L]]$is_correct)
			session$setInputs(suivant = 2)

			session$setInputs(passer = 1)
			expect_true(seance()$reponses[[3L]]$passee)
			session$setInputs(suivant = 3)

			n <- length(seance()$items)
			k <- 3L
			while (length(seance()$reponses) < n && k < 40L) {
				# Correction affichée (notation connue) : passer à la suivante.
				if (is.null(notation())) repondre(TRUE) else session$setInputs(suivant = 10L + k)
				k <- k + 1L
			}
			expect_identical(length(seance()$reponses), n)
			expect_identical(seance()$statut, "terminee")
		}
	)

	# La séance est enregistrée sur le disque à chaque réponse.
	relue <- store_charger(s0$session_id)
	expect_identical(length(relue$reponses), length(s0$items))
})

test_that("passation : valider sans choisir ne compte pas de réponse", {
	local_store()
	seance <- shiny::reactiveVal(fixture_seance(n_repondues = 0L, graine = 12L))
	shiny::testServer(
		mod_seance_run_server,
		args = list(seance = seance, retour_differe = shiny::reactive(FALSE), terminer = function() NULL),
		{
			session$setInputs(valider = 1)
			expect_length(seance()$reponses, 0L)
		}
	)
})

# --- Avis de fin de séance -------------------------------------------------------

test_that("avis : il faut noter chaque question, puis les notes sont transmises", {
	local_store()
	s <- fixture_seance(avis = 0L, graine = 13L)
	recu <- NULL
	seance <- shiny::reactiveVal(s)
	shiny::testServer(
		mod_seance_avis_server,
		args = list(seance = seance, valider = function(...) recu <<- seance()),
		{
			expect_match(html_de(output$liste), "cq-avis-liste", fixed = TRUE)
			n <- length(avis_questions(s))

			session$setInputs(note_1 = "4", valider = 1)
			expect_null(recu)

			notes <- stats::setNames(as.list(as.character(seq_len(n) %% 10 + 1)), paste0("note_", seq_len(n)))
			do.call(session$setInputs, notes)
			session$setInputs(valider = 2)
		}
	)
	expect_false(is.null(recu))
	expect_identical(nrow(avis_tableau(list(recu))), length(avis_questions(s)))
	expect_true(avis_complets(recu))
})

# --- Bilan de séance ---------------------------------------------------------------

test_that("bilan : score, thèmes et exports d'une séance terminée", {
	local_store()
	withr::local_envvar(OPENAI_API_KEY = "")
	s <- fixture_seance(nb_questions = 20L, taux = 0.6, graine = 14L)
	store_sauver(s)
	seance <- shiny::reactiveVal(s)

	shiny::testServer(
		mod_seance_bilan_server,
		args = list(seance = seance, rejouer = function(...) NULL),
		{
			session$flushReact()
			html <- html_de(output$rapport)
			justes <- sum(vapply(s$reponses, `[[`, logical(1), "is_correct"))
			expect_match(html, as.character(justes))
			expect_match(html, s$reponses[[1L]]$theme, fixed = TRUE)
			# Le bilan a reçu sa synthèse (repli sans IA) et a été enregistré.
			expect_false(is.null(seance()$synthese))

			expect_true(file.exists(output$dl_csv))
			expect_gt(file.size(output$dl_xlsx), 1000)
			expect_match(paste(readLines(output$dl_html, warn = FALSE), collapse = "\n"), "<html", fixed = TRUE)
		}
	)
})

# --- Carnet de progression -----------------------------------------------------------

test_that("progression : vide, puis rempli par plusieurs séances", {
	comptes <- fixture_base(amis = c("marie_l", "paulo"))
	marie <- comptes$marie_l
	for (k in 1:3) {
		store_sauver(fixture_seance(marie$user_id, nb_questions = 20L, taux = 0.5 + k / 10,
																jours_avant = 10 - 3 * k, graine = 20L + k), user_id = marie$user_id)
	}

	shiny::testServer(
		mod_stats_server,
		args = list(utilisateur = shiny::reactive(comptes$paulo)),
		{
			expect_match(html_de(output$rapport), "Pas encore de traces", fixed = TRUE)
		}
	)

	shiny::testServer(
		mod_stats_server,
		args = list(utilisateur = shiny::reactive(marie)),
		{
			session$setInputs(perimetre = "toutes")
			expect_match(html_de(output$portee), "3 séances", fixed = TRUE)
			html <- html_de(output$rapport)
			expect_gt(nchar(html), 5000)
			expect_no_match(html, "Pas encore de traces", fixed = TRUE)
			expect_true(file.exists(output$dl_csv))
			expect_gt(file.size(output$dl_xlsx), 1000)
			expect_true(file.exists(output$dl_html))
		}
	)
})

# --- Retours sur l'appli ----------------------------------------------------------------

test_that("retours appli : erreurs lisibles, puis enregistrement", {
	comptes <- fixture_base(amis = "marie_l")
	shiny::testServer(
		mod_retour_appli_server,
		args = list(utilisateur = shiny::reactive(comptes$marie_l), section = shiny::reactive("statistiques")),
		{
			session$setInputs(contexte = list(ecran = "390x844", navigateur = "Mozilla/5.0 (Linux; Android 14) Chrome/131.0", theme = "dark"))
			session$setInputs(ouvrir = 1)
			expect_match(html_de(output$contexte), "Chrome · Android")

			session$setInputs(deposer = 1)
			expect_match(html_de(output$erreur), "problème ou d'une idée")
			expect_identical(nrow(retour_appli_lister()), 0L)

			session$setInputs(genre = "probleme", partie = "progression", texte = "La courbe ne s'affiche pas sur mon téléphone.")
			session$setInputs(deposer = 2)
		}
	)
	l <- retour_appli_lister()
	expect_identical(nrow(l), 1L)
	expect_identical(l$partie, "progression")
	expect_identical(jsonlite::fromJSON(l$contexte)$section, "statistiques")
})

# --- Administration -------------------------------------------------------------------------

test_that("administration : rien pour un ami, même en forçant les actions", {
	comptes <- fixture_base(amis = c("marie_l", "paulo"))
	shiny::testServer(
		mod_admin_server,
		args = list(utilisateur = shiny::reactive(comptes$marie_l)),
		{
			expect_error(output$panneau, class = "shiny.silent.error")
			session$setInputs(action = list(quoi = "suppr", alias = "paulo"))
			session$setInputs(suppr_ok = 1)
			session$setInputs(nouvel_alias = "intrus", nouveau_ok = 1)
		}
	)
	expect_false(is.null(auth_trouver("paulo")))
	expect_null(auth_trouver("intrus"))
})

test_that("administration : chiffres, comptes, mot de passe, suppression", {
	comptes <- fixture_base(amis = c("marie_l", "paulo", "tonton"))
	marie <- comptes$marie_l
	store_sauver(fixture_seance(marie$user_id, commentaire = "Les choix se ressemblent", graine = 31L), user_id = marie$user_id)
	store_sauver(fixture_seance(comptes$paulo$user_id, jours_avant = 20, graine = 32L), user_id = comptes$paulo$user_id)
	retour_appli_enregistrer(marie$user_id, retour_appli_valider("suggestion", "accueil", texte = "Un mode révision express."))

	shiny::testServer(
		mod_admin_server,
		args = list(utilisateur = shiny::reactive(comptes$admin)),
		{
			d <- donnees()
			expect_identical(d$chiffres$n_comptes, 4L)
			expect_identical(d$chiffres$n_actifs, 1L)
			expect_identical(d$chiffres$appli_nouveaux, 1L)
			expect_identical(d$comptes$statut[d$comptes$username == "paulo"], "inactif")

			vue <- html_de(output$vue)
			expect_match(vue, "tonton")                      # compte sans séance, à surveiller
			expect_match(vue, "retour sur l'appli", fixed = TRUE)

			session$setInputs(cherche = "pau", statut = "tous")
			liste <- html_de(output$comptes)
			expect_match(liste, "paulo")
			expect_no_match(liste, "marie_l")
			# Pas de bouton de suppression pour l'administrateur.
			session$setInputs(cherche = "admin")
			expect_no_match(html_de(output$comptes), "Supprimer le compte admin_test", fixed = TRUE)

			# Nouveau mot de passe : l'ancien ne marche plus.
			session$setInputs(action = list(quoi = "mdp", alias = "paulo"))
			session$setInputs(mdp_ok = 1)
			expect_error(auth_connecter("paulo", "motdepasse"), class = "candidequizz_auth_invalide")

			# Suppression, puis la liste est relue.
			session$setInputs(action = list(quoi = "suppr", alias = "tonton"))
			session$setInputs(suppr_ok = 1)
			expect_null(auth_trouver("tonton"))
			expect_identical(donnees()$chiffres$n_comptes, 3L)

			# Un compte administrateur ne se supprime pas.
			session$setInputs(action = list(quoi = "suppr", alias = "admin_test"))
			session$setInputs(suppr_ok = 2)
			expect_false(is.null(auth_trouver("admin_test")))

			# Nouveau compte : alias refusé, puis accepté.
			session$setInputs(nouveau = 1, nouvel_alias = "Pas Bon!", nouveau_ok = 1)
			expect_match(html_de(output$nouvel_erreur), "3 à 40")
			session$setInputs(nouvel_alias = "nouvelle_amie", nouveau_ok = 2)
			expect_false(is.null(auth_trouver("nouvelle_amie")))

			# Retours appli : marquer comme traité.
			session$setInputs(appli_statut = "nouveau", appli_genre = "tous")
			expect_match(html_de(output$appli), "révision express")
			id <- retour_appli_lister()$id[[1L]]
			session$setInputs(appli_marquer = list(id = id, statut = "traite"))
			expect_identical(retour_appli_lister()$statut, "traite")
			expect_match(html_de(output$appli), "Rien à lire")
		}
	)
})

test_that("administration : exports réservés à l'administrateur", {
	comptes <- fixture_base(amis = "marie_l")
	store_sauver(fixture_seance(comptes$marie_l$user_id, graine = 41L), user_id = comptes$marie_l$user_id)

	shiny::testServer(mod_admin_server, args = list(utilisateur = shiny::reactive(comptes$admin)), {
		avis <- utils::read.csv(output$admin_avis)
		expect_gt(nrow(avis), 0L)
		expect_gt(file.size(output$admin_sqlite), 0)
	})
	shiny::testServer(mod_admin_server, args = list(utilisateur = shiny::reactive(comptes$marie_l)), {
		expect_lte(length(readLines(output$admin_avis, warn = FALSE)), 1L)
		expect_error(output$admin_sqlite)
	})
})

# --- Fenêtre d'un thème (carrousel de l'accueil) -------------------------------------

test_that("fenêtre de thème : titre, sous-thèmes ; thème inconnu ignoré", {
	themes <- bank_themes(bank_load())
	for (id in themes$theme_id) {
		html <- as.character(cq_theme_modal(id))
		expect_match(html, themes$theme[themes$theme_id == id], fixed = TRUE, info = id)
	}
	expect_null(cq_theme_modal("INCONNU"))
	expect_null(cq_theme_modal(NULL))
})
