# Séances réalistes pour les tests d'écrans.
#
# Construites par les fonctions de l'application elle-même (banque réelle,
# plan et lots de generer_lot(), réponses notées par reponse_nouvelle(), avis
# par avis_enregistrer()), sans IA : les données de test ne peuvent pas
# s'écarter du format réel des séances. Déterministes (graine fixée).

#' Une séance complète (ou entamée), sans IA
#'
#' @param user_id Compte.
#' @param nb_questions 10, 20, 40 ou 60.
#' @param taux Probabilité de bonne réponse.
#' @param jours_avant Ancienneté de la séance, en jours.
#' @param n_repondues Questions répondues (par défaut : toutes).
#' @param avis Nombre de questions notées à la fin.
#' @param commentaire Commentaire laissé sur la première question notée.
#' @param profil Profil de séance.
#' @param graine Graine aléatoire.
#' @noRd
fixture_seance <- function(user_id = "u-test",
													 nb_questions = 10L,
													 taux = 0.7,
													 jours_avant = 0,
													 n_repondues = NULL,
													 avis = 3L,
													 commentaire = NULL,
													 profil = "libre",
													 graine = 1L) {
	withr::with_seed(graine, {
		p <- parametres_seance(
			nb_questions = nb_questions, generation = "ia_off",
			verifier_faits = FALSE, profil = profil
		)
		ctx <- generer_lot(p, lot = 1L)
		s <- seance_ajouter_lot(seance_nouvelle(parametres = p, plan = ctx$plan), ctx)
		s$user_id <- user_id

		lot <- 2L
		while (length(s$items) < s$plan$nb_questions && lot <= s$plan$n_lots) {
			suite <- generer_lot(p, plan = s$plan, lot = lot, exclure_seed_ids = seance_seeds_servies(s))
			s <- seance_ajouter_lot(s, suite)
			lot <- lot + 1L
		}

		debut <- Sys.time() - jours_avant * 86400
		s$cree_le <- debut
		n <- min(n_repondues %||% length(s$items), length(s$items))

		for (i in seq_len(n)) {
			item <- s$items[[i]]
			options <- item$payload$options
			justes <- which(vapply(options, function(o) isTRUE(o$is_correct), logical(1)))
			fausses <- setdiff(seq_along(options), justes)
			choix <- if (stats::runif(1) < taux || !length(fausses)) justes[[1L]] else sample(fausses, 1L)
			affiche <- debut + (i - 1) * 40
			s <- seance_ajouter_reponse(s, reponse_nouvelle(
				item, choix,
				affiche_le = affiche,
				premiere_interaction_ms = 3000,
				repondu_le = affiche + stats::runif(1, 8, 35)
			))
		}

		if (n == length(s$items)) {
			qs <- avis_questions(s)
			for (k in seq_len(min(avis, length(qs)))) {
				s <- avis_enregistrer(
					s, qs[[k]]$item_id, note = 5L + k,
					commentaire = if (k == 1L) commentaire else NULL,
					moment = "finale"
				)
			}
			s <- seance_clore(s, "terminee")
			s$termine_le <- debut + n * 40
		}

		s
	})
}

#' Base SQLite temporaire, avec un administrateur et des comptes d'amis
#'
#' @return Liste nommée `alias -> compte` (user_id, username...).
#' @noRd
fixture_base <- function(amis = c("marie_l", "paulo"), env = parent.frame()) {
	repertoire <- withr::local_tempdir(.local_envir = env)
	withr::local_envvar(
		c(CANDIDEQUIZZ_STORE_DIR = repertoire, CANDIDEQUIZZ_STORE_BACKEND = "sqlite"),
		.local_envir = env
	)
	comptes <- list(admin = auth_creer("admin_test", "motdepasse", admin = TRUE))
	for (a in amis) {
		comptes[[a]] <- auth_creer(a, "motdepasse", par_admin = TRUE)
	}
	comptes
}
