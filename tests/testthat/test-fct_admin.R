# Tableau de bord d'administration : calculs (sans base).

seance_admin <- function(id, user_id, justes, jours_avant, maintenant, seeds = NULL) {
	quand <- maintenant - jours_avant * 86400
	seeds <- seeds %||% paste0("G-", seq_along(justes))
	list(
		session_id = id, user_id = user_id, cree_le = quand,
		reponses = lapply(seq_along(justes), function(i) list(
			seed_id = seeds[[i]], is_correct = justes[[i]], passee = FALSE,
			repondu_le = quand + i, duree_ms = 9000, premiere_interaction_ms = 3000
		))
	)
}

test_that("le tableau des comptes calcule activit\u00e9, r\u00e9ussite et statut", {
	maintenant <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
	comptes <- data.frame(
		user_id = c("u1", "u2", "u3", "u0"),
		username = c("marie", "paulo", "tonton", "admin"),
		is_admin = c(FALSE, FALSE, FALSE, TRUE),
		created_at = rep("2026-10-01 10:00:00", 4),
		stringsAsFactors = FALSE
	)
	seances <- list(
		seance_admin("s1", "u1", c(TRUE, TRUE, FALSE, TRUE), 0, maintenant),
		seance_admin("s2", "u1", c(TRUE, FALSE), 2, maintenant, seeds = c("G-1", "G-9")),
		seance_admin("s3", "u2", c(FALSE, FALSE), 30, maintenant)
	)

	t <- admin_comptes_tableau(comptes, seances, maintenant = maintenant, seuil_ia = 400L)

	expect_identical(t$username[[1L]], "marie")
	m <- t[t$username == "marie", ]
	expect_identical(m$n_seances, 2L)
	expect_identical(m$n_reponses, 6L)
	expect_equal(m$taux_reussite, 4 / 6)
	expect_identical(m$n_distinctes, 5L)   # G-1..G-4 + G-9
	expect_identical(m$statut, "actif")

	expect_identical(t$statut[t$username == "paulo"], "inactif")
	expect_identical(t$statut[t$username == "tonton"], "jamais")
	expect_true(is.na(t$taux_reussite[t$username == "tonton"]))
	# Jamais venus en dernier.
	expect_identical(utils::tail(t$username, 2L), c("admin", "tonton"))
	expect_true(t$is_admin[t$username == "admin"])
})

test_that("questions pass\u00e9es et r\u00e9ponses trop rapides : exclues ou non compt\u00e9es", {
	maintenant <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
	s <- seance_admin("s1", "u1", c(TRUE, TRUE), 0, maintenant)
	s$reponses[[2L]]$passee <- TRUE
	s$reponses[[1L]]$duree_ms <- 300
	s$reponses[[1L]]$premiere_interaction_ms <- 100
	r <- admin_reponses(list(s))
	expect_identical(nrow(r), 1L)
	expect_identical(r$compte, anti_abus_reponse_comptee(s$reponses[[1L]]))
})

test_that("filtre des comptes par alias et statut", {
	t <- data.frame(username = c("marie", "mario", "paulo"), statut = c("actif", "jamais", "actif"))
	expect_identical(admin_comptes_filtrer(t, "MAR")$username, c("marie", "mario"))
	expect_identical(admin_comptes_filtrer(t, "", "actif")$username, c("marie", "paulo"))
	expect_identical(admin_comptes_filtrer(t, "ma", "jamais")$username, "mario")
})

test_that("activit\u00e9 par jour sur la fen\u00eatre demand\u00e9e", {
	maintenant <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
	seances <- list(
		seance_admin("s1", "u1", c(TRUE, TRUE, FALSE), 0, maintenant),
		seance_admin("s2", "u2", c(TRUE), 0, maintenant),
		seance_admin("s3", "u1", c(TRUE), 3, maintenant),
		seance_admin("s4", "u1", c(TRUE), 90, maintenant)
	)
	a <- admin_activite_jours(seances, jours = 7L, maintenant = maintenant)
	expect_identical(nrow(a), 7L)
	expect_identical(a$jour[[7L]], as.Date("2026-11-19"))
	expect_identical(a$n_reponses[[7L]], 4L)
	expect_identical(a$n_comptes[[7L]], 2L)
	expect_identical(a$n_reponses[[4L]], 1L)
	expect_identical(sum(a$n_reponses), 5L)
})

test_that("chiffres cl\u00e9s et th\u00e8mes", {
	comptes <- data.frame(
		n_reponses = c(10L, 0L), taux_reussite = c(0.7, NA), statut = c("actif", "jamais")
	)
	activite <- data.frame(n_reponses = c(rep(0L, 20), rep(1L, 10)))
	retours <- data.frame(
		theme = c("Histoire", "Histoire", NA), n_reponses = c(10L, 30L, 5L),
		taux_echec = c(0.5, 0.1, 0.9), n_commentaires = c(2L, 0L, 1L),
		statut = c("a_traiter", "a_traiter", "traite"), stringsAsFactors = FALSE
	)
	k <- admin_chiffres(comptes, activite, retours)
	expect_identical(k$n_comptes, 2L)
	expect_identical(k$n_actifs, 1L)
	expect_identical(k$n_jamais, 1L)
	expect_identical(k$reponses_7j, 7L)
	expect_equal(k$reussite, 0.7)
	expect_identical(k$a_revoir, 1L)
	expect_identical(k$n_commentaires, 3L)

	th <- admin_themes(retours)
	expect_identical(th$theme, c("Autre", "Histoire"))
	expect_equal(th$taux_echec[th$theme == "Histoire"], (5 + 3) / 40)
	expect_identical(nrow(admin_themes(NULL)), 0L)
})

test_that("mots de passe g\u00e9n\u00e9r\u00e9s : forme, longueur et vari\u00e9t\u00e9", {
	mdp <- replicate(50, admin_mot_de_passe())
	expect_true(all(grepl("^[a-km-zA-HJ-NP-Z2-9]{4}-[a-km-zA-HJ-NP-Z2-9]{4}-[a-km-zA-HJ-NP-Z2-9]{4}$", mdp)))
	expect_false(any(grepl("[01lIO]", mdp)))
	expect_identical(length(unique(mdp)), 50L)
	expect_true(all(nchar(mdp) >= 8L))
})

test_that("\u00ab depuis \u00bb en mots", {
	m <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
	jour <- function(n) as.numeric(m) - n * 86400
	expect_identical(admin_depuis(NA_real_, m), "aucune s\u00e9ance")
	expect_identical(admin_depuis(jour(0), m), "aujourd'hui")
	expect_identical(admin_depuis(jour(1), m), "hier")
	expect_identical(admin_depuis(jour(5), m), "il y a 5 j")
	expect_identical(admin_depuis(jour(21), m), "il y a 3 sem.")
	expect_identical(admin_depuis(jour(90), m), "il y a 3 mois")
})
