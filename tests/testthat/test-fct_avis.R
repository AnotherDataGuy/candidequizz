test_that("avis_enregistrer garde la note de séance et la note finale", {
	s <- list(session_id = "s1", user_id = "u1", parametres = list(profil = "libre"), avis = list())
	s <- avis_enregistrer(s, "q1", note = 3, commentaire = "", moment = "seance")
	expect_equal(s$avis$q1$note_seance, 3L)
	expect_null(s$avis$q1$note)
	expect_true(is.na(s$avis$q1$commentaire))

	s <- avis_enregistrer(s, "q1", note = 8, commentaire = " trop dur ", moment = "finale")
	expect_equal(s$avis$q1$note_seance, 3L)
	expect_equal(s$avis$q1$note, 8L)
	expect_equal(s$avis$q1$commentaire, "trop dur")
})

test_that("avis_enregistrer refuse une note hors de 1 à 10", {
	s <- list(avis = list())
	expect_error(avis_enregistrer(s, "q1", note = 0), class = "candidequizz_avis_invalide")
	expect_error(avis_enregistrer(s, "q1", note = 11), class = "candidequizz_avis_invalide")
	expect_error(avis_enregistrer(s, "q1", note = NULL), class = "candidequizz_avis_invalide")
})

test_that("avis_tableau signale une note modifiée à la fin", {
	s <- list(
		session_id = "s1", user_id = "u1", parametres = list(profil = "libre"),
		items = list(
			list(item_id = "q1", indice_seance = 1L, enonce = "Question 1 ?"),
			list(item_id = "q2", indice_seance = 2L, enonce = "Question 2 ?")
		),
		reponses = list(
			list(item_id = "q1", indice_seance = 1L, seed_id = "g1", is_correct = TRUE, passee = FALSE),
			list(item_id = "q2", indice_seance = 2L, seed_id = "g2", is_correct = FALSE, passee = FALSE)
		),
		avis = list()
	)
	s <- avis_enregistrer(s, "q1", 4, moment = "seance")
	s <- avis_enregistrer(s, "q1", 4, moment = "finale")
	s <- avis_enregistrer(s, "q2", 2, moment = "seance")
	s <- avis_enregistrer(s, "q2", 7, moment = "finale")

	t <- avis_tableau(list(s))
	expect_equal(nrow(t), 2L)
	expect_equal(t$enonce, c("Question 1 ?", "Question 2 ?"))
	expect_equal(t$note_modifiee, c(FALSE, TRUE))
	expect_equal(t$seed_id, c("g1", "g2"))
})

test_that("avis_questions préremplit la note finale avec la note de séance", {
	s <- list(
		items = list(list(item_id = "q1", indice_seance = 1L, enonce = "Q ?")),
		reponses = list(list(item_id = "q1", indice_seance = 1L, is_correct = TRUE)),
		avis = list()
	)
	s <- avis_enregistrer(s, "q1", 6, moment = "seance")
	expect_equal(avis_questions(s)[[1]]$note, 6L)
	expect_false(avis_complets(s))
})

test_that("parametres_seance transmet avis_pendant, y compris en examen blanc", {
	expect_false(parametres_seance()$avis_pendant)
	expect_true(parametres_seance(avis_pendant = TRUE)$avis_pendant)
	expect_true(parametres_seance(profil = "examen_blanc", avis_pendant = TRUE)$avis_pendant)
})

test_that("avis_anterieurs reconnaît une question par graine et énoncé, la note la plus récente l'emporte", {
	q <- function(seed, enonce, note, com = NA_character_) {
		list(seed_id = seed, enonce = enonce, note = note, commentaire = com)
	}
	local_mocked_bindings(
		stats_charger = function(session_ids = NULL, user_id = NULL) {
			list(
				list(session_id = "en_cours", qs = list(q("A", "Qa ?", 9L))),
				list(session_id = "s2", qs = list(q("A", "Qa ?", 4L, "bof"), q("B", "Qb ?", NA_integer_))),
				list(session_id = "s1", qs = list(q("A", "Qa ?", 2L), q("A", "Qa reformulée ?", 7L)))
			)
		},
		avis_questions = function(seance) seance$qs
	)
	notes <- avis_anterieurs("u", sauf_session_id = "en_cours")
	expect_identical(notes[[avis_cle("A", "Qa ?")]]$note, 4L)
	expect_identical(notes[[avis_cle("A", " Qa ? ")]]$commentaire, "bof")
	expect_identical(notes[[avis_cle("A", "Qa reformulée ?")]]$note, 7L)
	expect_null(notes[[avis_cle("B", "Qb ?")]])
})

test_that("les commentaires sont bornés et rendus inertes pour Excel", {
	s <- avis_enregistrer(list(avis = list()), "q1", note = 5, commentaire = strrep("a", 5000), moment = "finale")
	expect_identical(nchar(s$avis$q1$commentaire), avis_commentaire_max())
	expect_identical(
		avis_csv_inerte(c("=SOMME(A1)", "+1", "-x", "@a", "ok", NA, "a=b")),
		c("'=SOMME(A1)", "'+1", "'-x", "'@a", "ok", NA, "a=b")
	)
})
