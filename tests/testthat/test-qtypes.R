# Validation et notation des types de questions.

test_that("un item conforme de chaque type est accept\u00e9", {
	attendus <- list(
		qcm_single = consigne_test(type = "qcm_single", n_options = 3L),
		qcm_multiple = consigne_test(type = "qcm_multiple", n_options = 4L),
		vrai_faux = consigne_test(type = "vrai_faux", verite_attendue = TRUE),
		ordre = consigne_test(type = "ordre", n_options = 4L),
		texte_a_trous = consigne_test(type = "texte_a_trous", n_options = 1L)
	)

	for (type in names(attendus)) {
		item <- item_test(type)
		verdict <- conformite_valider(item, attendus[[type]])

		expect_true(verdict$ok, info = paste(type, ":", paste(verdict$motifs, collapse = " ")))
	}
})

# --- QCM à option unique ---------------------------------------------------

test_that("le QCM \u00e0 option unique refuse un nombre d'options non demand\u00e9", {
	item <- item_test("qcm_single")
	verdict <- qtype_qcm_single_valider(item, consigne_test(n_options = 4L))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "3 options fournies au lieu de 4")
})

test_that("le QCM \u00e0 option unique refuse z\u00e9ro ou deux bonnes r\u00e9ponses", {
	deux <- item_test("qcm_single", payload = list(options = list(
		list(texte = "Charles de Gaulle", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "Jean Moulin", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "Philippe P\u00e9tain", is_correct = FALSE, diagnostic_tag = "t")
	)))

	expect_false(qtype_qcm_single_valider(deux, consigne_test())$ok)

	aucune <- item_test("qcm_single", payload = list(options = list(
		list(texte = "Charles de Gaulle", is_correct = FALSE, diagnostic_tag = "t"),
		list(texte = "Jean Moulin", is_correct = FALSE, diagnostic_tag = "t"),
		list(texte = "Philippe P\u00e9tain", is_correct = FALSE, diagnostic_tag = "t")
	)))

	expect_false(qtype_qcm_single_valider(aucune, consigne_test())$ok)
})

test_that("un \u00e9nonc\u00e9 qui contient sa r\u00e9ponse est refus\u00e9", {
	item <- item_test(
		"qcm_single",
		enonce = "Charles de Gaulle a-t-il lanc\u00e9 l'appel du 18 juin ? Qui l'a lanc\u00e9 ?"
	)

	verdict <- qtype_qcm_single_valider(item, consigne_test())

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "bonne r\u00e9ponse")
})

test_that("les formulations d'options proscrites sont refus\u00e9es", {
	item <- item_test("qcm_single", payload = list(options = list(
		list(texte = "Charles de Gaulle", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "Aucune de ces r\u00e9ponses", is_correct = FALSE, diagnostic_tag = "t"),
		list(texte = "1950, sans lien avec le sujet", is_correct = FALSE, diagnostic_tag = "t")
	)))

	verdict <- qtype_qcm_single_valider(item, consigne_test())

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "proscrite")
})

test_that("deux options identiques \u00e0 l'accent pr\u00e8s sont refus\u00e9es", {
	item <- item_test("qcm_single", payload = list(options = list(
		list(texte = "Libert\u00e9", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "liberte", is_correct = FALSE, diagnostic_tag = "t"),
		list(texte = "Fraternit\u00e9", is_correct = FALSE, diagnostic_tag = "t")
	)))

	expect_false(qtype_qcm_single_valider(item, consigne_test())$ok)
})

test_that("le QCM \u00e0 option unique se note et remonte la confusion touch\u00e9e", {
	item <- item_test("qcm_single")

	juste <- qtype_qcm_single_noter(item, 1L)
	expect_identical(juste$score, 1)
	expect_true(juste$is_correct)
	expect_identical(juste$detail$diagnostic_tags, character(0))

	faux <- qtype_qcm_single_noter(item, 2L)
	expect_identical(faux$score, 0)
	expect_false(faux$is_correct)
	expect_identical(faux$detail$diagnostic_tags, "confusion_resistance")

	# Absence de réponse et réponse hors bornes valent zéro, sans erreur.
	expect_identical(qtype_qcm_single_noter(item, NA)$score, 0)
	expect_identical(qtype_qcm_single_noter(item, 99L)$score, 0)
	expect_identical(qtype_qcm_single_noter(item, integer(0))$score, 0)
})

# --- QCM à options multiples ----------------------------------------------

test_that("le QCM multiple refuse une seule bonne r\u00e9ponse ou toutes correctes", {
	une <- item_test("qcm_multiple", payload = list(options = list(
		list(texte = "La libert\u00e9", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "La prosp\u00e9rit\u00e9", is_correct = FALSE, diagnostic_tag = "t"),
		list(texte = "La s\u00e9curit\u00e9", is_correct = FALSE, diagnostic_tag = "t"),
		list(texte = "La croissance", is_correct = FALSE, diagnostic_tag = "t")
	)))

	expect_false(qtype_qcm_multiple_valider(une, consigne_test())$ok)

	toutes <- item_test("qcm_multiple", payload = list(options = list(
		list(texte = "La libert\u00e9", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "L'\u00e9galit\u00e9", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "La fraternit\u00e9", is_correct = TRUE, diagnostic_tag = ""),
		list(texte = "La la\u00efcit\u00e9", is_correct = TRUE, diagnostic_tag = "")
	)))

	expect_false(qtype_qcm_multiple_valider(toutes, consigne_test())$ok)
})

test_that("le QCM multiple note partiellement et ne descend pas sous z\u00e9ro", {
	item <- item_test("qcm_multiple")

	expect_identical(qtype_qcm_multiple_noter(item, c(1L, 2L))$score, 1)
	expect_true(qtype_qcm_multiple_noter(item, c(1L, 2L))$is_correct)

	# Une bonne sur deux, sans erreur.
	expect_identical(qtype_qcm_multiple_noter(item, 1L)$score, 0.5)
	expect_false(qtype_qcm_multiple_noter(item, 1L)$is_correct)

	# Deux bonnes et une mauvaise : la mauvaise annule une bonne.
	expect_identical(qtype_qcm_multiple_noter(item, c(1L, 2L, 3L))$score, 0.5)

	# Tout cocher ne rapporte rien.
	expect_identical(qtype_qcm_multiple_noter(item, 1:4)$score, 0)

	expect_identical(
		qtype_qcm_multiple_noter(item, 3L)$detail$diagnostic_tags,
		"confusion_economie"
	)
})

# --- Vrai ou faux ---------------------------------------------------------

test_that("le vrai-faux refuse une question et une valeur de v\u00e9rit\u00e9 non demand\u00e9e", {
	question <- item_test("vrai_faux", enonce = "La devise comporte-t-elle trois mots ?")
	expect_false(qtype_vrai_faux_valider(question, consigne_test(verite_attendue = TRUE))$ok)

	item <- item_test("vrai_faux")
	verdict <- qtype_vrai_faux_valider(item, consigne_test(verite_attendue = FALSE))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "consigne demandait")
})

test_that("le vrai-faux se note", {
	item <- item_test("vrai_faux")

	expect_true(qtype_vrai_faux_noter(item, TRUE)$is_correct)
	expect_identical(qtype_vrai_faux_noter(item, FALSE)$score, 0)
	expect_identical(qtype_vrai_faux_noter(item, FALSE)$detail$diagnostic_tags, "confusion_date")
	expect_identical(qtype_vrai_faux_noter(item, NA)$score, 0)
})

# --- Ordre ----------------------------------------------------------------

test_that("l'ordre refuse des rangs qui ne forment pas une suite", {
	item <- item_test("ordre", payload = list(elements = list(
		list(texte = "A", rang_correct = 1L),
		list(texte = "B", rang_correct = 1L),
		list(texte = "C", rang_correct = 3L),
		list(texte = "D", rang_correct = 7L)
	)))

	verdict <- qtype_ordre_valider(item, consigne_test(type = "ordre"))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "suite 1 \u00e0 4")
})

test_that("l'ordre refuse une date \u00e9crite dans un \u00e9l\u00e9ment", {
	item <- item_test("ordre", payload = list(elements = list(
		list(texte = "Prise de la Bastille (1789)", rang_correct = 1L),
		list(texte = "Appel du 18 Juin \u2014 1940", rang_correct = 2L),
		list(texte = "Loi de s\u00e9paration, 1905", rang_correct = 3L),
		list(texte = "Ve R\u00e9publique", rang_correct = 4L)
	)))

	verdict <- qtype_ordre_valider(item, consigne_test(type = "ordre"))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "Date visible")
	expect_true(vld_fuite_date("Prise de la Bastille (1789)"))
	expect_false(vld_fuite_date("Prise de la Bastille"))
})

test_that("le QCM multiple refuse une bonne r\u00e9ponse d\u00e9j\u00e0 dans l'\u00e9nonc\u00e9", {
	item <- item_test(
		"qcm_multiple",
		enonce = "La libert\u00e9 figure-t-elle parmi les valeurs ? Plusieurs r\u00e9ponses attendues."
	)

	verdict <- qtype_qcm_multiple_valider(item, consigne_test(type = "qcm_multiple", n_options = 4L))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "bonnes r\u00e9ponses")
})

test_that("l'ordre note par paires concordantes", {
	item <- item_test("ordre")

	parfait <- qtype_ordre_noter(item, c(1L, 2L, 3L, 4L))
	expect_identical(parfait$score, 1)
	expect_true(parfait$is_correct)

	# Une seule inversion sur six paires.
	presque <- qtype_ordre_noter(item, c(2L, 1L, 3L, 4L))
	expect_identical(presque$score, 5 / 6)
	expect_false(presque$is_correct)

	# Ordre exactement inverse : aucune paire concordante.
	expect_identical(qtype_ordre_noter(item, c(4L, 3L, 2L, 1L))$score, 0)

	# Une réponse incomplète ou non permutée vaut zéro, sans erreur.
	expect_identical(qtype_ordre_noter(item, c(1L, 2L))$score, 0)
	expect_identical(qtype_ordre_noter(item, c(1L, 1L, 2L, 3L))$score, 0)
})

# --- Texte à trous --------------------------------------------------------

test_that("le texte \u00e0 trous refuse un marqueur manquant ou surnum\u00e9raire", {
	manquant <- item_test("texte_a_trous", payload = list(
		texte = "La devise de la R\u00e9publique est libre.",
		trous = list(list(position = 1L, mot_correct = "libert\u00e9")),
		distracteurs = c("s\u00e9curit\u00e9")
	))

	expect_false(qtype_texte_a_trous_valider(manquant, consigne_test(type = "texte_a_trous"))$ok)

	surnumeraire <- item_test("texte_a_trous", payload = list(
		texte = "La devise est {{1}}, {{2}}, fraternit\u00e9.",
		trous = list(list(position = 1L, mot_correct = "libert\u00e9")),
		distracteurs = c("s\u00e9curit\u00e9")
	))

	verdict <- qtype_texte_a_trous_valider(surnumeraire, consigne_test(type = "texte_a_trous"))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "non d\u00e9clar\u00e9s")
})

test_that("le texte \u00e0 trous refuse deux trous attendant le m\u00eame mot", {
	# Cas rencontré en conditions réelles : « La loi {{1}} de {{2}} » avec
	# « de 1905 » puis « 1905 », qui se lit « La loi de 1905 de 1905 ».
	item <- item_test("texte_a_trous", payload = list(
		texte = "La loi {{1}} de {{2}} s\u00e9pare les \u00c9glises et l'\u00c9tat.",
		trous = list(
			list(position = 1L, mot_correct = "de 1905"),
			list(position = 2L, mot_correct = "1905")
		),
		distracteurs = c("1881", "1958")
	))

	verdict <- qtype_texte_a_trous_valider(item, consigne_test(type = "texte_a_trous"))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "contient un autre mot attendu")
})

test_that("le texte \u00e0 trous refuse un mot attendu d\u00e9j\u00e0 visible et un distracteur redondant", {
	visible <- item_test("texte_a_trous", payload = list(
		texte = "La la\u00efcit\u00e9 est un principe. Ce principe s'appelle la {{1}}.",
		trous = list(list(position = 1L, mot_correct = "la\u00efcit\u00e9")),
		distracteurs = c("\u00e9galit\u00e9")
	))

	expect_false(qtype_texte_a_trous_valider(visible, consigne_test(type = "texte_a_trous"))$ok)

	# L'énoncé compte autant que le texte. Observé en conditions réelles : un
	# énoncé annonçant « l'appel lancé pendant la Seconde Guerre mondiale »
	# avec précisément ce groupe de mots à retrouver.
	enonce_bavard <- item_test(
		"texte_a_trous",
		enonce = "Compl\u00e9tez ce texte sur l'appel lanc\u00e9 pendant la Seconde Guerre mondiale.",
		payload = list(
			texte = "L'appel du 18 juin a \u00e9t\u00e9 lanc\u00e9 contre l'occupant {{1}}.",
			trous = list(list(position = 1L, mot_correct = "pendant la Seconde Guerre mondiale")),
			distracteurs = c("pendant la R\u00e9volution")
		)
	)

	verdict <- qtype_texte_a_trous_valider(enonce_bavard, consigne_test(type = "texte_a_trous"))

	expect_false(verdict$ok)
	expect_match(paste(verdict$motifs, collapse = " "), "d\u00e9j\u00e0 visible")

	redondant <- item_test("texte_a_trous", payload = list(
		texte = "La devise de la R\u00e9publique est {{1}}, \u00e9galit\u00e9, fraternit\u00e9.",
		trous = list(list(position = 1L, mot_correct = "libert\u00e9")),
		distracteurs = c("Liberte")
	))

	expect_false(qtype_texte_a_trous_valider(redondant, consigne_test(type = "texte_a_trous"))$ok)
})

test_that("le texte \u00e0 trous note trou par trou, \u00e0 l'accent et \u00e0 la casse pr\u00e8s", {
	item <- item_test("texte_a_trous", payload = list(
		texte = "La devise est {{1}}, {{2}}, fraternit\u00e9.",
		trous = list(
			list(position = 1L, mot_correct = "libert\u00e9"),
			list(position = 2L, mot_correct = "\u00e9galit\u00e9")
		),
		distracteurs = c("s\u00e9curit\u00e9", "prosp\u00e9rit\u00e9")
	))

	expect_identical(qtype_texte_a_trous_noter(item, c("libert\u00e9", "\u00e9galit\u00e9"))$score, 1)
	expect_true(qtype_texte_a_trous_noter(item, c("LIBERTE", "egalite"))$is_correct)
	expect_identical(qtype_texte_a_trous_noter(item, c("libert\u00e9", "s\u00e9curit\u00e9"))$score, 0.5)
	expect_identical(qtype_texte_a_trous_noter(item, character(0))$score, 0)
})

test_that("la banque de mots m\u00eale bonnes r\u00e9ponses et distracteurs de fa\u00e7on stable", {
	item <- item_test("texte_a_trous")
	banque <- qtype_texte_a_trous_banque(item)

	expect_true("libert\u00e9" %in% banque)
	expect_true(all(c("s\u00e9curit\u00e9", "prosp\u00e9rit\u00e9") %in% banque))
	expect_identical(banque, qtype_texte_a_trous_banque(item))
})

test_that("chaque type sait formuler sa bonne r\u00e9ponse sans que le bilan la devine", {
	expect_match(question_type_solution(item_test("qcm_single")), "Charles de Gaulle")
	expect_match(question_type_solution(item_test("qcm_multiple")), "libert")
	expect_identical(question_type_solution(item_test("vrai_faux")), "Vrai")
	expect_match(question_type_solution(item_test("ordre")), "R\u00e9volution")
	expect_match(question_type_solution(item_test("texte_a_trous")), "libert")
})

test_that("l'ordre initial d'une remise en ordre est stable et m\u00e9lang\u00e9", {
	item <- item_test("ordre")
	premier <- qtype_ordre_depart(item)
	second <- qtype_ordre_depart(item)

	expect_identical(premier, second)
	expect_setequal(premier, seq_along(item$payload$elements))
	expect_false(identical(premier, seq_along(item$payload$elements)))
})

test_that("d\u00e9placer un \u00e9l\u00e9ment pr\u00e9serve la permutation", {
	expect_identical(qtype_ordre_deplacer(1:4, 2L, 1L), c(2L, 1L, 3L, 4L))
	expect_identical(qtype_ordre_deplacer(1:4, 1L, 3L), c(2L, 3L, 1L, 4L))
	expect_identical(qtype_ordre_deplacer(1:4, 1L, 0L), 1:4)
})

test_that("un texte \u00e0 trous se d\u00e9coupe en segments texte / num\u00e9ro", {
	segments <- qtype_texte_a_trous_segments("La loi {{1}} de {{2}}.")

	expect_identical(segments[[1L]], "La loi ")
	expect_identical(segments[[2L]], 1L)
	expect_identical(segments[[3L]], " de ")
	expect_identical(segments[[4L]], 2L)
})

test_that("les options d'un QCM sont m\u00e9lang\u00e9es \u00e0 l'assemblage", {
	item <- item_depuis_graine(consigne_test(n_options = 4L))
	textes <- vapply(item$payload$options, function(o) o$texte, character(1))
	corrects <- vapply(item$payload$options, function(o) isTRUE(o$is_correct), logical(1))

	expect_setequal(
		textes,
		c("Libert\u00e9, \u00c9galit\u00e9, Fraternit\u00e9", "Travail, Famille, Patrie", "Unit\u00e9, Justice, Libert\u00e9", "Paix, Ordre, Progr\u00e8s")
	)
	expect_identical(sum(corrects), 1L)
	expect_true(item$payload$options[[which(corrects)]]$texte == "Libert\u00e9, \u00c9galit\u00e9, Fraternit\u00e9")

	positions <- replicate(40L, {
		it <- item_melanger_options(item_depuis_llm(
			brut = list(
				enonce = "x",
				explication = "y",
				options = list(
					list(texte = "juste", is_correct = TRUE, diagnostic_tag = ""),
					list(texte = "faux1", is_correct = FALSE, diagnostic_tag = "a"),
					list(texte = "faux2", is_correct = FALSE, diagnostic_tag = "b")
				)
			),
			consigne = consigne_test()
		))
		which(vapply(it$payload$options, function(o) isTRUE(o$is_correct), logical(1)))[[1L]]
	})

	expect_gt(length(unique(positions)), 1L)
})

test_that("un vrai/faux n'a pas ses champs permut\u00e9s comme des options", {
	item <- item_test("vrai_faux")
	expect_identical(item_melanger_options(item)$payload$reponse, TRUE)
})
