test_that("les choix sont 2, 3, 4 et le defaut reste quatre", {
	expect_identical(parametres_seance()$n_options, 4L)
	expect_setequal(unname(seance_config_choix_options()), c("2", "3", "4", "varie"))
	expect_error(parametres_seance(n_options = 5L), class = "candidequizz_parametres_invalides")
	expect_identical(question_type("qcm_single")$n_options_defaut, 4L)
	expect_identical(question_type("qcm_multiple")$n_options_defaut, 4L)
})

test_that("les 1976 choix gardent leur score et diagnostic apres melange", {
	bank <- bank_load()
	problemes <- character(0)
	for (i in seq_len(nrow(bank$graines))) {
		graine <- bank$graines[i, ]
		consigne <- consigne_test(n_options = 4L)
		consigne$graine <- graine
		item <- item_depuis_graine(consigne)
		options <- graine$options[[1L]]
		erreurs <- graine$erreurs[[1L]]
		faux <- which(!options$is_correct)
		attendus <- setNames(erreurs$diagnostic_tag[match(paste0("M", seq_along(faux)), toupper(erreurs$mistake_id))], options$texte[faux])
		ok <- length(item$payload$options) == 4L && item$n_options == 4L &&
			identical(item$explication, graine$explication) &&
			identical(qtype_qcm_single_solution(item), options$texte[options$is_correct])
		for (j in seq_along(item$payload$options)) {
			option <- item$payload$options[[j]]
			note <- qtype_qcm_single_noter(item, j)
			tag <- if (option$is_correct) character(0) else unname(attendus[option$texte])
			ok <- ok && identical(note$is_correct, option$is_correct) &&
				identical(note$detail$diagnostic_tags, tag) && !anyNA(tag) &&
				(option$is_correct || all(nzchar(tag)))
			if (!option$is_correct) {
				ok <- ok && identical(stats_libelle_confusion(tag, bank),
					erreurs$description[match(tag, erreurs$diagnostic_tag)])
			}
		}
		if (!ok) problemes <- c(problemes, graine$seed_id)
	}
	expect_identical(problemes, character(0))
})

test_that("un repli respecte le choix explicite de deux ou trois options", {
	graine <- bank_load()$graines[1L, ]
	for (n in 2:4) {
		consigne <- consigne_test(n_options = n)
		consigne$graine <- graine
		item <- item_depuis_graine(consigne)
		expect_length(item$payload$options, n)
		expect_identical(item$n_options, n)
		expect_true(qtype_qcm_single_valider(item, consigne)$ok)
		expect_identical(sum(vld_corrects(item$payload$options)), 1L)
	}
})

test_that("trois choix sont refuses si quatre sont demandes meme en repli ou cache", {
	consigne <- consigne_test(n_options = 4L)
	for (source in c("llm", "cache", "graine_repli")) {
		item <- item_test(source = source, n_options = 4L)
		expect_false(qtype_qcm_single_valider(item, consigne)$ok)
	}
	item <- item_test(n_options = 4L)
	item$empreinte_graine <- cache_empreinte(consigne$graine)
	entree <- cache_entree(item)
	expect_false(cache_compatible(entree, consigne, entree$empreinte, 1L))
	multiple <- item_test("qcm_multiple")
	multiple$payload$options[[5L]] <- list(texte = "Autre choix", is_correct = FALSE, diagnostic_tag = "test")
	expect_false(qtype_qcm_multiple_valider(multiple, consigne_test(type = "qcm_multiple", n_options = 4L))$ok)
})

test_that("les prompts redaction et reparation demandent bien quatre choix", {
	c <- consigne_test(n_options = 4L)
	expect_match(prompt_bloc_consignes(list(c)), "exactement 4 options", fixed = TRUE)
	r <- conformite_rejet(item_test(), c, "Nombre de choix incorrect", "conformite")
	expect_match(prompt_bloc_rejet(r), "exactement 4 options", fixed = TRUE)
	expect_match(prompt_socle(), "produis quatre choix", fixed = TRUE)
})

test_that("le serveur QCM affiche quatre choix et conserve le choix corrige", {
	consigne <- consigne_test(n_options = 4L)
	consigne$graine <- bank_load()$graines[1L, ]
	it <- item_depuis_graine(consigne)
	item <- shiny::reactiveVal(it)
	notation <- shiny::reactiveVal(NULL)
	shiny::testServer(qtype_qcm_single_server, args = list(item = item, notation = notation), {
		html <- output$zone$html
		expect_length(regmatches(html, gregexpr('type="radio"', html, fixed = TRUE))[[1L]], 4L)
		for (j in 1:4) {
			note <- qtype_qcm_single_noter(it, j)
			note$reponse <- j
			notation(note)
			session$flushReact()
			html <- output$zone$html
			expect_match(html, it$payload$options[[j]]$texte, fixed = TRUE)
			expect_match(html, if (note$is_correct) "Votre r\u00e9ponse, correcte" else "Votre r\u00e9ponse, incorrecte", fixed = TRUE)
		}
	})
})
