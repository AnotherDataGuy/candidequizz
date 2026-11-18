# Type « Texte à trous ».
#
# Remplissage par banque de mots, décision validée : la correction est
# déterministe et ne coûte aucun appel LLM supplémentaire, et l'interaction par
# clic fonctionne bien au téléphone.

#' Marqueur de trou attendu dans le texte
#'
#' @param position Numéro du trou.
#'
#' @return Une chaîne de caractères, par exemple `"{{1}}"`.
#' @noRd
qtype_texte_a_trous_marqueur <- function(position) {
	paste0("{{", position, "}}")
}

#' Schéma d'un item de type texte à trous
#'
#' @param n_options Nombre de trous attendu, entre 1 et 3.
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
qtype_texte_a_trous_schema <- function(n_options = 2L) {
	ellmer::type_object(
		numero = ellmer::type_integer(
			"Le numero de la consigne a laquelle cet item repond."
		),
		enonce = ellmer::type_string(
			"La consigne donnee a l'utilisateur, par exemple : completez le texte."
		),
		texte = ellmer::type_string(
			glue::glue(
				"Le texte a trous. Chaque trou est note {{{{n}}}} ou n va de 1 a ",
				"{n_options}, dans l'ordre. N'utilise aucune autre notation."
			)
		),
		trous = ellmer::type_array(
			ellmer::type_object(
				position = ellmer::type_integer("Le numero du trou, de 1 a n."),
				mot_correct = ellmer::type_string(
					"Le mot ou groupe de mots attendu, tel qu'il doit apparaitre."
				)
			),
			glue::glue("Exactement {n_options} trous.")
		),
		distracteurs = ellmer::type_array(
			ellmer::type_string("Un mot plausible mais faux."),
			paste(
				"Deux a quatre mots faux mais credibles, qui viendront s'ajouter aux",
				"bonnes reponses dans la banque de mots proposee a l'utilisateur."
			)
		),
		explication = ellmer::type_string(
			"Explication pedagogique du texte complete."
		)
	)
}

#' Consignes de rédaction propres au type
#'
#' @param consigne La consigne de génération d'un item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_texte_a_trous_consigne <- function(consigne) {
	glue::glue(
		"Type attendu : texte a trous, avec exactement {consigne$n_options} ",
		"trous notes {{{{1}}}}, {{{{2}}}} et ainsi de suite. Le texte doit rester ",
		"court, deux phrases au plus. Chaque mot attendu est un mot ou un groupe ",
		"court, jamais une proposition entiere. Aucun mot attendu ne doit figurer ",
		"ailleurs dans le texte ni dans l'enonce, sinon le trou se devine. ",
		"Fournis deux a quatre distracteurs, de meme nature que les mots attendus ",
		"pour qu'ils soient credibles a chaque trou."
	)
}

#' Banque de mots présentée à l'utilisateur
#'
#' Bonnes réponses et distracteurs mélangés. L'ordre est dérivé du contenu
#' plutôt que tiré au hasard, afin qu'un même item présente toujours la même
#' banque, y compris après rechargement d'une séance interrompue.
#'
#' @param item L'item posé.
#'
#' @return Un vecteur de caractères.
#' @noRd
qtype_texte_a_trous_banque <- function(item) {
	corrects <- vapply(
		item$payload$trous,
		function(t) as.character(t$mot_correct),
		character(1)
	)

	distracteurs <- as.character(item$payload$distracteurs %||% character(0))
	mots <- unique(c(corrects, distracteurs))

	mots[order(vld_normaliser(mots))]
}

#' Valider un item de type texte à trous
#'
#' @param item L'item produit.
#' @param consigne La consigne correspondante.
#'
#' @return `list(ok, motifs)`.
#' @noRd
qtype_texte_a_trous_valider <- function(item, consigne) {
	motifs <- character(0)
	trous <- item$payload$trous
	texte <- item$payload$texte

	if (!vld_texte_ok(texte, mini = 10L)) {
		return(vld_verdict("Le texte \u00e0 trous est vide ou trop court."))
	}

	if (!is.list(trous) || !length(trous)) {
		return(vld_verdict("Aucun trou d\u00e9fini."))
	}

	n <- length(trous)

	if (n < 1L || n > 3L) {
		motifs <- c(motifs, glue::glue("{n} trous fournis, attendu entre 1 et 3."))
	}

	positions <- suppressWarnings(vapply(
		trous,
		function(t) as.integer(t$position %||% NA_integer_),
		integer(1)
	))

	mots <- vapply(
		trous,
		function(t) if (vld_texte_ok(t$mot_correct)) t$mot_correct else NA_character_,
		character(1)
	)

	if (anyNA(mots)) {
		motifs <- c(motifs, "Au moins un mot attendu est vide.")
		return(vld_verdict(motifs))
	}

	if (anyNA(positions) || !setequal(positions, seq_len(n))) {
		motifs <- c(
			motifs,
			glue::glue("Les positions de trous ne forment pas la suite 1 \u00e0 {n}.")
		)
		return(vld_verdict(motifs))
	}

	# Chaque trou déclaré doit avoir son marqueur dans le texte, et le texte ne
	# doit pas contenir de marqueur non déclaré.
	marqueurs <- vapply(seq_len(n), qtype_texte_a_trous_marqueur, character(1))
	absents <- marqueurs[!vapply(marqueurs, function(m) grepl(m, texte, fixed = TRUE), logical(1))]

	if (length(absents)) {
		motifs <- c(
			motifs,
			glue::glue("Marqueurs absents du texte : {paste(absents, collapse = ', ')}.")
		)
	}

	trouves <- unlist(regmatches(texte, gregexpr("\\{\\{[0-9]+\\}\\}", texte)))
	surnumeraires <- setdiff(unique(trouves), marqueurs)

	if (length(surnumeraires)) {
		motifs <- c(
			motifs,
			glue::glue(
				"Marqueurs non d\u00e9clar\u00e9s dans le texte : ",
				"{paste(surnumeraires, collapse = ', ')}."
			)
		)
	}

	# Deux trous attendant le même mot rendent le texte absurde une fois
	# complété. Observé en conditions réelles : « La loi {{1}} de {{2}} » avec
	# « de 1905 » puis « 1905 », qui se lit « La loi de 1905 de 1905 ».
	mots_n <- vld_normaliser(mots)

	if (any(duplicated(mots_n))) {
		motifs <- c(motifs, "Deux trous attendent le m\u00eame mot.")
	}

	chevauchements <- vapply(
		seq_along(mots_n),
		function(i) any(vapply(
			mots_n[-i],
			function(autre) nchar(autre) >= 3L && grepl(autre, mots_n[[i]], fixed = TRUE),
			logical(1)
		)),
		logical(1)
	)

	if (any(chevauchements)) {
		motifs <- c(
			motifs,
			glue::glue(
				"Un mot attendu contient un autre mot attendu : ",
				"{paste(sprintf('\u00ab %s \u00bb', mots[chevauchements]), collapse = ', ')}."
			)
		)
	}

	distracteurs <- as.character(item$payload$distracteurs %||% character(0))

	if (length(distracteurs) < 1L) {
		motifs <- c(motifs, "Aucun distracteur fourni : la banque de mots serait triviale.")
	}

	if (any(vld_normaliser(distracteurs) %in% vld_normaliser(mots))) {
		motifs <- c(motifs, "Un distracteur reprend un mot attendu.")
	}

	# Un mot attendu déjà visible rendrait le trou devinable. L'énoncé compte
	# autant que le texte : observé en conditions réelles, un énoncé annonçant
	# « l'appel lancé pendant la Seconde Guerre mondiale » avec précisément
	# « pendant la Seconde Guerre mondiale » à trouver.
	visible <- vld_normaliser(paste(
		item$enonce,
		gsub("\\{\\{[0-9]+\\}\\}", " ", texte)
	))

	visibles <- mots[vapply(
		vld_normaliser(mots),
		function(m) nchar(m) >= 4L && grepl(m, visible, fixed = TRUE),
		logical(1)
	)]

	if (length(visibles)) {
		motifs <- c(
			motifs,
			glue::glue(
				"Mot attendu d\u00e9j\u00e0 visible dans le texte : ",
				"{paste(sprintf('\u00ab %s \u00bb', visibles), collapse = ', ')}."
			)
		)
	}

	vld_verdict(motifs)
}

#' Noter une réponse à un texte à trous
#'
#' @param item L'item posé.
#' @param reponse Vecteur des mots choisis, un par trou, dans l'ordre des
#'   positions.
#'
#' @return `list(score, is_correct, detail)`.
#' @noRd
qtype_texte_a_trous_noter <- function(item, reponse) {
	trous <- item$payload$trous
	n <- length(trous)

	positions <- vapply(trous, function(t) as.integer(t$position), integer(1))
	attendus <- vapply(trous, function(t) as.character(t$mot_correct), character(1))
	attendus <- attendus[order(positions)]

	donnes <- as.character(reponse)

	if (length(donnes) < n) {
		donnes <- c(donnes, rep(NA_character_, n - length(donnes)))
	}

	donnes <- donnes[seq_len(n)]
	justes <- !is.na(donnes) & vld_normaliser(donnes) == vld_normaliser(attendus)

	list(
		score = if (n == 0L) 0 else sum(justes) / n,
		is_correct = all(justes),
		detail = list(
			diagnostic_tags = character(0),
			trous_justes = sum(justes),
			trous_total = n,
			mots_donnes = donnes
		)
	)
}

#' Texte complété d'un texte à trous
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_texte_a_trous_solution <- function(item) {
	texte <- item$payload$texte
	trous <- item$payload$trous
	positions <- vapply(trous, function(t) as.integer(t$position), integer(1))

	for (i in order(positions)) {
		texte <- gsub(
			qtype_texte_a_trous_marqueur(positions[[i]]),
			trous[[i]]$mot_correct,
			texte,
			fixed = TRUE
		)
	}

	texte
}

#' Segments d'un texte à trous
#'
#' Alterne du texte et des numéros de trou. Un numéro est un entier ; le reste
#' est du texte à afficher tel quel.
#'
#' @param texte Le texte contenant des marqueurs `{{n}}`.
#'
#' @return Une liste de chaînes et d'entiers.
#' @noRd
qtype_texte_a_trous_segments <- function(texte) {
	matches <- gregexpr("\\{\\{[0-9]+\\}\\}", texte)[[1L]]

	if (identical(as.integer(matches[[1L]]), -1L)) {
		return(list(texte))
	}

	debuts <- as.integer(matches)
	longueurs <- as.integer(attr(matches, "match.length"))
	segments <- list()
	curseur <- 1L

	for (i in seq_along(debuts)) {
		if (debuts[[i]] > curseur) {
			segments <- c(segments, list(substr(texte, curseur, debuts[[i]] - 1L)))
		}

		marque <- substr(texte, debuts[[i]], debuts[[i]] + longueurs[[i]] - 1L)
		segments <- c(segments, list(as.integer(gsub("[^0-9]", "", marque))))
		curseur <- debuts[[i]] + longueurs[[i]]
	}

	if (curseur <= nchar(texte)) {
		segments <- c(segments, list(substr(texte, curseur, nchar(texte))))
	}

	segments
}

#' Interface de saisie d'un texte à trous
#'
#' @param id Identifiant du module.
#'
#' @return Un emplacement rempli par le serveur du type.
#' @noRd
qtype_texte_a_trous_ui <- function(id) {
	shiny::div(
		class = "cq-reponse cq-reponse--trous",
		shiny::uiOutput(shiny::NS(id, "zone"))
	)
}

#' Serveur du type texte à trous
#'
#' Clic sur un mot de la banque : il remplit le premier trou vide, ou le trou
#' actuellement sélectionné. Clic sur un trou rempli : le mot revient à la
#' banque. Aucun mot n'est réutilisable, ce qui évite de remplir deux trous
#' avec la même étiquette.
#'
#' @param id Identifiant du module.
#' @param item Réactif renvoyant l'item à poser.
#' @param notation Réactif renvoyant la notation, ou `NULL` tant que la réponse
#'   n'est pas validée.
#'
#' @return `list(reponse)`.
#' @noRd
qtype_texte_a_trous_server <- function(id, item, notation) {
	shiny::moduleServer(id, function(input, output, session) {
		remplissage <- shiny::reactiveVal(character(0))
		selection <- shiny::reactiveVal(NA_integer_)

		n_trous <- function(it) length(it$payload$trous)

		shiny::observeEvent(item()$item_id, {
			it <- item()
			shiny::req(it)

			remplissage(rep(NA_character_, n_trous(it)))
			selection(NA_integer_)
		}, ignoreNULL = TRUE)

		shiny::observeEvent(input$action_trou, {
			if (!is.null(notation())) {
				return(invisible(NULL))
			}

			position <- suppressWarnings(as.integer(input$action_trou$position %||% NA_integer_))
			shiny::req(!is.na(position))

			mots <- remplissage()

			if (position < 1L || position > length(mots)) {
				return(invisible(NULL))
			}

			if (!is.na(mots[[position]]) && nzchar(mots[[position]])) {
				mots[[position]] <- NA_character_
				remplissage(mots)
				selection(position)
			} else {
				selection(position)
			}
		}, ignoreNULL = TRUE)

		shiny::observeEvent(input$action_mot, {
			if (!is.null(notation())) {
				return(invisible(NULL))
			}

			mot <- as.character(input$action_mot$mot %||% "")
			shiny::req(nzchar(mot))

			mots <- remplissage()
			cible <- selection()

			if (is.na(cible) || (!is.na(mots[[cible]]) && nzchar(mots[[cible]]))) {
				vides <- which(is.na(mots) | !nzchar(mots))

				if (!length(vides)) {
					return(invisible(NULL))
				}

				cible <- vides[[1L]]
			}

			mots[[cible]] <- mot
			remplissage(mots)

			vides <- which(is.na(mots) | !nzchar(mots))
			selection(if (length(vides)) vides[[1L]] else NA_integer_)
		}, ignoreNULL = TRUE)

		output$zone <- shiny::renderUI({
			it <- item()
			shiny::req(it)

			verdict <- notation()
			mots <- if (is.null(verdict)) {
				remplissage()
			} else {
				as.character(verdict$reponse %||% remplissage())
			}

			n <- n_trous(it)
			shiny::req(length(mots) == n)

			attendus <- vapply(it$payload$trous, function(t) as.character(t$mot_correct), character(1))
			positions <- vapply(it$payload$trous, function(t) as.integer(t$position), integer(1))
			attendus <- attendus[order(positions)]

			segments <- qtype_texte_a_trous_segments(it$payload$texte)
			banque <- qtype_texte_a_trous_banque(it)
			pris <- mots[!is.na(mots) & nzchar(mots)]
			disponibles <- banque[!vld_normaliser(banque) %in% vld_normaliser(pris)]

			rendu_trou <- function(position) {
				occupe <- !is.na(mots[[position]]) && nzchar(mots[[position]])
				etat <- if (is.null(verdict) || !occupe) {
					""
				} else if (vld_normaliser(mots[[position]]) == vld_normaliser(attendus[[position]])) {
					"juste"
				} else {
					"faux"
				}

				shiny::tags$button(
					type = "button",
					class = paste(
						"cq-trou",
						if (occupe) "cq-trou--rempli" else "cq-trou--vide",
						if (identical(selection(), position) && is.null(verdict)) "cq-trou--actif" else "",
						if (nzchar(etat)) paste0("cq-trou--", etat) else ""
					),
					disabled = if (is.null(verdict)) NULL else NA,
					onclick = if (is.null(verdict)) {
						sprintf(
							"Shiny.setInputValue('%s', {position: %d, nonce: Math.random()}, {priority: 'event'})",
							session$ns("action_trou"),
							position
						)
					} else {
						NULL
					},
					if (occupe) mots[[position]] else paste0("\u2026")
				)
			}

			shiny::tagList(
				if (is.null(verdict)) {
					shiny::p(
						class = "cq-aide",
						"Touchez un mot, puis un trou \u2014 ou touchez d'abord un trou."
					)
				},
				shiny::div(
					class = "cq-trous-texte",
					lapply(segments, function(segment) {
						if (is.integer(segment)) rendu_trou(segment) else shiny::span(segment)
					})
				),
				if (is.null(verdict)) {
					shiny::div(
						class = "cq-banque",
						lapply(disponibles, function(mot) {
							shiny::tags$button(
								type = "button",
								class = "cq-banque-mot",
								onclick = sprintf(
									"Shiny.setInputValue('%s', {mot: %s, nonce: Math.random()}, {priority: 'event'})",
									session$ns("action_mot"),
									jsonlite::toJSON(mot, auto_unbox = TRUE)
								),
								mot
							)
						})
					)
				} else {
					shiny::p(
						class = "cq-ordre-solution",
						shiny::strong("Texte attendu : "),
						qtype_texte_a_trous_solution(it)
					)
				}
			)
		})

		list(reponse = shiny::reactive({
			it <- item()
			shiny::req(it)

			mots <- remplissage()

			if (length(mots) != n_trous(it) || any(is.na(mots) | !nzchar(mots))) {
				return(NULL)
			}

			mots
		}))
	})
}
