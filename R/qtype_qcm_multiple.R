# Type « QCM à options multiples ».

#' Schéma d'un item de type QCM à options multiples
#'
#' @param n_options Nombre d'options attendu.
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
qtype_qcm_multiple_schema <- function(n_options = 4L) {
	ellmer::type_object(
		numero = ellmer::type_integer(
			"Le numero de la consigne a laquelle cet item repond."
		),
		enonce = ellmer::type_string(
			paste(
				"La question posee, en francais. Elle doit indiquer clairement",
				"que plusieurs reponses sont attendues."
			)
		),
		options = ellmer::type_array(
			ellmer::type_object(
				texte = ellmer::type_string("Le libelle de l'option."),
				is_correct = ellmer::type_boolean("Cette option fait-elle partie des bonnes reponses."),
				diagnostic_tag = ellmer::type_string(
					paste(
						"Pour une option fausse : l'identifiant court de la confusion",
						"qu'elle revele. Pour une option correcte : chaine vide."
					)
				)
			),
			glue::glue(
				"Exactement {n_options} options, dont 2 a {min(4L, n_options - 1L)} correctes. ",
				"Jamais toutes correctes."
			)
		),
		explication = ellmer::type_string(
			"Explication pedagogique couvrant toutes les bonnes reponses."
		)
	)
}

#' Consignes de rédaction propres au type
#'
#' @param consigne La consigne de génération d'un item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_qcm_multiple_consigne <- function(consigne) {
	glue::glue(
		"Type attendu : QCM a options multiples, avec exactement ",
		"{consigne$n_options} options dont 2 a {min(4L, consigne$n_options - 1L)} correctes. L'enonce doit ",
		"signaler explicitement que plusieurs reponses sont attendues. ",
		"Aucune des bonnes reponses ne doit figurer dans l'enonce."
	)
}

#' Valider un item de type QCM à options multiples
#'
#' @param item L'item produit.
#' @param consigne La consigne correspondante.
#'
#' @return `list(ok, motifs)`.
#' @noRd
qtype_qcm_multiple_valider <- function(item, consigne) {
	motifs <- vld_options_communes(item$payload$options)

	if (length(motifs)) {
		return(vld_verdict(motifs))
	}

	options <- item$payload$options
	corrects <- vld_corrects(options)
	n <- length(options)

	attendu <- as.integer(consigne$n_options)
	if (n != attendu) {
		motifs <- c(motifs, glue::glue("{n} options fournies au lieu de {attendu}."))
	}

	if (sum(corrects) < 2L) {
		motifs <- c(
			motifs,
			glue::glue("{sum(corrects)} option correcte, attendu au moins 2.")
		)
	}

	if (sum(corrects) > 4L) {
		motifs <- c(
			motifs,
			glue::glue("{sum(corrects)} options correctes, attendu au plus 4.")
		)
	}

	if (n > 0L && sum(corrects) == n) {
		motifs <- c(motifs, "Toutes les options sont correctes : la question ne discrimine rien.")
	}

	if (any(corrects) && vld_fuites_reponses(item$enonce, vld_textes(options)[corrects])) {
		motifs <- c(motifs, "L'enonc\u00e9 contient d\u00e9j\u00e0 une des bonnes r\u00e9ponses.")
	}

	vld_verdict(motifs)
}

#' Noter une réponse à un QCM à options multiples
#'
#' Notation partielle : on retire les mauvaises cases cochées des bonnes, sans
#' descendre sous zéro. Cocher tout ne rapporte donc rien.
#'
#' @param item L'item posé.
#' @param reponse Les indices des options cochées.
#'
#' @return `list(score, is_correct, detail)`.
#' @noRd
qtype_qcm_multiple_noter <- function(item, reponse) {
	options <- item$payload$options
	corrects <- vld_corrects(options)
	choix <- suppressWarnings(as.integer(reponse))
	choix <- choix[!is.na(choix) & choix >= 1L & choix <= length(options)]
	choix <- unique(choix)

	coche <- rep(FALSE, length(options))
	coche[choix] <- TRUE

	justes_coches <- sum(coche & corrects)
	faux_coches <- sum(coche & !corrects)
	nb_justes <- sum(corrects)

	score <- if (nb_justes == 0L) 0 else max(0, (justes_coches - faux_coches) / nb_justes)

	tags <- vapply(
		options[coche & !corrects],
		function(o) as.character(o$diagnostic_tag %||% ""),
		character(1)
	)

	list(
		score = score,
		is_correct = identical(coche, corrects),
		detail = list(
			diagnostic_tags = unname(tags[nzchar(tags)]),
			options_choisies = vld_textes(options)[coche]
		)
	)
}

#' Texte des bonnes réponses d'un QCM à options multiples
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_qcm_multiple_solution <- function(item) {
	textes <- vld_textes(item$payload$options)
	corrects <- vld_corrects(item$payload$options)

	paste(textes[corrects], collapse = " ; ")
}

#' Interface de saisie d'un QCM à options multiples
#'
#' @param id Identifiant du module.
#'
#' @return Un emplacement rempli par le serveur du type.
#' @noRd
qtype_qcm_multiple_ui <- function(id) {
	shiny::div(
		class = "cq-reponse",
		shiny::uiOutput(shiny::NS(id, "zone"))
	)
}

#' Serveur du type QCM à options multiples
#'
#' @param id Identifiant du module.
#' @param item Réactif renvoyant l'item à poser.
#' @param notation Réactif renvoyant la notation, ou `NULL` tant que la réponse
#'   n'est pas validée.
#'
#' @return `list(reponse)`.
#' @noRd
qtype_qcm_multiple_server <- function(id, item, notation) {
	shiny::moduleServer(id, function(input, output, session) {
		champ <- function(it) paste0("choix_", it$item_id)

		output$zone <- shiny::renderUI({
			it <- item()
			shiny::req(it)

			verdict <- notation()

			if (is.null(verdict)) {
				choix <- qtype_qcm_single_choix(it)

				return(shiny::tagList(
					shiny::checkboxGroupInput(
						inputId = session$ns(champ(it)),
						label = "S\u00e9lectionnez toutes les bonnes r\u00e9ponses",
						choiceNames = choix$noms,
						choiceValues = choix$valeurs,
						selected = character(0),
						width = "100%"
					)
				))
			}

			options <- it$payload$options
			lettres <- LETTERS[seq_along(options)]
			choisis <- suppressWarnings(as.integer(verdict$reponse %||% integer(0)))

			shiny::div(
				class = "cq-options-corrigees",
				lapply(seq_along(options), function(i) {
					qtype_qcm_single_option_corrigee(
						option = options[[i]],
						lettre = lettres[[i]],
						choisie = i %in% choisis
					)
				})
			)
		})

		reponse <- shiny::reactive({
			it <- item()
			shiny::req(it)

			valeurs <- input[[champ(it)]]

			if (is.null(valeurs) || !length(valeurs)) {
				return(NULL)
			}

			as.integer(valeurs)
		})

		list(reponse = reponse)
	})
}
