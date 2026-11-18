# Type « QCM à option unique ».
#
# Schéma de sortie LLM, consignes de rédaction, validation et notation. Aucune
# autre partie de l'application n'a besoin de connaître ce fichier : tout passe
# par le registre des types.

#' Schéma d'un item de type QCM à option unique
#'
#' @param n_options Nombre d'options attendu.
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
qtype_qcm_single_schema <- function(n_options = 4L) {
	ellmer::type_object(
		numero = ellmer::type_integer(
			"Le numero de la consigne a laquelle cet item repond."
		),
		enonce = ellmer::type_string(
			"La question posee, en francais, sous forme interrogative."
		),
		options = ellmer::type_array(
			ellmer::type_object(
				texte = ellmer::type_string("Le libelle de l'option."),
				is_correct = ellmer::type_boolean("Cette option est-elle la bonne reponse."),
				diagnostic_tag = ellmer::type_string(
					paste(
						"Pour une option fausse : l'identifiant court de la confusion",
						"qu'elle revele, en minuscules avec des underscores.",
						"Pour l'option correcte : chaine vide."
					)
				)
			),
			glue::glue("Exactement {n_options} options, dont une seule correcte.")
		),
		explication = ellmer::type_string(
			"Explication pedagogique de la bonne reponse, une a deux phrases."
		)
	)
}

#' Consignes de rédaction propres au type
#'
#' @param consigne La consigne de génération d'un item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_qcm_single_consigne <- function(consigne) {
	glue::glue(
		"Type attendu : QCM a option unique, avec exactement ",
		"{consigne$n_options} options dont une seule correcte. ",
		"L'enonce ne doit pas contenir la bonne reponse, ni sa date, ",
		"ni un nom qui la rend evidente."
	)
}

#' Valider un item de type QCM à option unique
#'
#' @param item L'item produit.
#' @param consigne La consigne correspondante.
#'
#' @return `list(ok, motifs)`.
#' @noRd
qtype_qcm_single_valider <- function(item, consigne) {
	motifs <- vld_options_communes(item$payload$options)

	if (length(motifs)) {
		return(vld_verdict(motifs))
	}

	options <- item$payload$options
	corrects <- vld_corrects(options)
	textes <- vld_textes(options)

	# Un autre type peut se replier sur le QCM de référence à quatre choix.
	attendu <- if (identical(item$source, "graine_repli") &&
		!identical(consigne$type, "qcm_single")) 4L else as.integer(consigne$n_options)

	if (length(options) != attendu) {
		motifs <- c(
			motifs,
			glue::glue("{length(options)} options fournies au lieu de {attendu}.")
		)
	}

	if (sum(corrects) != 1L) {
		motifs <- c(
			motifs,
			glue::glue("{sum(corrects)} options correctes au lieu d'une seule.")
		)
	}

	if (sum(corrects) == 1L && vld_fuite_reponse(item$enonce, textes[corrects])) {
		motifs <- c(motifs, "L'enonc\u00e9 contient d\u00e9j\u00e0 la bonne r\u00e9ponse.")
	}

	vld_verdict(motifs)
}

#' Interface de saisie d'un QCM à option unique
#'
#' @param id Identifiant du module.
#'
#' @return Un emplacement rempli par le serveur du type.
#' @noRd
qtype_qcm_single_ui <- function(id) {
	shiny::div(
		class = "cq-reponse",
		shiny::uiOutput(shiny::NS(id, "zone"))
	)
}

#' Une option en état de saisie
#'
#' @param item L'item posé.
#'
#' @return Les arguments de libellés et de valeurs pour la saisie.
#' @noRd
qtype_qcm_single_choix <- function(item) {
	options <- item$payload$options
	lettres <- LETTERS[seq_along(options)]

	list(
		noms = lapply(seq_along(options), function(i) {
			shiny::tagList(
				shiny::span(class = "cq-option-lettre", lettres[[i]]),
				shiny::span(class = "cq-option-texte", options[[i]]$texte)
			)
		}),
		valeurs = as.character(seq_along(options))
	)
}

#' Une option en état corrigé
#'
#' Trois états à distinguer visuellement : la bonne réponse, l'option fautive
#' que l'utilisateur a choisie, et les autres. La couleur ne suffit pas à
#' porter l'information, chaque état est donc aussi marqué par une icône et un
#' texte.
#'
#' @param option L'option.
#' @param lettre Sa lettre.
#' @param choisie L'utilisateur a-t-il retenu cette option.
#'
#' @return Un élément d'interface.
#' @noRd
qtype_qcm_single_option_corrigee <- function(option, lettre, choisie) {
	correcte <- isTRUE(option$is_correct)

	etat <- if (correcte) {
		"juste"
	} else if (choisie) {
		"faux"
	} else {
		"neutre"
	}

	mention <- if (correcte && choisie) {
		"Votre r\u00e9ponse, correcte"
	} else if (correcte) {
		"Bonne r\u00e9ponse"
	} else if (choisie) {
		"Votre r\u00e9ponse, incorrecte"
	} else {
		NULL
	}

	icone <- if (correcte) {
		bsicons::bs_icon("check-circle-fill")
	} else if (choisie) {
		bsicons::bs_icon("x-circle-fill")
	} else {
		NULL
	}

	shiny::div(
		class = paste0("cq-option cq-option--", etat),
		shiny::span(class = "cq-option-lettre", lettre),
		shiny::div(
			class = "cq-option-corps",
			shiny::span(class = "cq-option-texte", option$texte),
			if (!is.null(mention)) shiny::span(class = "cq-option-mention", mention)
		),
		if (!is.null(icone)) shiny::span(class = "cq-option-icone", icone)
	)
}

#' Serveur du type QCM à option unique
#'
#' @param id Identifiant du module.
#' @param item Réactif renvoyant l'item à poser.
#' @param notation Réactif renvoyant la notation, ou `NULL` tant que la réponse
#'   n'est pas validée. C'est lui qui fait basculer l'affichage de l'état de
#'   saisie à l'état corrigé. Il porte aussi la réponse donnée, de sorte que la
#'   correction ne dépende pas d'une valeur d'`input` dont l'élément vient
#'   d'être retiré de la page.
#'
#' @return `list(reponse)`, où `reponse` est un réactif renvoyant la réponse
#'   dans la forme attendue par la fonction de notation du type.
#' @noRd
qtype_qcm_single_server <- function(id, item, notation) {
	shiny::moduleServer(id, function(input, output, session) {
		# L'identifiant de saisie porte celui de l'item. Sans cela, la valeur
		# choisie à la question précédente resterait dans `input` et serait
		# reprise comme réponse à la question suivante.
		champ <- function(it) paste0("choix_", it$item_id)

		output$zone <- shiny::renderUI({
			it <- item()
			shiny::req(it)

			verdict <- notation()

			if (is.null(verdict)) {
				choix <- qtype_qcm_single_choix(it)

				return(shiny::radioButtons(
					inputId = session$ns(champ(it)),
					label = "Choisissez une r\u00e9ponse",
					choiceNames = choix$noms,
					choiceValues = choix$valeurs,
					selected = character(0),
					width = "100%"
				))
			}

			options <- it$payload$options
			lettres <- LETTERS[seq_along(options)]
			choisi <- suppressWarnings(as.integer(verdict$reponse %||% NA_integer_))

			shiny::div(
				class = "cq-options-corrigees",
				lapply(seq_along(options), function(i) {
					qtype_qcm_single_option_corrigee(
						option = options[[i]],
						lettre = lettres[[i]],
						choisie = !is.na(choisi) && choisi == i
					)
				})
			)
		})

		reponse <- shiny::reactive({
			it <- item()
			shiny::req(it)

			input[[champ(it)]]
		})

		list(reponse = reponse)
	})
}

#' Noter une réponse à un QCM à option unique
#'
#' @param item L'item posé.
#' @param reponse L'indice de l'option choisie par l'utilisateur.
#'
#' @return `list(score, is_correct, detail)`. `detail` porte les
#'   `diagnostic_tags` touchés, base des statistiques de confusion.
#' @noRd
qtype_qcm_single_noter <- function(item, reponse) {
	options <- item$payload$options
	choix <- suppressWarnings(as.integer(reponse))

	if (length(choix) != 1L || is.na(choix) || choix < 1L || choix > length(options)) {
		return(list(score = 0, is_correct = FALSE, detail = list(diagnostic_tags = character(0))))
	}

	choisie <- options[[choix]]
	juste <- isTRUE(choisie$is_correct)
	tag <- as.character(choisie$diagnostic_tag %||% "")

	list(
		score = if (juste) 1 else 0,
		is_correct = juste,
		detail = list(
			diagnostic_tags = if (!juste && nzchar(tag)) tag else character(0),
			option_choisie = as.character(choisie$texte)
		)
	)
}

#' Texte de la bonne réponse d'un QCM à option unique
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_qcm_single_solution <- function(item) {
	textes <- vld_textes(item$payload$options)
	corrects <- vld_corrects(item$payload$options)

	paste(textes[corrects], collapse = " ; ")
}
