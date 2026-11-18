# Type « Vrai ou faux ».
#
# Particularité : l'énoncé n'est pas une question mais une affirmation à juger.
# Le piège à éviter est le déséquilibre, un générateur laissé libre produisant
# très majoritairement des affirmations vraies. La consigne impose donc la
# valeur de vérité attendue, tirée en amont par le sélecteur de graines.

#' Schéma d'un item de type vrai ou faux
#'
#' @param n_options Ignoré, la notion ne s'applique pas à ce type.
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
qtype_vrai_faux_schema <- function(n_options = NULL) {
	ellmer::type_object(
		numero = ellmer::type_integer(
			"Le numero de la consigne a laquelle cet item repond."
		),
		enonce = ellmer::type_string(
			paste(
				"L'affirmation a juger, en francais, sous forme affirmative et non",
				"interrogative. Une seule idee par affirmation."
			)
		),
		reponse = ellmer::type_boolean(
			"Vrai si l'affirmation est exacte, faux sinon."
		),
		diagnostic_tag = ellmer::type_string(
			paste(
				"Identifiant court de la confusion que revele une erreur de",
				"jugement sur cette affirmation."
			)
		),
		explication = ellmer::type_string(
			"Explication pedagogique indiquant pourquoi l'affirmation est vraie ou fausse."
		)
	)
}

#' Consignes de rédaction propres au type
#'
#' @param consigne La consigne de génération d'un item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_vrai_faux_consigne <- function(consigne) {
	attendue <- if (isTRUE(consigne$verite_attendue)) "VRAIE" else "FAUSSE"

	glue::glue(
		"Type attendu : vrai ou faux. L'affirmation doit etre {attendue}. ",
		"Ecris une affirmation, pas une question. Si elle doit etre fausse, ",
		"l'erreur doit etre precise et plausible, pas grossiere."
	)
}

#' Valider un item de type vrai ou faux
#'
#' @param item L'item produit.
#' @param consigne La consigne correspondante.
#'
#' @return `list(ok, motifs)`.
#' @noRd
qtype_vrai_faux_valider <- function(item, consigne) {
	motifs <- character(0)
	reponse <- item$payload$reponse

	if (!is.logical(reponse) || length(reponse) != 1L || is.na(reponse)) {
		motifs <- c(motifs, "La valeur de v\u00e9rit\u00e9 est absente ou non bool\u00e9enne.")
		return(vld_verdict(motifs))
	}

	if (grepl("\\?\\s*$", trimws(item$enonce))) {
		motifs <- c(motifs, "L'\u00e9nonc\u00e9 est une question alors qu'une affirmation est attendue.")
	}

	# On ne contrôle l'équilibre vrai/faux que si la consigne l'a imposé.
	if (!is.null(consigne$verite_attendue) &&
			!identical(as.logical(consigne$verite_attendue), reponse)) {
		motifs <- c(
			motifs,
			glue::glue(
				"Valeur de v\u00e9rit\u00e9 {reponse} alors que la consigne demandait ",
				"{as.logical(consigne$verite_attendue)}."
			)
		)
	}

	vld_verdict(motifs)
}

#' Noter une réponse à un vrai ou faux
#'
#' @param item L'item posé.
#' @param reponse Le jugement de l'utilisateur, booléen.
#'
#' @return `list(score, is_correct, detail)`.
#' @noRd
qtype_vrai_faux_noter <- function(item, reponse) {
	attendue <- isTRUE(item$payload$reponse)
	donnee <- if (length(reponse) == 1L && !is.na(reponse)) as.logical(reponse) else NA

	juste <- !is.na(donnee) && identical(donnee, attendue)
	tag <- as.character(item$payload$diagnostic_tag %||% "")

	list(
		score = if (juste) 1 else 0,
		is_correct = juste,
		detail = list(
			diagnostic_tags = if (!juste && nzchar(tag)) tag else character(0),
			reponse_donnee = donnee
		)
	)
}

#' Texte de la bonne réponse d'un vrai ou faux
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_vrai_faux_solution <- function(item) {
	if (isTRUE(item$payload$reponse)) "Vrai" else "Faux"
}

#' Interface de saisie d'un vrai ou faux
#'
#' @param id Identifiant du module.
#'
#' @return Un emplacement rempli par le serveur du type.
#' @noRd
qtype_vrai_faux_ui <- function(id) {
	shiny::div(
		class = "cq-reponse cq-reponse--vrai-faux",
		shiny::uiOutput(shiny::NS(id, "zone"))
	)
}

#' Une affirmation corrigée
#'
#' @param valeur `TRUE` pour l'option « Vrai ».
#' @param attendue La valeur de vérité de l'affirmation.
#' @param choisie Ce que l'utilisateur a répondu, ou `NA`.
#'
#' @return Un élément d'interface.
#' @noRd
qtype_vrai_faux_option_corrigee <- function(valeur, attendue, choisie) {
	libelle <- if (valeur) "Vrai" else "Faux"
	correcte <- identical(valeur, attendue)
	retenue <- !is.na(choisie) && identical(choisie, valeur)

	qtype_qcm_single_option_corrigee(
		option = list(texte = libelle, is_correct = correcte),
		lettre = if (valeur) "V" else "F",
		choisie = retenue
	)
}

#' Serveur du type vrai ou faux
#'
#' @param id Identifiant du module.
#' @param item Réactif renvoyant l'item à poser.
#' @param notation Réactif renvoyant la notation, ou `NULL` tant que la réponse
#'   n'est pas validée.
#'
#' @return `list(reponse)`.
#' @noRd
qtype_vrai_faux_server <- function(id, item, notation) {
	shiny::moduleServer(id, function(input, output, session) {
		champ <- function(it) paste0("jugement_", it$item_id)

		output$zone <- shiny::renderUI({
			it <- item()
			shiny::req(it)

			verdict <- notation()

			if (is.null(verdict)) {
				return(shiny::radioButtons(
					inputId = session$ns(champ(it)),
					label = NULL,
					choiceNames = list(
						shiny::tagList(
							shiny::span(class = "cq-option-lettre", "V"),
							shiny::span(class = "cq-option-texte", "Vrai")
						),
						shiny::tagList(
							shiny::span(class = "cq-option-lettre", "F"),
							shiny::span(class = "cq-option-texte", "Faux")
						)
					),
					choiceValues = c("TRUE", "FALSE"),
					selected = character(0),
					width = "100%"
				))
			}

			choisie <- if (length(verdict$reponse) == 1L && !is.na(verdict$reponse)) {
				as.logical(verdict$reponse)
			} else {
				NA
			}

			shiny::div(
				class = "cq-options-corrigees",
				qtype_vrai_faux_option_corrigee(TRUE, isTRUE(it$payload$reponse), choisie),
				qtype_vrai_faux_option_corrigee(FALSE, isTRUE(it$payload$reponse), choisie)
			)
		})

		reponse <- shiny::reactive({
			it <- item()
			shiny::req(it)

			valeur <- input[[champ(it)]]

			if (is.null(valeur) || !nzchar(valeur)) {
				return(NULL)
			}

			as.logical(valeur)
		})

		list(reponse = reponse)
	})
}
