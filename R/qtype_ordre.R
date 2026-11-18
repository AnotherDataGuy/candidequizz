# Type « Ordre ».
#
# Remise en ordre chronologique ou logique. C'est le type le plus exigeant pour
# le générateur, car il lui faut trois à six éléments comparables entre eux :
# la consigne l'invite donc explicitement à combiner plusieurs graines du même
# sous-thème.

#' Schéma d'un item de type ordre
#'
#' @param n_options Nombre d'éléments attendu, entre 3 et 6.
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
qtype_ordre_schema <- function(n_options = 4L) {
	ellmer::type_object(
		numero = ellmer::type_integer(
			"Le numero de la consigne a laquelle cet item repond."
		),
		enonce = ellmer::type_string(
			paste(
				"La consigne de classement, en francais, precisant le critere :",
				"par exemple du plus ancien au plus recent."
			)
		),
		elements = ellmer::type_array(
			ellmer::type_object(
				texte = ellmer::type_string(paste(
					"Le libelle de l'element a classer, sans date ni annee.",
					"Interdit : une annee entre parentheses, apres un tiret ou",
					"en fin de libelle."
				)),
				rang_correct = ellmer::type_integer(
					"Sa position correcte, de 1 a n, sans doublon ni trou."
				)
			),
			glue::glue("Exactement {n_options} elements comparables entre eux.")
		),
		explication = ellmer::type_string(
			"Explication pedagogique rappelant l'ordre correct et sa logique."
		)
	)
}

#' Consignes de rédaction propres au type
#'
#' @param consigne La consigne de génération d'un item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_ordre_consigne <- function(consigne) {
	glue::glue(
		"Type attendu : remise en ordre, avec exactement {consigne$n_options} ",
		"elements. Les elements doivent etre comparables sur un meme critere ",
		"explicite, chronologique de preference. Tu peux et tu dois combiner ",
		"plusieurs faits du sous-theme pour reunir assez d'elements. ",
		"Les rangs vont de 1 a {consigne$n_options}, sans doublon. ",
		"Pour un classement chronologique, chaque element nomme l'evenement ",
		"sans sa date : pas de parentheses, pas de tiret suivi d'une annee. ",
		"La date ne figure que dans l'explication, apres coup."
	)
}

#' Valider un item de type ordre
#'
#' @param item L'item produit.
#' @param consigne La consigne correspondante.
#'
#' @return `list(ok, motifs)`.
#' @noRd
qtype_ordre_valider <- function(item, consigne) {
	motifs <- character(0)
	elements <- item$payload$elements

	if (!is.list(elements) || !length(elements)) {
		return(vld_verdict("Aucun \u00e9l\u00e9ment \u00e0 classer."))
	}

	n <- length(elements)

	if (n < 3L || n > 6L) {
		motifs <- c(motifs, glue::glue("{n} \u00e9l\u00e9ments fournis, attendu entre 3 et 6."))
	}

	textes <- vapply(
		elements,
		function(e) if (vld_texte_ok(e$texte)) e$texte else NA_character_,
		character(1)
	)

	if (anyNA(textes)) {
		motifs <- c(motifs, "Au moins un \u00e9l\u00e9ment a un libell\u00e9 vide.")
		return(vld_verdict(motifs))
	}

	if (any(duplicated(vld_normaliser(textes)))) {
		motifs <- c(motifs, "Deux \u00e9l\u00e9ments \u00e0 classer sont identiques.")
	}

	dates <- textes[vapply(textes, vld_fuite_date, logical(1))]

	if (length(dates)) {
		motifs <- c(
			motifs,
			glue::glue(
				"Date visible dans un \u00e9l\u00e9ment \u00e0 classer : ",
				"{paste(sprintf('\u00ab %s \u00bb', dates), collapse = ', ')}."
			)
		)
	}

	rangs <- suppressWarnings(vapply(
		elements,
		function(e) as.integer(e$rang_correct %||% NA_integer_),
		integer(1)
	))

	if (anyNA(rangs)) {
		motifs <- c(motifs, "Au moins un rang est absent ou non entier.")
	} else if (!setequal(rangs, seq_len(n))) {
		motifs <- c(
			motifs,
			glue::glue(
				"Les rangs ne forment pas la suite 1 \u00e0 {n} : ",
				"{paste(sort(rangs), collapse = ', ')}."
			)
		)
	}

	vld_verdict(motifs)
}

#' Noter une réponse à une question d'ordre
#'
#' Le score est la proportion de paires d'éléments placées dans le bon ordre
#' relatif. Un classement presque correct est ainsi mieux noté qu'un classement
#' aléatoire, ce qu'un simple tout ou rien ne permettrait pas de distinguer.
#'
#' @param item L'item posé.
#' @param reponse Vecteur d'indices des éléments, dans l'ordre où l'utilisateur
#'   les a placés.
#'
#' @return `list(score, is_correct, detail)`.
#' @noRd
qtype_ordre_noter <- function(item, reponse) {
	elements <- item$payload$elements
	n <- length(elements)
	place <- suppressWarnings(as.integer(reponse))

	vide <- list(score = 0, is_correct = FALSE, detail = list(diagnostic_tags = character(0)))

	if (length(place) != n || anyNA(place) || !setequal(place, seq_len(n))) {
		return(vide)
	}

	rangs <- vapply(elements, function(e) as.integer(e$rang_correct), integer(1))
	# Rang correct de l'élément que l'utilisateur a mis à chaque position.
	suite <- rangs[place]

	if (n < 2L) {
		return(vide)
	}

	paires <- utils::combn(n, 2L)
	concordantes <- sum(suite[paires[1L, ]] < suite[paires[2L, ]])
	score <- concordantes / ncol(paires)

	list(
		score = score,
		is_correct = identical(suite, sort(suite)),
		detail = list(
			diagnostic_tags = character(0),
			ordre_donne = vapply(elements[place], function(e) as.character(e$texte), character(1)),
			paires_concordantes = concordantes,
			paires_totales = ncol(paires)
		)
	)
}

#' Texte de l'ordre correct
#'
#' @param item L'item.
#'
#' @return Une chaîne de caractères.
#' @noRd
qtype_ordre_solution <- function(item) {
	elements <- item$payload$elements
	rangs <- vapply(elements, function(e) as.integer(e$rang_correct), integer(1))
	textes <- vapply(elements, function(e) as.character(e$texte), character(1))

	paste(seq_along(textes), textes[order(rangs)], sep = ". ", collapse = " \u2192 ")
}

#' Ordre initial présenté à l'utilisateur
#'
#' Dérivé de l'`item_id` plutôt que tiré au hasard : une séance reprise montre
#' les éléments dans le même désordre. Une rotation d'un cran suffit à éviter
#' de servir l'ordre déjà correct.
#'
#' @param item L'item.
#'
#' @return Une permutation de `seq_along(item$payload$elements)`.
#' @noRd
qtype_ordre_depart <- function(item) {
	n <- length(item$payload$elements)

	if (n < 2L) {
		return(seq_len(n))
	}

	decalage <- 1L + (sum(utf8ToInt(item$item_id)) %% (n - 1L))

	c(seq.int(decalage + 1L, n), seq_len(decalage))
}

#' Permuter deux positions d'un ordre
#'
#' @param ordre Vecteur d'indices.
#' @param depuis Position à déplacer, 1-based.
#' @param vers Position d'arrivée.
#'
#' @return L'ordre modifié.
#' @noRd
qtype_ordre_deplacer <- function(ordre, depuis, vers) {
	n <- length(ordre)
	depuis <- as.integer(depuis)
	vers <- as.integer(vers)

	if (depuis < 1L || depuis > n || vers < 1L || vers > n || depuis == vers) {
		return(ordre)
	}

	valeur <- ordre[[depuis]]
	reste <- ordre[-depuis]
	append(reste, valeur, after = vers - 1L)
}

#' Interface de saisie d'une remise en ordre
#'
#' Boutons monter / descendre plutôt qu'un glisser-déposer : la cible est le
#' téléphone tenu d'une main, et un doigt n'y fait pas un glissement fiable.
#'
#' @param id Identifiant du module.
#'
#' @return Un emplacement rempli par le serveur du type.
#' @noRd
qtype_ordre_ui <- function(id) {
	shiny::div(
		class = "cq-reponse cq-reponse--ordre",
		shiny::uiOutput(shiny::NS(id, "zone"))
	)
}

#' Une ligne de classement
#'
#' @param texte Libellé de l'élément.
#' @param rang Position affichée.
#' @param ns Fonction de préfixe d'identifiants.
#' @param item_id Identifiant de l'item, pour des boutons uniques.
#' @param n Nombre total d'éléments.
#' @param corrigee Affichage figé après validation.
#' @param etat `"juste"`, `"faux"` ou `NULL`.
#'
#' @return Un élément d'interface.
#' @noRd
qtype_ordre_ligne <- function(texte,
														 rang,
														 ns,
														 n,
														 corrigee = FALSE,
														 etat = NULL) {
	bouton <- function(direction, libelle, icone, inactif) {
		shiny::tags$button(
			type = "button",
			class = "btn btn-outline-secondary btn-sm cq-ordre-btn",
			disabled = if (inactif) NA else NULL,
			"aria-label" = libelle,
			onclick = if (inactif) {
				NULL
			} else {
				sprintf(
					"Shiny.setInputValue('%s', {rang: %d, dir: '%s', nonce: Math.random()}, {priority: 'event'})",
					ns("mouvement"),
					rang,
					direction
				)
			},
			bsicons::bs_icon(icone)
		)
	}

	shiny::div(
		class = paste(
			"cq-ordre-ligne",
			if (!is.null(etat)) paste0("cq-ordre-ligne--", etat) else ""
		),
		shiny::span(class = "cq-option-lettre", as.character(rang)),
		shiny::span(class = "cq-ordre-texte", texte),
		if (!corrigee) {
			shiny::div(
				class = "cq-ordre-commandes",
				bouton("haut", "Monter", "chevron-up", rang == 1L),
				bouton("bas", "Descendre", "chevron-down", rang == n)
			)
		}
	)
}

#' Serveur du type ordre
#'
#' @param id Identifiant du module.
#' @param item Réactif renvoyant l'item à poser.
#' @param notation Réactif renvoyant la notation, ou `NULL` tant que la réponse
#'   n'est pas validée.
#'
#' @return `list(reponse)`.
#' @noRd
qtype_ordre_server <- function(id, item, notation) {
	shiny::moduleServer(id, function(input, output, session) {
		ordre <- shiny::reactiveVal(integer(0))

		shiny::observeEvent(item()$item_id, {
			it <- item()
			shiny::req(it)

			ordre(qtype_ordre_depart(it))
		}, ignoreNULL = TRUE)

		# Un seul observateur pour tous les boutons : les créer dans une
		# boucle à chaque rendu accumulerait des écouteurs au fil des questions.
		shiny::observeEvent(input$mouvement, {
			if (!is.null(notation())) {
				return(invisible(NULL))
			}

			mouvement <- input$mouvement
			rang <- suppressWarnings(as.integer(mouvement$rang %||% NA_integer_))
			vers <- if (identical(mouvement$dir, "haut")) rang - 1L else rang + 1L

			ordre(qtype_ordre_deplacer(ordre(), rang, vers))
		}, ignoreNULL = TRUE)

		output$zone <- shiny::renderUI({
			it <- item()
			shiny::req(it)

			elements <- it$payload$elements
			n <- length(elements)
			verdict <- notation()
			place <- if (is.null(verdict)) ordre() else as.integer(verdict$reponse %||% ordre())
			shiny::req(length(place) == n)

			rangs <- vapply(elements, function(e) as.integer(e$rang_correct), integer(1))

			shiny::tagList(
				if (is.null(verdict)) {
					shiny::p(
						class = "cq-aide",
						"D\u00e9placez les \u00e9l\u00e9ments avec les fl\u00e8ches, du premier au dernier."
					)
				},
				shiny::div(
					class = "cq-ordre",
					lapply(seq_len(n), function(rang) {
						idx <- place[[rang]]
						etat <- if (is.null(verdict)) {
							NULL
						} else if (identical(rangs[[idx]], rang)) {
							"juste"
						} else {
							"faux"
						}

						qtype_ordre_ligne(
							texte = elements[[idx]]$texte,
							rang = rang,
							ns = session$ns,
							n = n,
							corrigee = !is.null(verdict),
							etat = etat
						)
					})
				),
				if (!is.null(verdict)) {
					shiny::p(
						class = "cq-ordre-solution",
						shiny::strong("Ordre correct : "),
						qtype_ordre_solution(it)
					)
				}
			)
		})

		list(reponse = shiny::reactive({
			it <- item()
			shiny::req(it)

			place <- ordre()

			if (length(place) != length(it$payload$elements)) {
				return(NULL)
			}

			place
		}))
	})
}
