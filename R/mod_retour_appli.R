# « Un problème avec l'appli ? Une suggestion ? »
#
# Bouton de l'en-tête (version en ligne seulement, voir app_server.R), qui
# ouvre une fenêtre plein écran en forme de guichet : à gauche le ticket, à
# droite trois étapes (genre, partie de l'appli, détail). Enregistrement :
# fct_retours_appli.R. Lecture : section Administration, onglet « Retours
# appli ».

#' Bouton de l'en-tête
#'
#' Avant le clic, le navigateur envoie la taille d'écran, le navigateur et le
#' thème (`ns("contexte")`) : affichés dans la fenêtre avant l'envoi.
#'
#' @param id Identifiant du module.
#' @noRd
mod_retour_appli_bouton <- function(id) {
	ns <- shiny::NS(id)
	shiny::tags$button(
		id = ns("ouvrir"),
		type = "button",
		class = "iconbtn retour-toggle action-button",
		title = "Un probl\u00e8me ? Une suggestion ?",
		`aria-label` = "Signaler un probl\u00e8me ou proposer une id\u00e9e",
		onclick = sprintf(
			paste0(
				"Shiny.setInputValue('%s', {ecran: window.innerWidth + 'x' + window.innerHeight,",
				" navigateur: navigator.userAgent,",
				" theme: document.documentElement.getAttribute('data-theme') || ''}, {priority: 'event'});"
			),
			ns("contexte")
		),
		bsicons::bs_icon("chat-square-heart")
	)
}

#' Serveur du bouton et de la fenêtre
#'
#' @param id Identifiant du module.
#' @param utilisateur Réactif : compte connecté.
#' @param section Réactif : section affichée (partie proposée d'office).
#' @noRd
mod_retour_appli_server <- function(id, utilisateur, section = shiny::reactive(NULL)) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns
		erreur <- shiny::reactiveVal(NULL)
		section_ouverture <- shiny::reactiveVal(NULL)

		shiny::observeEvent(input$ouvrir, {
			u <- utilisateur()
			shiny::req(u)
			erreur(NULL)
			section_ouverture(section())
			shiny::showModal(retour_appli_fenetre(
				ns,
				alias = u$username,
				partie = retour_appli_partie_depuis(section())
			))
		})

		output$contexte <- shiny::renderUI({
			ctx <- input$contexte
			theme <- if (identical(ctx$theme, "dark")) "th\u00e8me sombre" else "th\u00e8me clair"
			parties <- c(
				if (!is.null(ctx$ecran)) paste("\u00e9cran", sub("x", "\u00d7", ctx$ecran)),
				retour_appli_navigateur(ctx$navigateur),
				theme
			)
			shiny::tags$span(paste(parties, collapse = " \u00b7 "))
		})

		output$erreur <- shiny::renderUI({
			m <- erreur()
			if (is.null(m)) {
				return(NULL)
			}
			shiny::tags$p(class = "cq-fb-erreur", role = "alert", bsicons::bs_icon("exclamation-circle"), " ", m)
		})

		shiny::observeEvent(input$deposer, {
			u <- utilisateur()
			shiny::req(u)
			tryCatch({
				retour <- retour_appli_valider(input$genre, input$partie, input$partie_autre, input$texte)
				ctx <- input$contexte
				if (is.list(ctx)) {
					ctx$navigateur <- retour_appli_navigateur(ctx$navigateur)
				}
				numero <- retour_appli_enregistrer(
					u$user_id, retour,
					retour_appli_contexte(ctx, section = section_ouverture())
				)
				erreur(NULL)
				shiny::showModal(retour_appli_merci(u$username, numero, retour$genre))
			}, error = function(e) {
				erreur(cq_message_utilisateur(e))
			})
		})

		invisible(NULL)
	})
}

#' Fenêtre plein écran du formulaire
#'
#' @param ns Espace de noms du module.
#' @param alias Alias du compte (imprimé sur le ticket).
#' @param partie Partie cochée d'office, ou `NULL`.
#' @noRd
retour_appli_fenetre <- function(ns, alias, partie = NULL) {
	icones_parties <- c(
		accueil = "house", composer = "sliders", questions = "ui-radios",
		corrections = "check2-square", avis = "star-half", bilan = "clipboard-data",
		progression = "graph-up-arrow", ressources = "book", ia = "stars",
		compte = "person-lock", autre = "three-dots"
	)
	parties <- retour_appli_parties()

	carte_genre <- function(code, icone, titre, sous) {
		shiny::tags$span(
			class = paste0("cq-fb-genre-carte is-", code),
			shiny::tags$span(class = "cq-fb-genre-icone", bsicons::bs_icon(icone)),
			shiny::tags$span(class = "cq-fb-genre-titre", titre),
			shiny::tags$span(class = "cq-fb-genre-sous", sous)
		)
	}

	texte <- htmltools::tagQuery(shiny::textAreaInput(
		ns("texte"), label = NULL, width = "100%", rows = 6,
		placeholder = paste(
			"Ce que vous faisiez, ce qui s'est pass\u00e9, ce que vous attendiez\u2026",
			"ou l'id\u00e9e qui vous trotte dans la t\u00eate."
		)
	))
	texte <- texte$find("textarea")$addAttrs(maxlength = retour_appli_max())$allTags()

	shiny::modalDialog(
		title = NULL,
		footer = NULL,
		size = "xl",
		easyClose = TRUE,
		class = "cq-fb",
		shiny::div(
			class = "cq-fb-page",
			shiny::tags$aside(
				class = "cq-fb-cote",
				shiny::tags$p(class = "cq-fb-sur-titre", "Guichet des remarques"),
				shiny::tags$h2(
					class = "cq-fb-titre",
					"Un probl\u00e8me avec l'appli ?",
					shiny::tags$br(),
					shiny::tags$span(class = "serif", "Une suggestion ?")
				),
				shiny::tags$p(
					class = "cq-fb-chapo",
					"Chaque mot est lu par la personne qui fabrique Candide. ",
					"Les probl\u00e8mes sont corrig\u00e9s, les id\u00e9es discut\u00e9es."
				),
				retour_appli_ticket(alias)
			),
			shiny::tags$section(
				class = "cq-fb-form",
				shiny::tags$button(
					type = "button", class = "cq-fb-fermer", `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
					`aria-label` = "Fermer", bsicons::bs_icon("x-lg")
				),
				shiny::div(
					class = "cq-fb-etape",
					shiny::tags$h3(shiny::tags$span(class = "cq-fb-num", "1"), "C'est plut\u00f4t\u2026"),
					shiny::radioButtons(
						ns("genre"), label = NULL, inline = TRUE, width = "100%",
						selected = character(0),
						choiceNames = list(
							carte_genre("probleme", "bug", "Quelque chose cloche", "un bug, un affichage, une erreur"),
							carte_genre("suggestion", "lightbulb", "J'ai une id\u00e9e", "une am\u00e9lioration, un manque")
						),
						choiceValues = c("probleme", "suggestion")
					)
				),
				shiny::div(
					class = "cq-fb-etape",
					shiny::tags$h3(shiny::tags$span(class = "cq-fb-num", "2"), "O\u00f9, dans l'appli ?"),
					shiny::radioButtons(
						ns("partie"), label = NULL, inline = TRUE, width = "100%",
						selected = partie %||% character(0),
						choiceNames = lapply(names(parties), function(code) {
							shiny::tags$span(
								class = "cq-fb-partie",
								bsicons::bs_icon(icones_parties[[code]]),
								shiny::tags$span(parties[[code]])
							)
						}),
						choiceValues = names(parties)
					),
					shiny::conditionalPanel(
						condition = sprintf("input['%s'] == 'autre'", ns("partie")),
						shiny::textInput(
							ns("partie_autre"), "O\u00f9 exactement ?",
							placeholder = "le pied de page, la fen\u00eatre d'un th\u00e8me\u2026",
							width = "100%"
						)
					)
				),
				shiny::div(
					class = "cq-fb-etape",
					shiny::tags$h3(shiny::tags$span(class = "cq-fb-num", "3"), "Racontez-nous"),
					texte,
					shiny::div(
						class = "cq-fb-sous-texte",
						shiny::tags$span(
							class = "cq-fb-contexte",
							bsicons::bs_icon("paperclip"),
							" Joint pour nous aider : ",
							shiny::uiOutput(ns("contexte"), inline = TRUE)
						),
						shiny::tags$span(
							class = "cq-fb-compteur",
							`data-max` = retour_appli_max(),
							`data-pour` = ns("texte"),
							paste0("0 / ", retour_appli_max())
						)
					)
				),
				shiny::uiOutput(ns("erreur")),
				shiny::div(
					class = "cq-fb-actions",
					shiny::tags$button(
						type = "button", class = "btn", `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
						"Plus tard"
					),
					shiny::actionButton(
						ns("deposer"),
						shiny::tagList("D\u00e9poser au guichet ", bsicons::bs_icon("send")),
						class = "btn primary"
					)
				)
			)
		)
	)
}

#' Ticket du guichet (côté gauche de la fenêtre)
#'
#' @param alias Alias imprimé.
#' @param numero Numéro du retour une fois enregistré (sinon « — »).
#' @param tampon Afficher le tampon « Reçu ».
#' @noRd
retour_appli_ticket <- function(alias, numero = NULL, tampon = FALSE) {
	shiny::div(
		class = paste("cq-fb-ticket", if (tampon) "is-recu"),
		`aria-hidden` = if (!tampon) "true",
		shiny::div(
			class = "cq-fb-ticket-haut",
			shiny::tags$span("TICKET"),
			shiny::tags$span("GUICHET 1")
		),
		shiny::tags$span(class = "cq-fb-ticket-label", "Votre num\u00e9ro"),
		shiny::tags$span(
			class = "cq-fb-ticket-numero",
			if (is.null(numero)) "\u2014\u2014" else sprintf("%03d", as.integer(numero))
		),
		shiny::div(
			class = "cq-fb-ticket-bas",
			shiny::tags$span(alias),
			shiny::tags$span(format(Sys.time(), "%d/%m/%Y \u00b7 %H:%M", tz = "Europe/Paris"))
		),
		if (tampon) {
			shiny::tags$span(
				class = "cq-tampon cq-tampon--juste cq-fb-tampon",
				shiny::tags$span(
					class = "cq-tampon-cadre",
					shiny::tags$span(class = "cq-tampon-haut", "Candide"),
					shiny::tags$span(class = "cq-fb-tampon-mot", "Re\u00e7u"),
					shiny::tags$span(class = "cq-tampon-haut", format(Sys.Date(), "%d.%m.%y"))
				)
			)
		}
	)
}

#' Fenêtre de remerciement (le ticket reçoit son tampon)
#' @noRd
retour_appli_merci <- function(alias, numero, genre) {
	shiny::modalDialog(
		title = NULL,
		footer = NULL,
		size = "xl",
		easyClose = TRUE,
		class = "cq-fb",
		shiny::div(
			class = "cq-fb-page cq-fb-page--merci",
			shiny::tags$aside(
				class = "cq-fb-cote",
				shiny::tags$p(class = "cq-fb-sur-titre", "Guichet des remarques"),
				retour_appli_ticket(alias, numero, tampon = TRUE)
			),
			shiny::tags$section(
				class = "cq-fb-form cq-fb-merci",
				shiny::tags$h2(
					class = "cq-fb-titre",
					"Merci, ", alias, " !",
					shiny::tags$br(),
					shiny::tags$span(class = "serif", "C'est bien arriv\u00e9.")
				),
				shiny::tags$p(
					if (identical(genre, "probleme")) {
						"On regarde ce qui cloche. Si vous avez un d\u00e9tail \u00e0 ajouter, d\u00e9posez simplement un autre ticket."
					} else {
						"Votre id\u00e9e rejoint la pile des choses \u00e0 construire. Les bonnes id\u00e9es viennent souvent de l\u00e0."
					}
				),
				shiny::tags$p(class = "muted tiny", sprintf("Ticket n\u00b0 %03d.", as.integer(numero))),
				shiny::div(
					class = "cq-fb-actions",
					shiny::tags$button(
						type = "button", class = "btn primary", `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
						"Retour \u00e0 l'appli"
					)
				)
			)
		)
	)
}
