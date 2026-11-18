# Administration : retours des participants, question par question.
#
# Affiché dans le panneau Administration de l'accueil (mod_home.R), pour
# l'administrateur seulement. Sert à repérer les questions à corriger dans la
# banque : commentaires, taux d'échec, difficulté ressentie. La correction se
# fait dans le classeur Excel de la banque, grâce à l'identifiant de graine.

#' Retours — interface
#' @noRd
mod_admin_retours_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-retours",
		shiny::tags$p(class = "eyebrow", "Retours sur les questions"),
		# Aide repliée : où et comment corriger une question repérée ici.
		# Détails : dev/BANQUE_DE_QUESTIONS.md.
		shiny::tags$details(
			class = "cq-retours-aide",
			shiny::tags$summary("Corriger une question : comment faire"),
			shiny::tags$p(
				"Les questions viennent du classeur ",
				shiny::tags$code("inst/extdata/banque_donnees_questions_reponses.xlsx"),
				", publi\u00e9 sur GitHub avec le code. Il sert aux deux versions de l'application : ",
				"sans IA, ses questions sont pos\u00e9es telles quelles ; avec IA, chacune est r\u00e9\u00e9crite \u00e0 partir ",
				"de son \u00e9nonc\u00e9, de sa bonne r\u00e9ponse et de son explication."
			),
			shiny::tags$ol(
				shiny::tags$li("Notez l'identifiant de la question (par exemple REHF-RECH-001)."),
				shiny::tags$li(
					"Dans le classeur, feuille QUESTIONS_REPONSES, cherchez cet identifiant et corrigez l'\u00e9nonc\u00e9 (MAIN_QUESTION), ",
					"les r\u00e9ponses (ANSWER_OPTIONS, garder le format JSON), l'explication (EXPLANATION) ou la difficult\u00e9 (DIFFICULTY, 1 \u00e0 10). ",
					"Ne changez jamais l'identifiant."
				),
				shiny::tags$li(
					"Sur votre ordinateur, lancez ", shiny::tags$code("dev/04_publier_banque.R"),
					" : v\u00e9rification de la banque, manifest.json, commit et push."
				),
				shiny::tags$li("Une fois l'application republi\u00e9e, revenez ici et marquez la question comme trait\u00e9e.")
			)
		),
		shiny::div(
			class = "cq-retours-filtres",
			shiny::selectInput(
				ns("statut"), NULL,
				choices = c("\u00c0 traiter" = "a_traiter", "Trait\u00e9es" = "traite", "Toutes" = "tous"),
				width = "auto"
			),
			shiny::selectInput(
				ns("tri"), NULL,
				choices = c(
					"Commentaires d'abord" = "commentaires",
					"Plus souvent rat\u00e9es" = "echec",
					"Ressenties plus difficiles" = "difficulte"
				),
				width = "auto"
			),
			shiny::checkboxInput(ns("avec_avis"), "Seulement avec un avis", value = TRUE),
			shiny::actionButton(ns("actualiser"), "Actualiser", class = "btn"),
			shiny::downloadButton(ns("csv"), "CSV", class = "btn")
		),
		shiny::uiOutput(ns("liste"))
	)
}

#' Retours — serveur
#'
#' @param utilisateur Réactif du compte connecté (seul l'administrateur voit
#'   et modifie quelque chose).
#' @noRd
mod_admin_retours_server <- function(id, utilisateur) {
	shiny::moduleServer(id, function(input, output, session) {
		maj <- shiny::reactiveVal(0L)
		est_admin <- shiny::reactive(isTRUE(utilisateur()$is_admin))

		# Relu à la demande (bouton Actualiser, ou après un changement de statut) :
		# la lecture parcourt toutes les séances.
		tableau <- shiny::reactive({
			maj()
			input$actualiser
			shiny::req(est_admin())
			tryCatch(retours_tableau(), error = function(e) NULL)
		})

		output$liste <- shiny::renderUI({
			t <- tableau()

			if (is.null(t)) {
				return(shiny::tags$p(class = "muted", "Les retours n'ont pas pu \u00eatre lus. R\u00e9essayez avec Actualiser."))
			}

			t <- retours_filtrer(t, input$statut %||% "a_traiter", input$tri %||% "commentaires", isTRUE(input$avec_avis))

			if (!nrow(t)) {
				return(shiny::tags$p(class = "muted", "Rien \u00e0 afficher avec ces filtres."))
			}

			montres <- utils::head(t, 50L)
			pourcent <- function(x) if (is.na(x)) "\u2013" else paste0(round(100 * x), " %")
			un_chiffre <- function(x) if (is.na(x)) "\u2013" else format(round(x, 1), decimal.mark = ",")

			shiny::tagList(
				shiny::tags$p(
					class = "muted tiny",
					sprintf("%d question(s), %d affich\u00e9e(s).", nrow(t), nrow(montres))
				),
				lapply(seq_len(nrow(montres)), function(i) {
					l <- montres[i, ]
					traite <- identical(l$statut, "traite")
					shiny::tags$details(
						class = paste("cq-retour", if (traite) "is-traite"),
						shiny::tags$summary(
							shiny::tags$code(l$seed_id),
							shiny::tags$span(class = "cq-retour-question", l$question),
							shiny::tags$span(
								class = "cq-retour-chiffres",
								sprintf(
									"%d comm. \u00b7 \u00e9chec %s (%d r\u00e9p.) \u00b7 ressenti %s / banque %s",
									l$n_commentaires, pourcent(l$taux_echec), l$n_reponses,
									un_chiffre(l$difficulte_ressentie), un_chiffre(l$difficulte_banque)
								)
							)
						),
						if (!is.na(l$theme)) shiny::tags$p(class = "muted tiny", l$theme),
						if (l$n_commentaires) {
							shiny::tags$ul(lapply(l$commentaires[[1L]], shiny::tags$li))
						} else {
							shiny::tags$p(class = "muted", "Pas de commentaire.")
						},
						shiny::tags$button(
							type = "button",
							class = "btn",
							onclick = sprintf(
								"Shiny.setInputValue('%s', {seed: '%s', statut: '%s'}, {priority: 'event'});",
								session$ns("marquer"),
								# Identifiants de graine : lettres, chiffres et tirets seulement.
								gsub("[^A-Za-z0-9_-]", "", l$seed_id),
								if (traite) "a_traiter" else "traite"
							),
							if (traite) "Remettre \u00e0 traiter" else "Marquer comme trait\u00e9e"
						)
					)
				})
			)
		})

		shiny::observeEvent(input$marquer, {
			if (!est_admin()) {
				return(invisible(NULL))
			}

			tryCatch({
				retours_marquer(input$marquer$seed, input$marquer$statut)
				maj(maj() + 1L)
			}, error = function(e) {
				shiny::showNotification(cq_message_utilisateur(e), type = "error", duration = 6)
			})
		})

		output$csv <- shiny::downloadHandler(
			filename = function() paste0("candidequizz-retours-", format(Sys.Date()), ".csv"),
			content = function(file) {
				if (!est_admin()) {
					utils::write.csv(data.frame(), file, row.names = FALSE)
					return(invisible(file))
				}
				utils::write.csv(retours_csv(retours_tableau()), file, row.names = FALSE, fileEncoding = "UTF-8")
			}
		)

		invisible(NULL)
	})
}
