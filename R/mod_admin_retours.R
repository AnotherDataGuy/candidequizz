# Administration : retours des participants, question par question.
#
# Onglet « Questions » de la section Administration (mod_admin.R), pour
# l'administrateur seulement. Sert à repérer les questions à corriger dans la
# banque : commentaires, taux d'échec, difficulté ressentie. La correction se
# fait dans le classeur Excel de la banque, grâce à l'identifiant de graine.

#' Retours — interface
#' @noRd
mod_admin_retours_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-retours",
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
				width = "150px"
			),
			shiny::selectInput(
				ns("tri"), NULL,
				choices = c(
					"Commentaires d'abord" = "commentaires",
					"Plus souvent rat\u00e9es" = "echec",
					"Ressenties plus difficiles" = "difficulte"
				),
				width = "240px"
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
mod_admin_retours_server <- function(id, utilisateur, tableau = NULL, rafraichir = NULL) {
	shiny::moduleServer(id, function(input, output, session) {
		maj <- shiny::reactiveVal(0L)
		est_admin <- shiny::reactive(isTRUE(utilisateur()$is_admin))

		# Tableau fourni par l'écran d'administration (lu une fois pour tous les
		# onglets), sinon relu ici à la demande (bouton Actualiser, ou après un
		# changement de statut) : la lecture parcourt toutes les séances.
		if (is.null(tableau)) {
			tableau <- shiny::reactive({
				maj()
				input$actualiser
				shiny::req(est_admin())
				tryCatch(retours_tableau(), error = function(e) NULL)
			})
		}
		relire <- function() {
			if (is.null(rafraichir)) maj(maj() + 1L) else rafraichir()
		}
		shiny::observeEvent(input$actualiser, {
			if (!is.null(rafraichir)) rafraichir()
		}, ignoreInit = TRUE)

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

			shiny::tagList(
				shiny::tags$p(
					class = "muted tiny",
					sprintf("%d question(s), %d affich\u00e9e(s). Touchez une ligne pour lire les commentaires.", nrow(t), nrow(montres))
				),
				shiny::div(
					class = "cq-q-liste",
					lapply(seq_len(nrow(montres)), function(i) retours_ligne(montres[i, ], session$ns("marquer")))
				)
			)
		})

		shiny::observeEvent(input$marquer, {
			if (!est_admin()) {
				return(invisible(NULL))
			}

			tryCatch({
				retours_marquer(input$marquer$seed, input$marquer$statut)
				relire()
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

#' Une question dans la liste des retours (ligne repliable)
#'
#' @param l Une ligne de [retours_tableau()].
#' @param input_id Identifiant d'entrée du bouton « traitée ».
#' @noRd
retours_ligne <- function(l, input_id) {
	traite <- identical(l$statut, "traite")
	echec <- l$taux_echec
	ressenti <- l$difficulte_ressentie
	prevue <- l$difficulte_banque
	ecart <- if (is.na(ressenti) || is.na(prevue)) NA_real_ else ressenti - prevue
	un_chiffre <- function(x) if (is.na(x)) "\u2013" else format(round(x, 1), decimal.mark = ",")
	# Identifiants de graine : lettres, chiffres et tirets seulement.
	seed <- gsub("[^A-Za-z0-9_-]", "", l$seed_id)

	bouton <- shiny::tags$button(
		type = "button",
		class = paste("cq-q-traiter", if (traite) "is-traite"),
		title = if (traite) "Remettre \u00e0 traiter" else "Marquer comme trait\u00e9e",
		onclick = sprintf(
			"event.preventDefault();event.stopPropagation();Shiny.setInputValue('%s', {seed: '%s', statut: '%s'}, {priority: 'event'});",
			input_id, seed, if (traite) "a_traiter" else "traite"
		),
		bsicons::bs_icon(if (traite) "arrow-counterclockwise" else "check2"),
		if (traite) " Rouvrir" else " Trait\u00e9e"
	)

	shiny::tags$details(
		class = paste("cq-q", if (traite) "is-traite"),
		shiny::tags$summary(
			shiny::div(
				class = "cq-q-texte",
				shiny::div(
					class = "cq-q-meta",
					if (!is.na(l$theme)) shiny::tags$span(class = "cq-q-theme", l$theme),
					shiny::tags$code(l$seed_id),
					if (l$n_commentaires) {
						shiny::tags$span(
							class = "cq-q-comm",
							title = "Commentaires",
							bsicons::bs_icon("chat-left-text"), paste0(" ", l$n_commentaires)
						)
					}
				),
				shiny::tags$span(class = "cq-q-question", l$question %||% "")
			),
			shiny::div(
				class = "cq-q-chiffre",
				`data-label` = "\u00c9chec",
				if (is.na(echec)) {
					shiny::tags$span(class = "muted", "pas de r\u00e9ponse")
				} else {
					shiny::tagList(
						admin_barre(echec, admin_couleur_echec(echec)),
						shiny::tags$span(class = "tiny", sprintf("%d %% \u00b7 %d r\u00e9p.", round(100 * echec), l$n_reponses))
					)
				}
			),
			shiny::div(
				class = "cq-q-chiffre",
				`data-label` = "Difficult\u00e9",
				title = "Difficult\u00e9 ressentie par les joueurs / pr\u00e9vue dans la banque (1 facile, 10 difficile)",
				shiny::tags$span(
					class = paste(
						"cq-q-diff",
						if (!is.na(ecart) && ecart >= 2) "is-plus-dure",
						if (!is.na(ecart) && ecart <= -2) "is-plus-facile"
					),
					un_chiffre(ressenti), shiny::tags$span(class = "muted", paste0("/ ", un_chiffre(prevue))),
					if (!is.na(ecart) && ecart >= 2) bsicons::bs_icon("arrow-up-short"),
					if (!is.na(ecart) && ecart <= -2) bsicons::bs_icon("arrow-down-short")
				),
				shiny::tags$span(class = "muted tiny", "ressentie / pr\u00e9vue")
			),
			bouton
		),
		shiny::div(
			class = "cq-q-detail",
			if (l$n_commentaires) {
				shiny::tags$ul(class = "cq-q-commentaires", lapply(l$commentaires[[1L]], shiny::tags$li))
			} else {
				shiny::tags$p(class = "muted", "Pas de commentaire.")
			},
			shiny::tags$p(
				class = "muted tiny",
				sprintf(
					"%d avis \u00b7 temps de r\u00e9ponse m\u00e9dian : %s",
					l$n_avis,
					if (is.na(l$duree_mediane_s)) "\u2013" else paste0(round(l$duree_mediane_s), " s")
				)
			)
		)
	)
}
