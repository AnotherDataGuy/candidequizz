# Écran de connexion. Le reste de l'application n'apparaît qu'une fois
# le compte reconnu.

#' Connexion — interface
#' @noRd
mod_auth_ui <- function(id) {
	ns <- shiny::NS(id)

	# M\u00eame illustration que l'accueil, guichet d\u00e9j\u00e0 ouvert (sans bouton :
	# guichet.js ne vise que la sc\u00e8ne de l'accueil, par son id).
	guichet_svg <- {
		chemin <- app_sys("app/www/guichet.svg")
		if (nzchar(chemin) && file.exists(chemin)) {
			shiny::HTML(paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
		}
	}

	shiny::div(
		class = "cq-accueil cq-auth",
		shiny::tags$section(
			class = "hero",
			shiny::tags$div(
				class = "hero-copy",
				shiny::tags$div(
					class = "row",
					shiny::tags$span(class = "edition-number", "00 /"),
					shiny::tags$span(class = "eyebrow", "Candide au guichet")
				),
				shiny::tags$h1("Entrez."),
				shiny::tags$p(
					"Un compte relie vos s\u00e9ances \u00e0 vous.",
					shiny::tags$br(),
					if (identical(store_sql_moteur(), "postgres")) {
						"La progression est enregistr\u00e9e pour votre compte."
					} else {
						"La progression reste sur ce serveur, et peut dispara\u00eetre s'il red\u00e9marre."
					}
				),
				shiny::uiOutput(ns("alerte")),
				shiny::div(
					class = "cq-auth-form",
					shiny::textInput(ns("username"), "Identifiant", width = "100%"),
					shiny::passwordInput(ns("password"), "Mot de passe", width = "100%"),
					shiny::div(
						class = "hero-cta",
						shiny::actionButton(ns("entrer"), "Entrer", class = "btn primary"),
						if (auth_inscription_ouverte()) {
							shiny::actionButton(ns("creer"), "Cr\u00e9er un compte", class = "btn")
						}
					)
				)
			),
			shiny::tags$div(
				class = "guichet-scene is-open",
				`aria-hidden` = "true",
				guichet_svg
			)
		)
	)
}

#' Connexion — serveur
#'
#' @param utilisateur Réactif à renseigner avec le compte connecté.
#' @noRd
mod_auth_server <- function(id, utilisateur) {
	shiny::moduleServer(id, function(input, output, session) {
		alerte <- shiny::reactiveVal(NULL)
		message_bloque <- "Trop de tentatives. Patientez une minute avant de r\u00e9essayer."

		output$alerte <- shiny::renderUI({
			message <- alerte()

			if (is.null(message)) {
				return(NULL)
			}

			shiny::div(class = "cq-config-banner cq-config-banner--warn", role = "alert", message)
		})

		connecter <- function(user) {
			alerte(NULL)
			utilisateur(user)
			session$sendCustomMessage("cq-auth-token", list(action = "set", token = user$token))
		}

		shiny::observeEvent(input$entrer, {
			# Après 5 échecs sur un identifiant, refuser pendant une minute
			# (voir auth_bloque() dans fct_auth.R), sans vérifier le mot de passe.
			if (auth_bloque(input$username)) {
				alerte(message_bloque)
				return(invisible(NULL))
			}

			tryCatch(
				{
					connecter(auth_connecter(input$username, input$password))
					auth_oublier_echecs(input$username)
				},
				error = function(e) {
					auth_noter_echec(input$username)
					alerte(if (auth_bloque(input$username)) message_bloque else cq_message_utilisateur(e))
				}
			)
		})

		shiny::observeEvent(input$creer, {
			tryCatch({
				auth_creer(input$username, input$password, admin = FALSE)
				connecter(auth_connecter(input$username, input$password))
			}, error = function(e) alerte(cq_message_utilisateur(e)))
		})

		invisible(NULL)
	})
}
