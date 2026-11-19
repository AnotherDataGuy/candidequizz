#' Accueil — interface
#'
#' @param id Identifiant du module.
#'
#' @return Un tag HTML.
#' @noRd
mod_home_ui <- function(id) {
	ns <- shiny::NS(id)

	guichet_svg <- {
		chemin <- app_sys("app/www/guichet.svg")
		if (nzchar(chemin) && file.exists(chemin)) {
			shiny::HTML(paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
		} else {
			NULL
		}
	}

	shiny::tags$div(
		class = "cq-accueil",
		shiny::tags$section(
			class = "hero",
			shiny::tags$div(
				class = "hero-copy",
				shiny::tags$div(
					class = "row",
					shiny::tags$span(class = "edition-number", "01 /"),
					shiny::tags$span(class = "eyebrow", "Pr\u00e9paration \u00e0 l'examen civique"),
					# Version en d\u00e9veloppement : le d\u00e9tail est dans \u00ab Notre d\u00e9marche \u00bb.
					# Tampon encreur (SVG) : bords us\u00e9s par un filtre de bruit.
					shiny::tags$span(
						class = "cq-tampon-dev",
						title = "Application en cours de d\u00e9veloppement : les questions sont encore en test.",
						shiny::HTML('<svg class="cq-tampon-svg" viewBox="0 0 176 56" width="150" height="48" role="img" aria-label="Version d\'essai, questions en test"><defs><filter id="cq-encre" x="-5%" y="-5%" width="110%" height="110%"><feTurbulence type="fractalNoise" baseFrequency="0.85" numOctaves="2" seed="7" result="bruit"/><feColorMatrix in="bruit" type="matrix" values="0 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 -1.4 1.6" result="usure"/><feComposite in="SourceGraphic" in2="usure" operator="in"/></filter></defs><g filter="url(#cq-encre)"><rect x="2" y="2" width="172" height="52" rx="7" fill="none" stroke="currentColor" stroke-width="2.4"/><rect x="7" y="7" width="162" height="42" rx="4" fill="none" stroke="currentColor" stroke-width=".9"/><text x="88" y="27" text-anchor="middle" font-family="\'IBM Plex Mono\', ui-monospace, monospace" font-weight="700" font-size="12.5" letter-spacing="1.7" fill="currentColor">VERSION D\'ESSAI</text><path d="M30 33.5 H146" stroke="currentColor" stroke-width=".8" stroke-dasharray="2 2.5"/><text x="88" y="44" text-anchor="middle" font-family="Georgia, serif" font-style="italic" font-size="10.5" fill="currentColor">questions en test</text></g></svg>')
					)
				),
				shiny::tags$h1(
					"Des si\u00e8cles d'histoire,",
					shiny::tags$br(),
					"quarante questions.",
					shiny::tags$br(),
					shiny::tags$span(
						class = "serif",
						"Prenez le temps qu'il faut."
					)
				),
				shiny::tags$p(
					"Pr\u00e9parez votre examen civique avec confiance.",
					shiny::tags$br(),
					"Des questions pour vous entra\u00eener,",
					shiny::tags$br(),
					"des explications pour comprendre."
				),
		shiny::tags$div(
			class = "hero-cta",
			shiny::actionButton(
				inputId = ns("demarrer"),
				label = shiny::tagList("Je me pr\u00e9pare", cq_icon("i-arrow")),
				class = "btn primary"
			),
			shiny::tags$span(
				class = "hero-micro",
				"\u00c0 votre rythme.",
				shiny::tags$br(),
				"Quand vous voulez."
			)
		),
		shiny::uiOutput(ns("reprise"))
			),
			shiny::tags$div(
				class = "guichet-scene",
				id = "guichet-scene",
				guichet_svg,
				shiny::tags$div(
					class = "scene-controls",
					shiny::tags$button(
						class = "scene-toggle",
						id = "open-guichet",
						type = "button",
						`aria-pressed` = "false",
						`aria-label` = "Ouvrir le guichet de l'illustration",
						shiny::tags$span(class = "switch-light"),
						shiny::tags$span(id = "guichet-label", "Et si on ouvrait ?"),
						shiny::tags$span(`aria-hidden` = "true", "\u2197")
					),
					shiny::tags$span(
						class = "scene-caption",
						id = "scene-caption",
						`aria-live` = "polite",
						"Ici, on peut s'entra\u00eener \u00e0 toute heure."
					)
				)
			)
		),
		shiny::tags$div(
			class = "editorial-band",
			shiny::tags$span(class = "eyebrow", "Le principe"),
			shiny::tags$p(
				"Un rep\u00e8re apr\u00e8s l'autre, ",
				shiny::tags$span(class = "serif", "la confiance se construit.")
			),
			shiny::tags$button(
				class = "textbtn",
				type = "button",
				`data-manifesto` = "true",
				"Notre d\u00e9marche \u2197"
			)
		)
	)
}

#' Accueil — serveur
#'
#' @param id Identifiant du module.
#' @param aller_a Fonction appelée avec l'identifiant de la section.
#' @param reprendre Conservé pour compatibilité d'appel (inutilisé).
#' @param rafraichir Conservé pour compatibilité d'appel (inutilisé).
#'
#' @return `NULL`, invisiblement.
#' @noRd
mod_home_server <- function(id,
														aller_a = function(section) NULL,
														reprendre = function(session_id) NULL,
														utilisateur = shiny::reactive(NULL),
														donnees_maj = shiny::reactive(0)) {
	shiny::moduleServer(id, function(input, output, session) {
		shiny::observeEvent(input$demarrer, {
			aller_a("seances")
		})

		output$reprise <- shiny::renderUI({
			donnees_maj()
			u <- utilisateur()
			if (is.null(u)) {
				return(NULL)
			}

			ligne <- store_seance_reprenable(u$user_id)

			if (is.null(ligne)) {
				return(NULL)
			}

			shiny::div(
				class = "cq-carte",
				shiny::tags$p(class = "eyebrow", "S\u00e9ance en cours"),
				shiny::tags$p(ligne$libelle[[1L]]),
				shiny::div(
					class = "hero-cta",
					shiny::actionButton(session$ns("reprendre_btn"), "Reprendre", class = "btn primary"),
					shiny::actionButton(session$ns("abandonner_btn"), "Abandonner", class = "btn")
				)
			)
		})

		shiny::observeEvent(input$reprendre_btn, {
			u <- utilisateur()
			shiny::req(u)
			ligne <- store_seance_reprenable(u$user_id)
			shiny::req(ligne)
			reprendre(ligne$session_id[[1L]])
			aller_a("seances")
		})

		shiny::observeEvent(input$abandonner_btn, {
			u <- utilisateur()
			shiny::req(u)
			ligne <- store_seance_reprenable(u$user_id)
			shiny::req(ligne)
			seance <- store_charger(ligne$session_id[[1L]], user_id = u$user_id)
			seance <- seance_clore(seance, "abandonnee")
			store_sauver(seance, user_id = u$user_id)
			reprendre(seance$session_id)
			aller_a("seances")
		})

		invisible(NULL)
	})
}
