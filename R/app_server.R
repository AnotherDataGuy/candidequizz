#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	async_preparer()
	local <- identical(auth_mode(), "local")

	if (!local) {
		auth_bootstrap()
	}

	# Mode local (version à télécharger) : pas de connexion, un compte unique.
	utilisateur <- shiny::reactiveVal(if (local) auth_utilisateur_local() else NULL)

	output$connecte <- shiny::reactive(!is.null(utilisateur()))
	shiny::outputOptions(output, "connecte", suspendWhenHidden = FALSE)

	shiny::observeEvent(input$cq_auth_token, {
		if (!is.null(shiny::isolate(utilisateur()))) {
			return(invisible(NULL))
		}

		compte <- auth_token_resoudre(input$cq_auth_token)

		if (is.null(compte)) {
			session$sendCustomMessage("cq-auth-token", list(action = "clear"))
			return(invisible(NULL))
		}

		utilisateur(compte)
	}, ignoreNULL = TRUE)

	mod_auth_server("auth", utilisateur = utilisateur)

	# Version en ligne : l'interface principale n'est envoyée qu'une fois le
	# compte connu. Construite une seule fois : la déconnexion recharge la page.
	if (!local) {
		output$cq_principale <- shiny::renderUI({
			shiny::req(utilisateur())
			shiny::isolate(cq_ui_principale())
		})
	}

	output$compte <- shiny::renderUI({
		u <- utilisateur()
		shiny::req(u, !local)

		shiny::tags$div(
			class = "header-tools",
			shiny::tags$span(class = "tiny muted", u$username),
			shiny::actionButton("deconnecter", "Sortir", class = "textbtn")
		)
	})

	shiny::observeEvent(input$deconnecter, {
		u <- utilisateur()

		if (!is.null(u)) {
			try(auth_token_revoquer(u$token), silent = TRUE)
		}

		utilisateur(NULL)
		session$sendCustomMessage("cq-auth-token", list(action = "clear"))
		# Repartir d'une session Shiny vierge : sinon la s\u00e9ance en m\u00e9moire
		# du compte pr\u00e9c\u00e9dent resterait visible pour le compte suivant.
		session$reload()
	})

	# Section courante : source de vérité unique de la navigation. Les boutons
	# de la navbar et les appels programmatiques (bouton d'appel à l'action de
	# l'accueil) passent tous par elle.
	section <- reactiveVal("accueil")

	# Signal partagé : incrémenté quand une séance démarre, se termine ou
	# est abandonnée. L'accueil, la révision ciblée et la progression le
	# lisent pour se mettre à jour sans rechargement.
	donnees_maj <- reactiveVal(0)

	# Carnet de progression : verrouillé jusqu'au seuil de questions
	# (voir fct_progression.R). `jauge` porte le panneau affiché, ou NULL.
	progression <- reactive({
		donnees_maj()
		u <- utilisateur()
		req(u)
		progression_etat(u$user_id)
	})
	jauge <- reactiveVal(NULL)

	lapply(cq_sections(), function(cible) {
		observeEvent(input[[cq_nav_input_id(cible$id)]], {
			if (identical(cible$id, "statistiques") && !isTRUE(progression()$debloque)) {
				jauge(list(gain = 0L, ouvert = FALSE))
				return(invisible(NULL))
			}
			jauge(NULL)
			section(cible$id)
		})
	})

	observeEvent(section(), {
		bslib::nav_select("cq_section", selected = section())
		session$sendCustomMessage("cq_nav_active", section())
	})

	aller_a <- function(cible) {
		section(cible)
	}

	# Reprise d'une séance : la section « Séances » charge l'identifiant
	# fourni via ce réactif (ex. liens futurs depuis Progression).
	reprise <- reactiveVal(NULL)

	# Th\u00e8me choisi depuis la fen\u00eatre d'un th\u00e8me du carrousel.
	theme_preselection <- reactiveVal(NULL)

	observeEvent(input$theme_info, {
		req(utilisateur())
		fenetre <- cq_theme_modal(input$theme_info)
		if (!is.null(fenetre)) {
			showModal(fenetre)
		}
	})

	# « Comment l'IA fonctionne ici » : ouvert par cq_ia_lien() depuis la
	# composition d'une séance, la fenêtre de la surprise ou « Pourquoi Candide ? ».
	observeEvent(input$ia_info, {
		showModal(cq_ia_modal())
	})

	observeEvent(input$theme_entrainer, {
		req(utilisateur())
		removeModal()
		theme_preselection(list(theme_id = input$theme_entrainer, t = Sys.time()))
		aller_a("seances")
	})

	mod_home_server(
		"accueil",
		aller_a = aller_a,
		reprendre = function(session_id) {
			reprise(session_id)
			aller_a("seances")
		},
		utilisateur = utilisateur,
		donnees_maj = donnees_maj
	)

	mod_seances_server(
		"seances",
		reprendre = reprise,
		utilisateur = utilisateur,
		donnees_maj = donnees_maj,
		theme_preselection = theme_preselection,
		apres_avis = function(n_reponses) {
			etat <- progression_etat(utilisateur()$user_id)
			# Seuil franchi pendant cette séance : la jauge devient la récompense.
			vient_d_ouvrir <- etat$debloque && (etat$n - n_reponses) < etat$seuil
			if (!etat$debloque || vient_d_ouvrir) {
				jauge(list(gain = n_reponses, ouvert = vient_d_ouvrir, etat = etat))
			}

			# Deuxième surprise : l'IA s'ouvre à ce moment-là (seuil de questions
			# différentes franchi pendant cette séance).
			u <- utilisateur()
			ia <- ia_etat(u$user_id, admin = isTRUE(u$is_admin))
			if (isFALSE(ia_ouverte_avant()) && ia$debloque) {
				showModal(cq_ia_surprise(ia))
			}
			ia_ouverte_avant(ia$debloque)
		}
	)

	# IA déjà ouverte à l'arrivée du compte ? Sert à ne montrer la surprise
	# qu'au moment où le seuil est franchi, une seule fois.
	ia_ouverte_avant <- reactiveVal(NA)

	# Jauge affichée dès l'arrivée tant que le carnet est verrouillé.
	observeEvent(utilisateur(), {
		if (!isTRUE(progression()$debloque)) {
			jauge(list(gain = 0L, ouvert = FALSE))
		}
		u <- utilisateur()
		ia_ouverte_avant(isTRUE(ia_etat(u$user_id, admin = isTRUE(u$is_admin))$debloque))
	})

	observeEvent(input$jauge_fermer, jauge(NULL))

	output$jauge <- renderUI({
		j <- jauge()
		req(j, utilisateur())
		cq_jauge_ui(j$etat %||% isolate(progression()), gain = j$gain, ouvert = j$ouvert)
	})

	# Le bouton Progression du dock n'apparaît qu'une fois le seuil atteint ;
	# la section n'est calculée qu'à partir de ce moment.
	stats_lance <- FALSE
	premier_etat <- TRUE
	observeEvent(progression()$debloque, {
		session$sendCustomMessage("cq-progression", list(
			debloque = progression()$debloque,
			nouveau = progression()$debloque && !premier_etat
		))
		premier_etat <<- FALSE
		if (progression()$debloque && !stats_lance) {
			stats_lance <<- TRUE
			isolate(mod_stats_server("statistiques", utilisateur = utilisateur, donnees_maj = donnees_maj))
		}
	})

	mod_ressources_server("ressources")
}
