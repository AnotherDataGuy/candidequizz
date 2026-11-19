#' Administration — interface
#'
#' Section à part, ouverte par l'icône « comptes » de l'en-tête, visible
#' seulement des administrateurs (voir `output$admin_bouton`, app_server.R).
#' Le contenu n'est envoyé qu'aux administrateurs : pour les autres comptes,
#' le panneau reste vide, et chaque action revérifie `is_admin` côté serveur.
#'
#' Trois onglets : vue d'ensemble (chiffres, activité, thèmes, points
#' d'attention, exports), comptes (une ligne par compte, avec ses actions),
#' questions (retours des participants, mod_admin_retours.R). Les calculs
#' sont dans fct_admin.R.
#'
#' @param id Identifiant du module.
#'
#' @return Un tag HTML.
#' @noRd
mod_admin_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::tags$div(
		class = "cq-admin",
		shiny::uiOutput(ns("panneau"))
	)
}

#' Administration — serveur
#'
#' @param id Identifiant du module.
#' @param utilisateur Réactif : compte connecté.
#'
#' @return `NULL`, invisiblement.
#' @noRd
mod_admin_server <- function(id, utilisateur = shiny::reactive(NULL)) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns
		est_admin <- shiny::reactive({
			u <- utilisateur()
			!is.null(u) && isTRUE(u$is_admin)
		})

		# Toutes les données de l'écran, relues d'un coup (bouton Actualiser,
		# ou après une action) : la lecture parcourt toutes les séances.
		maj <- shiny::reactiveVal(0L)
		rafraichir <- function() maj(maj() + 1L)

		donnees <- shiny::reactive({
			maj()
			input$actualiser
			shiny::req(est_admin())
			seances <- admin_seances()
			retours <- tryCatch(
				retours_tableau(seances, noms = retours_noms(), statuts = retours_statuts()),
				error = function(e) NULL
			)
			comptes <- admin_comptes_tableau(admin_comptes_bruts(), seances)
			activite <- admin_activite_jours(seances, jours = 30L)

			chiffres <- admin_chiffres(comptes, activite, retours)
			appli <- tryCatch(retour_appli_lister(), error = function(e) retour_appli_lister_vide())
			chiffres$appli_nouveaux <- sum(appli$statut == "nouveau")

			list(
				comptes = comptes,
				activite = activite,
				retours = retours,
				appli = appli,
				chiffres = chiffres,
				lu_le = Sys.time()
			)
		})

		mod_admin_retours_server(
			"retours",
			utilisateur = utilisateur,
			tableau = shiny::reactive(donnees()$retours),
			rafraichir = rafraichir
		)

		# Squelette : rendu une seule fois par connexion (les onglets gardent
		# leur position quand les données sont relues).
		output$panneau <- shiny::renderUI({
			shiny::req(est_admin())

			shiny::tagList(
				shiny::div(
					class = "cq-admin-tete",
					shiny::div(
						shiny::tags$p(class = "eyebrow", "Administration"),
						shiny::tags$h2(class = "cq-admin-titre", "Le guichet, c\u00f4t\u00e9 coulisses"),
						shiny::uiOutput(ns("lu_le"), inline = TRUE)
					),
					shiny::div(
						class = "cq-admin-actions",
						shiny::actionButton(
							ns("actualiser"),
							shiny::tagList(bsicons::bs_icon("arrow-clockwise"), " Actualiser"),
							class = "btn"
						),
						shiny::actionButton(
							ns("nouveau"),
							shiny::tagList(bsicons::bs_icon("person-plus"), " Nouveau compte"),
							class = "btn primary"
						)
					)
				),
				bslib::navset_underline(
					id = ns("onglet"),
					bslib::nav_panel(
						title = shiny::tagList(bsicons::bs_icon("speedometer2"), " Vue d'ensemble"),
						value = "vue",
						shiny::uiOutput(ns("vue"))
					),
					bslib::nav_panel(
						title = shiny::tagList(
							bsicons::bs_icon("people"), " Comptes ",
							shiny::uiOutput(ns("badge_comptes"), inline = TRUE)
						),
						value = "comptes",
						shiny::div(
							class = "cq-admin-outils",
							shiny::textInput(ns("cherche"), NULL, placeholder = "Chercher un alias", width = "220px"),
							shiny::uiOutput(ns("filtre_statut"), inline = TRUE)
						),
						shiny::uiOutput(ns("comptes"))
					),
					bslib::nav_panel(
						title = shiny::tagList(
							bsicons::bs_icon("patch-question"), " Questions ",
							shiny::uiOutput(ns("badge_questions"), inline = TRUE)
						),
						value = "questions",
						mod_admin_retours_ui(ns("retours"))
					),
					bslib::nav_panel(
						title = shiny::tagList(
							bsicons::bs_icon("chat-square-heart"), " Retours appli ",
							shiny::uiOutput(ns("badge_appli"), inline = TRUE)
						),
						value = "appli",
						shiny::div(
							class = "cq-admin-outils",
							shiny::selectInput(
								ns("appli_statut"), NULL,
								choices = c("Nouveaux" = "nouveau", "Trait\u00e9s" = "traite", "Tous" = "tous"),
								width = "160px"
							),
							shiny::selectInput(
								ns("appli_genre"), NULL,
								choices = c("Probl\u00e8mes et id\u00e9es" = "tous", "Probl\u00e8mes" = "probleme", "Id\u00e9es" = "suggestion"),
								width = "210px"
							)
						),
						shiny::uiOutput(ns("appli"))
					)
				)
			)
		})

		output$lu_le <- shiny::renderUI({
			d <- donnees()
			shiny::tags$span(
				class = "muted tiny",
				paste0("Donn\u00e9es relues \u00e0 ", format(d$lu_le, "%H:%M", tz = "Europe/Paris"), ".")
			)
		})

		output$badge_comptes <- shiny::renderUI({
			admin_pastille(nrow(donnees()$comptes))
		})

		output$badge_questions <- shiny::renderUI({
			n <- donnees()$chiffres$a_revoir
			if (!n) {
				return(NULL)
			}
			admin_pastille(paste(n, "\u00e0 revoir"), alerte = TRUE)
		})

		output$badge_appli <- shiny::renderUI({
			n <- donnees()$chiffres$appli_nouveaux
			if (!n) {
				return(NULL)
			}
			admin_pastille(paste(n, if (n > 1L) "nouveaux" else "nouveau"), alerte = TRUE)
		})

		# --- Retours sur l'application ---------------------------------------

		output$appli <- shiny::renderUI({
			t <- donnees()$appli
			statut <- input$appli_statut %||% "nouveau"
			genre <- input$appli_genre %||% "tous"
			if (!identical(statut, "tous")) t <- t[t$statut == statut, , drop = FALSE]
			if (!identical(genre, "tous")) t <- t[t$genre == genre, , drop = FALSE]

			if (!nrow(donnees()$appli)) {
				return(shiny::tags$p(
					class = "muted",
					"Aucun retour pour l'instant. Vos amis peuvent en d\u00e9poser avec le bouton ",
					bsicons::bs_icon("chat-square-heart"), " de l'en-t\u00eate."
				))
			}
			if (!nrow(t)) {
				return(shiny::tags$p(class = "cq-admin-ok", bsicons::bs_icon("check2-circle"), " Rien \u00e0 lire avec ces filtres."))
			}

			shiny::div(
				class = "cq-appli-liste",
				lapply(seq_len(nrow(t)), function(i) admin_carte_appli(t[i, ], ns("appli_marquer")))
			)
		})

		shiny::observeEvent(input$appli_marquer, {
			shiny::req(est_admin())
			tryCatch({
				retour_appli_marquer(input$appli_marquer$id, input$appli_marquer$statut)
				rafraichir()
			}, error = function(e) {
				shiny::showNotification(cq_message_utilisateur(e), type = "error", duration = 6)
			})
		})

		shiny::observeEvent(input$voir_appli, {
			shiny::req(est_admin())
			bslib::nav_select("onglet", "appli")
		})

		# --- Vue d'ensemble ---------------------------------------------------

		output$vue <- shiny::renderUI({
			d <- donnees()
			k <- d$chiffres
			pourcent <- function(x) if (is.na(x)) "\u2013" else paste0(round(100 * x), " %")

			shiny::tagList(
				shiny::div(
					class = "cq-admin-tuiles",
					admin_tuile(
						"Comptes", k$n_comptes,
						sprintf("%d actif%s ces 14 jours", k$n_actifs, if (k$n_actifs > 1L) "s" else ""),
						icone = "people",
						lien = ns("voir_tous")
					),
					admin_tuile(
						"R\u00e9ponses (7 jours)", k$reponses_7j,
						sprintf("%s au total", format(k$reponses_total, big.mark = "\u202f")),
						icone = "pencil-square",
						extra = admin_mini_barres(utils::tail(d$activite$n_reponses, 14L))
					),
					admin_tuile(
						"R\u00e9ussite moyenne", pourcent(k$reussite),
						"toutes les r\u00e9ponses",
						icone = "bullseye",
						extra = if (!is.na(k$reussite)) admin_barre(k$reussite, admin_couleur_reussite(k$reussite))
					),
					admin_tuile(
						"Questions \u00e0 revoir", k$a_revoir,
						sprintf("%d commentaire%s re\u00e7u%s", k$n_commentaires,
										if (k$n_commentaires > 1L) "s" else "", if (k$n_commentaires > 1L) "s" else ""),
						icone = "chat-left-text",
						alerte = k$a_revoir > 0L,
						lien = if (k$a_revoir > 0L) ns("voir_questions")
					)
				),
				shiny::div(
					class = "cq-admin-grille",
					shiny::div(
						class = "cq-carte cq-admin-bloc",
						shiny::tags$h3("Activit\u00e9 des 30 derniers jours"),
						admin_histogramme(d$activite)
					),
					shiny::div(
						class = "cq-carte cq-admin-bloc",
						shiny::tags$h3("\u00c0 surveiller"),
						admin_alertes(d, ns)
					)
				),
				shiny::div(
					class = "cq-admin-grille",
					shiny::div(
						class = "cq-carte cq-admin-bloc",
						shiny::tags$h3("Taux d'\u00e9chec par th\u00e8me"),
						admin_barres_themes(admin_themes(d$retours))
					),
					shiny::div(
						class = "cq-carte cq-admin-bloc",
						shiny::tags$h3("Exports"),
						shiny::tags$p(class = "muted tiny", "Pour analyser dans un tableur."),
						shiny::div(
							class = "cq-admin-exports",
							shiny::downloadButton(ns("admin_avis"), "Avis sur les questions (CSV)", class = "btn"),
							if (identical(store_sql_moteur(), "sqlite")) {
								shiny::downloadButton(ns("admin_sqlite"), "Copie de la base (SQLite)", class = "btn")
							}
						),
						if (identical(store_sql_moteur(), "sqlite")) {
							shiny::tags$p(
								class = "muted tiny",
								"Base locale : la progression peut dispara\u00eetre au red\u00e9marrage du serveur. T\u00e9l\u00e9chargez une copie de temps en temps."
							)
						} else {
							shiny::tags$p(
								class = "muted tiny",
								"Donn\u00e9es dans la base PostgreSQL : sauvegardes chez l'h\u00e9bergeur de la base (Neon)."
							)
						}
					)
				)
			)
		})

		shiny::observeEvent(input$voir_questions, {
			shiny::req(est_admin())
			bslib::nav_select("onglet", "questions")
		})

		shiny::observeEvent(input$voir_tous, {
			shiny::req(est_admin())
			bslib::nav_select("onglet", "comptes")
			shiny::updateSelectInput(session, "statut", selected = "tous")
		})

		shiny::observeEvent(input$voir_comptes, {
			shiny::req(est_admin())
			bslib::nav_select("onglet", "comptes")
			shiny::updateSelectInput(session, "statut", selected = input$voir_comptes)
		})

		# --- Comptes ----------------------------------------------------------

		output$filtre_statut <- shiny::renderUI({
			t <- donnees()$comptes
			n <- function(s) sum(t$statut == s)
			shiny::selectInput(
				ns("statut"), NULL,
				choices = stats::setNames(
					c("tous", "actif", "inactif", "jamais"),
					c(
						sprintf("Tous (%d)", nrow(t)),
						sprintf("Actifs (%d)", n("actif")),
						sprintf("Inactifs (%d)", n("inactif")),
						sprintf("Sans s\u00e9ance (%d)", n("jamais"))
					)
				),
				selected = shiny::isolate(input$statut) %||% "tous",
				width = "200px"
			)
		})

		output$comptes <- shiny::renderUI({
			d <- donnees()
			t <- admin_comptes_filtrer(d$comptes, input$cherche, input$statut %||% "tous")

			if (!nrow(d$comptes)) {
				return(shiny::tags$p(class = "muted", "Aucun compte pour l'instant. Cr\u00e9ez-en un avec \u00ab Nouveau compte \u00bb."))
			}
			if (!nrow(t)) {
				return(shiny::tags$p(class = "muted", "Aucun compte ne correspond \u00e0 cette recherche."))
			}

			shiny::div(
				class = "cq-admin-liste",
				shiny::div(
					class = "cq-admin-ligne cq-admin-entete",
					`aria-hidden` = "true",
					shiny::tags$span("Compte"),
					shiny::tags$span("Derni\u00e8re activit\u00e9"),
					shiny::tags$span("Vers l'IA (questions diff\u00e9rentes)"),
					shiny::tags$span("R\u00e9ussite"),
					shiny::tags$span("S\u00e9ances"),
					shiny::tags$span("")
				),
				lapply(seq_len(nrow(t)), function(i) admin_ligne_compte(t[i, ], ns))
			)
		})

		# Actions d'une ligne : { quoi: "mdp" | "suppr", alias }.
		cible <- shiny::reactiveVal(NULL)

		shiny::observeEvent(input$action, {
			shiny::req(est_admin())
			compte <- auth_trouver(input$action$alias)
			if (is.null(compte)) {
				shiny::showNotification("Compte introuvable.", type = "error", duration = 6)
				return(invisible(NULL))
			}
			cible(compte$username)

			if (identical(input$action$quoi, "mdp")) {
				shiny::showModal(shiny::modalDialog(
					title = paste("Nouveau mot de passe pour", compte$username, "?"),
					shiny::tags$p(
						"Un mot de passe est g\u00e9n\u00e9r\u00e9 et affich\u00e9 une seule fois. ",
						"L'ancien ne marchera plus, et le compte sera d\u00e9connect\u00e9 de tous ses appareils."
					),
					footer = shiny::tagList(
						shiny::modalButton("Annuler"),
						shiny::actionButton(ns("mdp_ok"), "G\u00e9n\u00e9rer", class = "btn primary")
					),
					easyClose = TRUE
				))
			} else if (identical(input$action$quoi, "suppr")) {
				if (isTRUE(compte$is_admin)) {
					shiny::showNotification(
						"Un compte administrateur ne peut pas \u00eatre supprim\u00e9 depuis l'application.",
						type = "warning", duration = 6
					)
					return(invisible(NULL))
				}
				shiny::showModal(shiny::modalDialog(
					title = "Supprimer ce compte ?",
					shiny::tags$p(
						"Le compte ", shiny::tags$strong(compte$username),
						" et toutes ses s\u00e9ances, r\u00e9ponses et avis seront effac\u00e9s. C'est d\u00e9finitif."
					),
					footer = shiny::tagList(
						shiny::modalButton("Annuler"),
						shiny::actionButton(ns("suppr_ok"), "Supprimer", class = "btn cq-admin-danger")
					),
					easyClose = TRUE
				))
			}
		})

		shiny::observeEvent(input$mdp_ok, {
			shiny::req(est_admin(), cible())
			mdp <- admin_mot_de_passe()
			tryCatch({
				auth_reinitialiser(cible(), mdp)
				shiny::showModal(admin_modal_mdp(cible(), mdp, admin_adresse(session), nouveau = FALSE))
			}, error = function(e) {
				shiny::removeModal()
				shiny::showNotification(cq_message_utilisateur(e), type = "error", duration = 6)
			})
		})

		shiny::observeEvent(input$suppr_ok, {
			shiny::req(est_admin(), cible())
			shiny::removeModal()
			tryCatch({
				n <- auth_supprimer(cible())
				shiny::showNotification(
					glue::glue("Compte {cible()} supprim\u00e9 ({n} s\u00e9ance(s) effac\u00e9e(s))."),
					type = "message", duration = 5
				)
				rafraichir()
			}, error = function(e) {
				shiny::showNotification(cq_message_utilisateur(e), type = "error", duration = 6)
			})
		})

		# Nouveau compte : l'alias seulement, le mot de passe est généré.
		shiny::observeEvent(input$nouveau, {
			shiny::req(est_admin())
			shiny::showModal(shiny::modalDialog(
				title = "Nouveau compte",
				shiny::textInput(ns("nouvel_alias"), "Alias", placeholder = "marie_l", width = "100%"),
				shiny::tags$p(
					class = "muted tiny",
					"3 \u00e0 40 caract\u00e8res : lettres minuscules, chiffres ou _. ",
					"\u00c9vitez le vrai nom : un alias suffit. Le mot de passe est g\u00e9n\u00e9r\u00e9 \u00e0 l'\u00e9tape suivante."
				),
				shiny::uiOutput(ns("nouvel_erreur")),
				footer = shiny::tagList(
					shiny::modalButton("Annuler"),
					shiny::actionButton(ns("nouveau_ok"), "Cr\u00e9er le compte", class = "btn primary")
				),
				easyClose = TRUE
			))
		})

		nouvel_erreur <- shiny::reactiveVal(NULL)
		output$nouvel_erreur <- shiny::renderUI({
			m <- nouvel_erreur()
			if (is.null(m)) {
				return(NULL)
			}
			shiny::tags$p(class = "cq-admin-erreur", role = "alert", m)
		})

		shiny::observeEvent(input$nouveau, nouvel_erreur(NULL), priority = 10)

		shiny::observeEvent(input$nouveau_ok, {
			shiny::req(est_admin())
			alias <- auth_username(input$nouvel_alias)
			mdp <- admin_mot_de_passe()
			tryCatch({
				auth_creer(alias, mdp, par_admin = TRUE)
				nouvel_erreur(NULL)
				shiny::showModal(admin_modal_mdp(alias, mdp, admin_adresse(session), nouveau = TRUE))
				rafraichir()
			}, error = function(e) {
				nouvel_erreur(cq_message_utilisateur(e))
			})
		})

		# --- Exports ----------------------------------------------------------

		# Avis (notes de difficulté et commentaires), tous comptes.
		output$admin_avis <- shiny::downloadHandler(
			filename = function() paste0("candidequizz-avis-", format(Sys.Date()), ".csv"),
			content = function(file) {
				if (!est_admin()) {
					utils::write.csv(data.frame(), file, row.names = FALSE)
					return(invisible(file))
				}
				tableau <- avis_tableau()
				# Commentaires libres : pas de formule Excel (voir avis_csv_inerte()).
				if ("commentaire" %in% names(tableau)) {
					tableau$commentaire <- avis_csv_inerte(tableau$commentaire)
				}
				utils::write.csv(tableau, file, row.names = FALSE, fileEncoding = "UTF-8")
			}
		)

		output$admin_sqlite <- shiny::downloadHandler(
			filename = function() paste0("candidequizz-", format(Sys.Date(), "%Y%m%d"), ".sqlite"),
			content = function(file) {
				if (!est_admin()) {
					stop("Acc\u00e8s refus\u00e9.")
				}
				store_sqlite_checkpoint()
				file.copy(store_sqlite_chemin(), file, overwrite = TRUE)
			}
		)

		invisible(NULL)
	})
}

# --- Petits composants de l'écran d'administration -----------------------

#' Pastille de compte (onglets)
#' @noRd
admin_pastille <- function(texte, alerte = FALSE) {
	shiny::tags$span(class = paste("cq-admin-pastille", if (alerte) "is-alerte"), texte)
}

#' Couleur d'une barre de réussite (0 à 1)
#' @noRd
admin_couleur_reussite <- function(x) {
	if (is.na(x)) "neutre" else if (x >= 0.7) "bon" else if (x >= 0.5) "moyen" else "faible"
}

#' Couleur d'une barre d'échec (0 à 1)
#' @noRd
admin_couleur_echec <- function(x) {
	if (is.na(x)) "neutre" else if (x >= 0.5) "faible" else if (x >= 0.3) "moyen" else "bon"
}

#' Barre de progression horizontale
#'
#' @param part Part remplie (0 à 1).
#' @param couleur `"bon"`, `"moyen"`, `"faible"`, `"neutre"` ou `"ia"`.
#' @noRd
admin_barre <- function(part, couleur = "neutre") {
	part <- max(0, min(1, as.numeric(part)))
	shiny::tags$span(
		class = paste0("cq-admin-barre is-", couleur),
		shiny::tags$span(style = sprintf("width: %.1f%%", 100 * part))
	)
}

#' Tuile de chiffre clé
#'
#' @param lien Identifiant d'entrée : la tuile devient un bouton.
#' @noRd
admin_tuile <- function(titre, valeur, sous, icone = NULL, extra = NULL, alerte = FALSE, lien = NULL) {
	contenu <- shiny::tagList(
		shiny::tags$span(
			class = "cq-admin-tuile-titre",
			if (!is.null(icone)) bsicons::bs_icon(icone),
			titre
		),
		shiny::tags$span(class = "cq-admin-tuile-valeur", valeur),
		shiny::tags$span(class = "cq-admin-tuile-sous", sous),
		extra
	)
	classe <- paste("cq-admin-tuile", if (alerte) "is-alerte")

	if (is.null(lien)) {
		return(shiny::div(class = classe, contenu))
	}

	shiny::tags$button(
		type = "button",
		class = paste(classe, "is-lien"),
		onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", lien),
		contenu
	)
}

#' Mini-histogramme d'une tuile (14 derniers jours)
#' @noRd
admin_mini_barres <- function(valeurs) {
	m <- max(c(valeurs, 1))
	shiny::tags$span(
		class = "cq-admin-mini",
		`aria-hidden` = "true",
		lapply(valeurs, function(v) {
			shiny::tags$span(style = sprintf("height: %.0f%%", max(4, 100 * v / m)))
		})
	)
}

#' Histogramme d'activité (réponses par jour), en HTML et CSS
#' @noRd
admin_histogramme <- function(activite) {
	if (!sum(activite$n_reponses)) {
		return(shiny::tags$p(class = "muted", "Aucune r\u00e9ponse ces 30 derniers jours."))
	}

	m <- max(activite$n_reponses)
	etiquette <- function(j) format(j, "%d/%m")
	n <- nrow(activite)

	shiny::tagList(
		shiny::div(
			class = "cq-admin-histo",
			role = "img",
			`aria-label` = sprintf(
				"%d r\u00e9ponses sur 30 jours, au plus %d en un jour.",
				sum(activite$n_reponses), m
			),
			lapply(seq_len(n), function(i) {
				v <- activite$n_reponses[[i]]
				c <- activite$n_comptes[[i]]
				shiny::tags$span(
					class = paste("cq-admin-histo-col", if (i == n) "is-aujourdhui"),
					title = sprintf(
						"%s : %d r\u00e9ponse%s, %d compte%s",
						etiquette(activite$jour[[i]]), v, if (v > 1L) "s" else "",
						c, if (c > 1L) "s" else ""
					),
					shiny::tags$span(
						class = paste("cq-admin-histo-barre", if (!v) "is-vide"),
						style = sprintf("height: %.0f%%", if (v) max(4, 100 * v / m) else 2)
					)
				)
			})
		),
		shiny::div(
			class = "cq-admin-histo-axe",
			shiny::tags$span(etiquette(activite$jour[[1L]])),
			shiny::tags$span(etiquette(activite$jour[[ceiling(n / 2)]])),
			shiny::tags$span("aujourd'hui")
		),
		shiny::tags$p(
			class = "muted tiny",
			sprintf("Maximum : %d r\u00e9ponses en un jour. Survolez une colonne pour le d\u00e9tail.", m)
		)
	)
}

#' Barres horizontales : taux d'échec par thème
#' @noRd
admin_barres_themes <- function(themes) {
	if (!nrow(themes)) {
		return(shiny::tags$p(class = "muted", "Pas encore assez de r\u00e9ponses."))
	}

	shiny::div(
		class = "cq-admin-themes",
		lapply(seq_len(nrow(themes)), function(i) {
			x <- themes$taux_echec[[i]]
			shiny::div(
				class = "cq-admin-theme",
				shiny::tags$span(class = "cq-admin-theme-nom", themes$theme[[i]]),
				admin_barre(x, admin_couleur_echec(x)),
				shiny::tags$span(
					class = "cq-admin-theme-chiffre",
					title = sprintf("%d r\u00e9ponses", themes$n_reponses[[i]]),
					paste0(round(100 * x), " %")
				)
			)
		}),
		shiny::tags$p(class = "muted tiny", "Part de mauvaises r\u00e9ponses, toutes questions du th\u00e8me confondues.")
	)
}

#' Liste « À surveiller » de la vue d'ensemble
#' @noRd
admin_alertes <- function(d, ns) {
	t <- d$comptes
	k <- d$chiffres
	noms <- function(x) {
		x <- x[!t$is_admin[match(x, t$username)]]
		if (length(x) > 4L) paste0(paste(utils::head(x, 4L), collapse = ", "), "\u2026") else paste(x, collapse = ", ")
	}
	lien <- function(texte, id, valeur = "x") {
		shiny::tags$button(
			type = "button",
			class = "textbtn",
			onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns(id), valeur),
			texte
		)
	}

	jamais <- t$username[t$statut == "jamais" & !t$is_admin]
	inactifs <- t$username[t$statut == "inactif" & !t$is_admin]

	items <- list(
		if (length(jamais)) {
			list("person-x", sprintf("%d compte%s sans aucune s\u00e9ance : %s", length(jamais),
															 if (length(jamais) > 1L) "s" else "", noms(jamais)),
					 lien("Voir", "voir_comptes", "jamais"))
		},
		if (length(inactifs)) {
			list("moon", sprintf("%d compte%s sans activit\u00e9 depuis 2 semaines : %s", length(inactifs),
													 if (length(inactifs) > 1L) "s" else "", noms(inactifs)),
					 lien("Voir", "voir_comptes", "inactif"))
		},
		if (k$a_revoir > 0L) {
			list("chat-left-text", sprintf("%d question%s comment\u00e9e%s \u00e0 revoir", k$a_revoir,
																		 if (k$a_revoir > 1L) "s" else "", if (k$a_revoir > 1L) "s" else ""),
					 lien("Voir", "voir_questions"))
		},
		if (isTRUE(k$appli_nouveaux > 0L)) {
			list("chat-square-heart", sprintf("%d retour%s sur l'appli \u00e0 lire", k$appli_nouveaux,
																				if (k$appli_nouveaux > 1L) "s" else ""),
					 lien("Lire", "voir_appli"))
		},
		if (!llm_cle_disponible()) {
			list("robot", "IA \u00e9teinte : pas de cl\u00e9 OpenAI (OPENAI_API_KEY) sur le serveur. Les questions viennent de la banque.", NULL)
		}
	)
	items <- Filter(Negate(is.null), items)

	if (!length(items)) {
		return(shiny::tags$p(class = "cq-admin-ok", bsicons::bs_icon("check2-circle"), " Rien \u00e0 signaler."))
	}

	shiny::tags$ul(
		class = "cq-admin-alertes",
		lapply(items, function(x) {
			shiny::tags$li(bsicons::bs_icon(x[[1L]]), shiny::tags$span(x[[2L]]), x[[3L]])
		})
	)
}

#' Une ligne de la liste des comptes
#' @noRd
admin_ligne_compte <- function(l, ns) {
	alias <- gsub("[^a-z0-9_]", "", l$username)
	action <- function(quoi) {
		sprintf(
			"Shiny.setInputValue('%s', {quoi: '%s', alias: '%s'}, {priority: 'event'})",
			ns("action"), quoi, alias
		)
	}
	reussite <- l$taux_reussite
	# L'administrateur a toujours l'IA (ia_etat(admin = TRUE)).
	part_ia <- if (isTRUE(l$is_admin) || l$seuil_ia <= 0L) 1 else l$n_distinctes / l$seuil_ia
	statuts <- c(actif = "Actif", inactif = "Inactif (plus de 2 semaines)", jamais = "Aucune s\u00e9ance")

	shiny::div(
		class = paste0("cq-admin-ligne is-", l$statut),
		shiny::div(
			class = "cq-admin-compte",
			shiny::tags$span(class = "cq-admin-point", title = statuts[[l$statut]], `aria-label` = statuts[[l$statut]]),
			shiny::tags$span(class = "cq-admin-alias", l$username),
			if (isTRUE(l$is_admin)) shiny::tags$span(class = "cq-admin-pastille", "admin"),
			shiny::tags$span(
				class = "cq-admin-cree muted tiny",
				if (!is.na(l$cree_le)) paste("cr\u00e9\u00e9 le", format(as.POSIXct(l$cree_le, origin = "1970-01-01", tz = "Europe/Paris"), "%d/%m/%Y"))
			)
		),
		shiny::div(
			class = "cq-admin-cellule",
			`data-label` = "Derni\u00e8re activit\u00e9",
			shiny::tags$span(class = if (l$statut != "actif") "muted", admin_depuis(l$derniere))
		),
		shiny::div(
			class = "cq-admin-cellule",
			`data-label` = "Vers l'IA",
			admin_barre(part_ia, if (part_ia >= 1) "bon" else "ia"),
			shiny::tags$span(
				class = "muted tiny",
				if (isTRUE(l$is_admin)) {
					"IA toujours ouverte (admin)"
				} else if (part_ia >= 1) {
					sprintf("IA ouverte \u00b7 %d questions", l$n_distinctes)
				} else {
					sprintf("%d / %d questions", l$n_distinctes, l$seuil_ia)
				}
			)
		),
		shiny::div(
			class = "cq-admin-cellule",
			`data-label` = "R\u00e9ussite",
			if (is.na(reussite)) {
				shiny::tags$span(class = "muted", "\u2013")
			} else {
				shiny::tagList(
					admin_barre(reussite, admin_couleur_reussite(reussite)),
					shiny::tags$span(class = "tiny", sprintf("%d %% \u00b7 %d r\u00e9p.", round(100 * reussite), l$n_reponses))
				)
			}
		),
		shiny::div(
			class = "cq-admin-cellule",
			`data-label` = "S\u00e9ances",
			shiny::tags$span(l$n_seances)
		),
		shiny::div(
			class = "cq-admin-boutons",
			shiny::tags$button(
				type = "button", class = "cq-admin-icone",
				title = "Nouveau mot de passe", `aria-label` = paste("Nouveau mot de passe pour", l$username),
				onclick = action("mdp"),
				bsicons::bs_icon("key")
			),
			if (!isTRUE(l$is_admin)) {
				shiny::tags$button(
					type = "button", class = "cq-admin-icone is-danger",
					title = "Supprimer le compte", `aria-label` = paste("Supprimer le compte", l$username),
					onclick = action("suppr"),
					bsicons::bs_icon("trash")
				)
			} else {
				shiny::tags$span(
					class = "cq-admin-icone is-verrou",
					title = "Un compte administrateur ne se supprime pas depuis l'application.",
					bsicons::bs_icon("lock")
				)
			}
		)
	)
}

#' Adresse de l'application, pour le message à envoyer au nouveau compte
#' @noRd
admin_adresse <- function(session) {
	cd <- session$clientData
	tryCatch({
		port <- cd$url_port
		port <- if (is.null(port) || !nzchar(port) || port %in% c("80", "443")) "" else paste0(":", port)
		paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname)
	}, error = function(e) "")
}

#' Fenêtre du mot de passe généré (affiché une seule fois)
#'
#' @param alias Compte.
#' @param mdp Mot de passe en clair.
#' @param adresse Adresse de l'application.
#' @param nouveau Compte créé (sinon : mot de passe changé).
#' @noRd
admin_modal_mdp <- function(alias, mdp, adresse = "", nouveau = TRUE) {
	message <- paste0(
		"Bonjour ! Voici ton acc\u00e8s \u00e0 Candide",
		if (nzchar(adresse)) paste0(" (", adresse, ")") else "",
		".\nIdentifiant : ", alias,
		"\nMot de passe : ", mdp
	)
	copier <- function(texte, libelle) {
		shiny::tags$button(
			type = "button",
			class = "btn",
			`data-copie` = texte,
			onclick = paste0(
				"var b=this;navigator.clipboard.writeText(b.dataset.copie).then(function(){",
				"b.classList.add('is-copie');b.textContent='Copi\u00e9';});"
			),
			bsicons::bs_icon("clipboard"), paste0(" ", libelle)
		)
	}

	shiny::modalDialog(
		title = if (nouveau) paste("Compte", alias, "cr\u00e9\u00e9") else paste("Nouveau mot de passe pour", alias),
		shiny::div(
			class = "cq-admin-mdp",
			shiny::tags$span(class = "eyebrow", "Mot de passe"),
			shiny::tags$code(class = "cq-admin-mdp-valeur", mdp)
		),
		shiny::tags$p(
			class = "cq-admin-avertissement",
			bsicons::bs_icon("exclamation-triangle"),
			" Copiez-le maintenant : il ne sera plus jamais affich\u00e9."
		),
		shiny::div(
			class = "cq-admin-copies",
			copier(mdp, "Copier le mot de passe"),
			copier(message, "Copier le message \u00e0 envoyer")
		),
		shiny::tags$pre(class = "cq-admin-message", message),
		shiny::tags$p(class = "muted tiny", "Envoyez-le par un canal priv\u00e9 (SMS, messagerie), pas par un groupe."),
		footer = shiny::modalButton("J'ai copi\u00e9, fermer"),
		easyClose = FALSE
	)
}

#' Une carte de retour sur l'application (onglet « Retours appli »)
#'
#' @param l Une ligne de [retour_appli_lister()].
#' @param input_id Identifiant d'entrée du bouton « Traité ».
#' @noRd
admin_carte_appli <- function(l, input_id) {
	probleme <- identical(l$genre, "probleme")
	traite <- identical(l$statut, "traite")
	parties <- retour_appli_parties()
	partie <- if (identical(l$partie, "autre") && !is.na(l$partie_autre)) {
		paste0("Autre : ", l$partie_autre)
	} else {
		lib <- unname(parties[l$partie])
		if (length(lib) != 1L || is.na(lib)) l$partie else lib
	}
	ctx <- tryCatch(jsonlite::fromJSON(l$contexte %||% "{}"), error = function(e) list())
	ctx_txt <- paste(Filter(nzchar, c(
		if (nzchar(ctx$ecran %||% "")) paste("\u00e9cran", sub("x", "\u00d7", ctx$ecran)),
		ctx$navigateur %||% "",
		if (nzchar(ctx$theme %||% "")) paste("th\u00e8me", if (identical(ctx$theme, "dark")) "sombre" else "clair"),
		if (nzchar(ctx$section %||% "")) paste("ouvert depuis", ctx$section)
	)), collapse = " \u00b7 ")
	quand <- admin_temps(l$cree_le)

	shiny::div(
		class = paste("cq-appli", if (probleme) "is-probleme" else "is-suggestion", if (traite) "is-traite"),
		shiny::tags$span(
			class = "cq-appli-icone",
			title = if (probleme) "Probl\u00e8me" else "Id\u00e9e",
			bsicons::bs_icon(if (probleme) "bug" else "lightbulb")
		),
		shiny::div(
			class = "cq-appli-corps",
			shiny::div(
				class = "cq-appli-tete",
				shiny::tags$strong(if (probleme) "Quelque chose cloche" else "Une id\u00e9e"),
				shiny::tags$span(class = "cq-q-theme", partie),
				shiny::tags$span(class = "muted tiny", sprintf(
					"%s \u00b7 %s \u00b7 n\u00b0 %03d",
					l$username, admin_depuis(quand), l$id
				))
			),
			shiny::tags$p(class = "cq-appli-texte", l$texte),
			if (nzchar(ctx_txt)) shiny::tags$p(class = "cq-appli-contexte", bsicons::bs_icon("paperclip"), " ", ctx_txt)
		),
		shiny::tags$button(
			type = "button",
			class = paste("cq-q-traiter", if (traite) "is-traite"),
			onclick = sprintf(
				"Shiny.setInputValue('%s', {id: %d, statut: '%s'}, {priority: 'event'});",
				input_id, as.integer(l$id), if (traite) "nouveau" else "traite"
			),
			bsicons::bs_icon(if (traite) "arrow-counterclockwise" else "check2"),
			if (traite) " Rouvrir" else " Trait\u00e9"
		)
	)
}
