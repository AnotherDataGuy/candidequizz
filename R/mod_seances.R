# Section « Séances ».
#
# Conteneur à trois écrans : paramétrage, passation, bilan. Il détient la
# séance en cours et la fait circuler ; les sous-modules ne se connaissent pas
# entre eux et ne décident pas de la navigation.

#' Séances — interface
#'
#' @param id Identifiant du module.
#'
#' @return Un tag HTML.
#' @noRd
mod_seances_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-section-seances",
		quiz_loading_ui(ns("chargement")),
		shiny::conditionalPanel(
			condition = "output.vue == 'config'",
			ns = ns,
			mod_seance_config_ui(ns("config"))
		),
		shiny::conditionalPanel(
			condition = "output.vue == 'passation'",
			ns = ns,
			mod_seance_run_ui(ns("run"))
		),
		shiny::conditionalPanel(
			condition = "output.vue == 'avis'",
			ns = ns,
			mod_seance_avis_ui(ns("avis"))
		),
		shiny::conditionalPanel(
			condition = "output.vue == 'bilan'",
			ns = ns,
			mod_seance_bilan_ui(ns("bilan"))
		)
	)
}

#' Démarrer une séance
#'
#' Le plan et le premier lot sont produits avant d'afficher la passation : la
#' première question est ainsi prête quand l'écran s'affiche, et l'utilisateur
#' n'attend qu'une fois, à un moment qu'il a choisi.
#'
#' @param parametres Paramètres validés.
#'
#' @return Une séance, ou une condition d'erreur.
#' @noRd
seances_demarrer <- function(parametres, user_id = NULL) {
	resultat <- tryCatch(
		shiny::withProgress(
			message = "Pr\u00e9paration de votre s\u00e9ance",
			detail = "\u00c9criture et relecture des premi\u00e8res questions\u2026",
			value = 0.3,
			{
				ctx <- generer_lot(
					parametres = parametres,
					plan = NULL,
					lot = 1L,
					exclure_seed_ids = seance_seeds_a_eviter(user_id = user_id),
					historique = if (parametres_adaptatif_actif(parametres)) {
						list(
							reponses = store_reponses_historiques(user_id = user_id),
							reponses_session = list()
						)
					} else {
						NULL
					},
					cache = cache_parametres()
				)

				shiny::setProgress(value = 1)

				ctx
			}
		),
		error = function(e) e
	)

	if (inherits(resultat, "error")) {
		return(resultat)
	}

	cache_integrer(resultat$items)

	seance <- seance_ajouter_lot(
		seance_nouvelle(parametres = parametres, plan = resultat$plan),
		resultat
	)
	seance$user_id <- user_id
	seance
}

#' Séances — serveur
#'
#' @param id Identifiant du module.
#' @param reprendre Réactif portant l'identifiant d'une séance à reprendre.
#'
#' @return `NULL`, invisiblement.
#' @noRd
mod_seances_server <- function(id,
															 reprendre = shiny::reactive(NULL),
															 utilisateur = shiny::reactive(NULL),
															 donnees_maj = NULL,
															 theme_preselection = shiny::reactive(NULL),
															 apres_avis = function(n_reponses) NULL) {
	shiny::moduleServer(id, function(input, output, session) {
		vue <- shiny::reactiveVal("config")
		seance <- shiny::reactiveVal(NULL)
		corrections_differees <- shiny::reactiveVal(FALSE)
		preparation <- shiny::reactiveVal(NULL)

		output$vue <- shiny::renderText(vue())
		shiny::outputOptions(output, "vue", suspendWhenHidden = FALSE)

		mod_seance_config_server(
			"config",
			utilisateur = utilisateur,
			donnees_maj = if (is.null(donnees_maj)) shiny::reactive(0) else donnees_maj,
			theme_preselection = theme_preselection,
			demarrer = function(parametres, retour_differe = FALSE) {
				# Aucune g\u00e9n\u00e9ration (donc aucun appel payant) sans compte connect\u00e9.
				if (is.null(shiny::isolate(utilisateur()))) return(invisible(NULL))
				if (!is.null(preparation())) return(invisible(NULL))
				# Rythme des séances par compte (fct_anti_abus.R).
				u <- shiny::isolate(utilisateur())
				rythme <- anti_abus_seance_autorisee(u$user_id, admin = isTRUE(u$is_admin))
				if (!isTRUE(rythme$ok)) {
					shiny::showNotification(rythme$message, type = "warning", duration = 8)
					return(invisible(NULL))
				}
				token <- paste0(as.numeric(Sys.time()))
				preparation(list(parametres = parametres, retour_differe = retour_differe, token = token))
				session$sendCustomMessage("cq-quiz-loading", list(
					action = "show", id = session$ns("chargement"),
					readyId = session$ns("chargement_pret"), token = token))
			}
		)

		# Attendre l'accusé de rendu du navigateur : un simple renderUI serait
		# envoyé trop tard, après la génération synchrone. Pas de délai artificiel.
		shiny::observeEvent(input$chargement_pret, {
			p <- preparation()
			shiny::req(p, identical(input$chargement_pret, p$token), utilisateur())
			on.exit({
				preparation(NULL)
				session$onFlushed(function() {
					session$sendCustomMessage("cq-quiz-loading", list(action = "hide", focusId = "main"))
				}, once = TRUE)
			}, add = TRUE)
			u <- utilisateur()
			# Une seule préparation à la fois par compte, tous onglets confondus.
			if (!anti_abus_verrouiller(u$user_id)) {
				shiny::showNotification(
					"Une s\u00e9ance est d\u00e9j\u00e0 en pr\u00e9paration pour ce compte, dans un autre onglet.",
					type = "warning", duration = 8
				)
				return(invisible(NULL))
			}
			on.exit(anti_abus_liberer(u$user_id), add = TRUE)

			tryCatch({
				# Quota d'IA du compte et budget du jour : au-delà, la séance
				# démarre sans IA plutôt que de ne pas démarrer.
				parametres <- p$parametres
				if (!generation_degradee(parametres)) {
					ia <- anti_abus_ia_autorisee(u$user_id, admin = isTRUE(u$is_admin))
					if (!isTRUE(ia$ok)) {
						parametres$generation <- "ia_off"
						parametres$verifier_faits <- FALSE
						shiny::showNotification(ia$message, type = "warning", duration = 10)
					}
				}

				resultat <- seances_demarrer(
					parametres,
					user_id = if (is.null(u)) NULL else u$user_id
				)
				if (inherits(resultat, "error")) stop(resultat)
				store_sauver(resultat)
				corrections_differees(isTRUE(p$parametres$retour_differe) || isTRUE(p$retour_differe))
				seance(resultat)
				vue("passation")
			}, error = seance_run_signaler_echec)
		})

		mod_seance_run_server(
			"run",
			seance = seance,
			retour_differe = corrections_differees,
			terminer = function() {
				# « Vos avis » d'abord : le bilan n'ouvre qu'une fois chaque
				# question notée.
				s <- shiny::isolate(seance())
				shiny::removeModal()
				vue(if (length(s$reponses) && !avis_complets(s)) "avis" else "bilan")
			},
			utilisateur = utilisateur
		)

		mod_seance_avis_server(
			"avis",
			seance = seance,
			valider = function() {
				vue("bilan")
				# Jauge du carnet de progression : juste après les avis.
				apres_avis(length(shiny::isolate(seance())$reponses))
			}
		)

		mod_seance_bilan_server(
			"bilan",
			seance = seance,
			utilisateur = utilisateur,
			rejouer = function() {
				seance(NULL)
				vue("config")
			}
		)

		# Pr\u00e9venir l'accueil, la r\u00e9vision et la progression quand une s\u00e9ance
		# change d'\u00e9tat (cr\u00e9\u00e9e, termin\u00e9e, abandonn\u00e9e), pas \u00e0 chaque r\u00e9ponse.
		etat_connu <- NULL
		shiny::observeEvent(seance(), {
			s <- seance()
			cle <- paste(s$session_id, s$statut)

			if (!identical(cle, etat_connu)) {
				etat_connu <<- cle

				if (!is.null(donnees_maj)) {
					donnees_maj(shiny::isolate(donnees_maj()) + 1)
				}
			}
		}, ignoreNULL = TRUE)

		# F-06 : reprise d'une séance interrompue, demandée depuis l'accueil.
		shiny::observeEvent(reprendre(), {
			u <- utilisateur()
			reprise <- tryCatch(
				store_charger(reprendre(), user_id = if (is.null(u)) NULL else u$user_id),
				error = function(e) e
			)

			if (inherits(reprise, "error")) {
				shiny::showNotification(
					cq_message_utilisateur(reprise),
					type = "error",
					duration = 8
				)

				return(invisible(NULL))
			}

			reprise$chrono_marque_le <- Sys.time()
			corrections_differees(isTRUE(reprise$parametres$retour_differe))
			seance(reprise)
			vue(if (seance_closee(reprise)) "bilan" else "passation")
		}, ignoreNULL = TRUE)

		invisible(NULL)
	})
}
