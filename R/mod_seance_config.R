# Paramétrage d'une séance.

#' Libellés des profils de séance
#' @return Un vecteur nommé.
#' @noRd
seance_config_profils <- function() {
	c(
		"Entra\u00eenement" = "libre",
		"R\u00e9vision cibl\u00e9e" = "revision_ciblee",
		"Examen blanc" = "examen_blanc"
	)
}

#' Icônes sprite des profils
#' @noRd
seance_config_profil_icones <- function() {
	c(
		libre = "i-spark",
		revision_ciblee = "i-flag",
		examen_blanc = "i-clock"
	)
}

#' Modes effectivement jouables
#' @return Un vecteur de caractères.
#' @noRd
seance_config_modes_jouables <- function() {
	modes <- cq_modes()

	modes[vapply(
		modes,
		function(mode) {
			types <- question_type_ids(mode = mode)

			length(types) > 0L && all(types %in% question_type_ids(jouable = TRUE))
		},
		logical(1)
	)]
}

#' Choix de nombre d'options
#' @return Un vecteur nommé.
#' @noRd
seance_config_choix_options <- function() {
	c(
		"2" = "2",
		"3" = "3",
		"4" = "4",
		"Vari\u00e9" = "varie"
	)
}

#' Choix de sous-thèmes, groupés par thème
#' @param bank Banque de graines.
#' @return Une liste nommée.
#' @noRd
seance_config_choix_themes <- function(bank = bank_load()) {
	referentiel <- bank$referentiel
	themes <- unique(referentiel$theme)

	stats::setNames(
		lapply(themes, function(theme) {
			lignes <- referentiel[referentiel$theme == theme, , drop = FALSE]
			stats::setNames(lignes$full_theme_id, lignes$sub_theme)
		}),
		themes
	)
}

#' Identifiants des grands thèmes, tous sélectionnés par défaut
#' @param bank Banque de graines.
#' @return Vecteur de `theme_id`.
#' @noRd
seance_config_theme_ids <- function(bank = bank_load()) {
	bank_themes(bank)$theme_id
}

#' Cartes illustrées pour choisir les grands thèmes
#' @param bank Banque de graines.
#' @return Liste `choiceNames`, `choiceValues`, `selected`.
#' @noRd
seance_config_theme_picker <- function(bank = bank_load()) {
	themes <- bank_themes(bank)

	choice_names <- lapply(seq_len(nrow(themes)), function(i) {
		id <- themes$theme_id[[i]]
		nom <- themes$theme[[i]]
		src <- cq_theme_art_src(theme_id = id, theme = nom, bank = bank)

		shiny::tagList(
			if (!is.null(src)) {
				shiny::tags$img(src = src, alt = "", loading = "lazy", decoding = "async")
			},
			shiny::tags$span(class = "jeu-theme-nom", nom),
			shiny::tags$span(
				class = "jeu-theme-sous",
				glue::glue("{themes$n_sous_themes[[i]]} sous-th\u00e8mes")
			),
			shiny::tags$span(class = "jeu-theme-coche", `aria-hidden` = "true")
		)
	})

	list(
		choiceNames = choice_names,
		choiceValues = themes$theme_id,
		selected = themes$theme_id
	)
}

#' Étendre des `theme_id` vers tous les `full_theme_id` correspondants
#' @param theme_ids Vecteur de grands thèmes.
#' @param bank Banque.
#' @return Vecteur de `full_theme_id`, ou `NULL` si tout le programme.
#' @noRd
seance_config_expand_theme_ids <- function(theme_ids, bank = bank_load()) {
	tous <- bank_themes(bank)$theme_id
	theme_ids <- unique(as.character(theme_ids %||% character()))

	if (!length(theme_ids) || setequal(theme_ids, tous)) {
		return(NULL)
	}

	ref <- bank$referentiel
	as.character(ref$full_theme_id[ref$theme_id %in% theme_ids])
}

#' Illustration d'un guichet (étape 1)
#'
#' SVG en ligne, lu dans `inst/app/www/illustrations/guichets/` : les traits
#' sont en `currentColor` et les aplats en variables CSS, si bien que le dessin
#' suit le thème clair, le thème sombre et le guichet choisi.
#'
#' @param profil `"libre"`, `"revision_ciblee"` ou `"examen_blanc"`.
#'
#' @return Une balise `span`, ou `NULL` si le fichier manque.
#' @noRd
seance_config_illustration <- function(profil) {
	chemin <- app_sys("app/www/illustrations/guichets", paste0(profil, ".svg"))

	if (!nzchar(chemin) || !file.exists(chemin)) {
		return(NULL)
	}

	shiny::tags$span(
		class = "jeu-guichet-visuel",
		`aria-hidden` = "true",
		shiny::HTML(paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
	)
}

#' Les trois guichets (un par profil) : numéro, consigne, règles du jeu
#'
#' @return Liste nommée par valeur de profil.
#' @noRd
seance_config_guichets <- function() {
	nb_examen <- as.integer(cq_config("seance", "examen_nb_questions"))
	limite <- as.integer(cq_config("seance", "examen_limite_s"))

	list(
		libre = list(
			num = "Guichet 1",
			desc = "Apprendre en se trompant, sans pression.",
			stats = list(c("Correction", "imm\u00e9diate"), c("Chrono", "aucun"), c("Th\u00e8mes", "au choix"))
		),
		revision_ciblee = list(
			num = "Guichet 2",
			desc = "Revenir l\u00e0 o\u00f9 \u00e7a coince.",
			stats = list(c("Correction", "imm\u00e9diate"), c("Chrono", "aucun"), c("Th\u00e8mes", "vos points faibles"))
		),
		examen_blanc = list(
			num = "Guichet 3",
			desc = "Les conditions du jour J.",
			stats = list(
				c("Questions", as.character(nb_examen)),
				c("Chrono", paste(limite %/% 60, "min")),
				c("R\u00e9ussite", "32 / 40")
			)
		)
	)
}

#' Durée estimée d'une séance (environ 45 secondes par question)
#' @noRd
seance_config_duree_estimee <- function(nb_questions) {
	paste0("\u2248 ", max(1L, round(as.integer(nb_questions) * 0.75)), " min")
}

#' Une étape du paramétrage séquentiel
#'
#' Une seule étape est visible à la fois (attribut `data-etape` du
#' conteneur, piloté par `seance-fiche.js`). Retour est dans l'en-tête de
#' l'étape ; Continuer et Commencer sont sous la fiche, à droite.
#'
#' @param num Numéro de l'étape (1 à 3).
#' @param titre Titre affiché.
#' @param ... Contenu.
#' @param total Nombre d'étapes.
#' @noRd
seance_config_etape <- function(num, titre, ..., total = 3L) {
	num <- as.integer(num)

	shiny::tags$section(
		class = "jeu-etape",
		`data-etape-num` = num,
		shiny::tags$header(
			class = "jeu-etape-tete",
			shiny::tags$span(class = "jeu-etape-num", num),
			shiny::tags$h2(titre, tabindex = "-1"),
			if (num > 1L) {
				shiny::tags$button(
					type = "button",
					class = "textbtn jeu-retour",
					`data-aller` = num - 1L,
					"\u2190 Retour"
				)
			}
		),
		...
	)
}

#' Fil des étapes : numéro, nom, choix retenu (rempli par le navigateur)
#' @noRd
seance_config_fil <- function() {
	etapes <- c("Guichet", "Longueur", "Th\u00e8mes")

	shiny::tags$ol(
		class = "jeu-fil",
		`aria-label` = "\u00c9tapes du param\u00e9trage",
		lapply(seq_along(etapes), function(i) {
			shiny::tags$li(shiny::tags$button(
				type = "button",
				`data-aller` = i,
				`aria-current` = if (i == 1L) "step" else "false",
				shiny::tags$span(class = "jeu-fil-num", i),
				shiny::tags$span(
					class = "jeu-fil-texte",
					shiny::tags$b(etapes[[i]]),
					shiny::tags$small(`data-resume` = i, "\u2014")
				)
			))
		})
	)
}

#' Interface du paramétrage
#'
#' Les identifiants d'entrée ne changent pas (`profil`, `nb_questions`,
#' `theme_ids`, `demarrer`, options avancées) : seul l'habillage évolue.
#'
#' @param id Identifiant du module.
#' @return Un élément d'interface.
#' @noRd
mod_seance_config_ui <- function(id) {
	ns <- shiny::NS(id)

	profils <- seance_config_profils()
	icones <- seance_config_profil_icones()
	guichets <- seance_config_guichets()
	nb_possibles <- as.character(cq_config("seance", "nb_questions_possibles"))
	picker <- seance_config_theme_picker()
	pas_examen <- sprintf("input['%s'] != 'examen_blanc'", ns("profil"))

	shiny::div(
		class = "cq-config cq-jeu",
		`data-etape` = "1",
		shiny::tags$header(
			class = "jeu-tete",
			shiny::tags$p(class = "eyebrow", "Le guichet des possibles"),
			shiny::tags$h1(
				"Composez votre s\u00e9ance,",
				shiny::tags$br(),
				shiny::tags$span(class = "serif", "\u00e0 votre rythme.")
			)
		),
		shiny::uiOutput(ns("avertissement")),
		seance_config_fil(),
		shiny::div(
			class = "jeu-plateau",
			shiny::div(
				class = "jeu-choix",

				# 1. Le guichet (profil)
				seance_config_etape(
					"1",
					"Choisissez votre guichet",
					shiny::div(
						class = "jeu-guichets",
						shiny::radioButtons(
							ns("profil"),
							label = NULL,
							choiceNames = lapply(names(profils), function(lab) {
								val <- unname(profils[[lab]])
								g <- guichets[[val]]
								shiny::tagList(
									shiny::tags$span(class = "jeu-guichet-voyant", shiny::tags$i(), shiny::tags$b("Ouvert")),
									shiny::tags$span(class = "jeu-guichet-num", g$num),
									shiny::tags$span(class = "jeu-guichet-icone", cq_icon(icones[[val]])),
									shiny::tags$strong(class = "jeu-guichet-titre", lab),
									shiny::tags$span(class = "jeu-guichet-desc", g$desc),
									seance_config_illustration(val),
									shiny::tags$span(
										class = "jeu-guichet-stats",
										lapply(g$stats, function(x) shiny::tags$span(shiny::tags$small(x[[1]]), shiny::tags$b(x[[2]])))
									)
								)
							}),
							choiceValues = unname(profils),
							selected = "libre"
						)
					),
					shiny::uiOutput(ns("note_profil"))
				),

				# 2. La longueur, puis les options avancées (inchangées)
				seance_config_etape(
					"2",
					"Combien de questions ?",
					shiny::conditionalPanel(
						condition = pas_examen,
						shiny::div(
							class = "jeu-longueurs",
							shiny::radioButtons(
								ns("nb_questions"),
								label = NULL,
								choiceNames = lapply(nb_possibles, function(n) {
									shiny::tagList(shiny::tags$b(n), shiny::tags$small(seance_config_duree_estimee(n)))
								}),
								choiceValues = nb_possibles,
								selected = "10",
								inline = TRUE
							)
						)
					),
					shiny::conditionalPanel(
						condition = sprintf("input['%s'] == 'examen_blanc'", ns("profil")),
						shiny::tags$p(
							class = "jeu-note",
							"Format officiel : 40 questions QCM sur tout le programme, chronom\u00e8tre et corrections \u00e0 la fin."
						)
					),
					# Questions rédigées par l'IA : une carte visible, hors des options
					# avancées. Verrouillée tant que le seuil de questions différentes
					# n'est pas atteint (ia_etat(), fct_progression.R) : elle montre
					# alors la progression vers l'ouverture.
					shiny::div(
						class = "cq-ia-carte",
						shiny::conditionalPanel(
							condition = sprintf("output['%s']", ns("ia_ouverte")),
							shiny::div(
								class = "cq-ia-carte-corps",
								shiny::tags$span(class = "cq-ia-carte-icone", bsicons::bs_icon("stars")),
								shiny::div(
									class = "cq-ia-carte-texte",
									shiny::tags$p(class = "cq-ia-carte-titre", "Questions r\u00e9dig\u00e9es par l'IA"),
									shiny::tags$p(
										class = "cq-ia-carte-desc",
										"De nouvelles formes de questions, r\u00e9\u00e9crites \u00e0 partir de la banque et v\u00e9rifi\u00e9es."
									),
									cq_ia_lien()
								),
								shiny::div(
									class = "segments cq-champ cq-ia-choix",
									shiny::radioButtons(
										ns("generation"),
										label = NULL,
										choices = c("Avec IA" = "ia_on", "Sans IA" = "ia_off"),
										selected = if (llm_par_defaut()) "ia_on" else "ia_off",
										inline = TRUE
									)
								)
							)
						),
						shiny::conditionalPanel(
							condition = sprintf("!output['%s']", ns("ia_ouverte")),
							shiny::uiOutput(ns("ia_verrou"))
						),
						# Version à télécharger : l'utilisateur colle sa propre clé
						# OpenAI (voir llm_cle_definir()). Jamais dans la version en ligne.
						shiny::conditionalPanel(
							condition = sprintf(
								"output['%s'] && input['%s'] == 'ia_on' && output['%s']",
								ns("ia_ouverte"), ns("generation"), ns("cle_manquante")
							),
							shiny::div(
								class = "cq-ia-cle",
								shiny::tags$p(
									class = "cq-ia-cle-titre",
									"Votre cl\u00e9 OpenAI"
								),
								shiny::tags$p(
									class = "cq-ia-cle-desc muted",
									"L'IA est factur\u00e9e sur votre compte OpenAI. ",
									"La cl\u00e9 reste en m\u00e9moire sur votre ordinateur tant que l'application est ouverte, et n'est envoy\u00e9e qu'\u00e0 OpenAI."
								),
								shiny::div(
									class = "cq-ia-cle-saisie",
									shiny::passwordInput(ns("cle_openai"), label = NULL, placeholder = "sk-...", width = "100%"),
									shiny::actionButton(ns("cle_utiliser"), "Utiliser cette cl\u00e9", class = "btn primary")
								),
								shiny::uiOutput(ns("cle_erreur")),
								shiny::actionButton(
									ns("cle_guide"),
									shiny::tagList(bsicons::bs_icon("question-circle"), " Obtenir une cl\u00e9 en 4 \u00e9tapes"),
									class = "cq-ia-lien"
								)
							)
						)
					),
					bslib::accordion(
						id = ns("avance"),
						open = FALSE,
						class = "cq-config-avance jeu-avance",
						bslib::accordion_panel(
							title = "Options avanc\u00e9es",
							value = "avance",
							shiny::uiOutput(ns("avance_resume")),
							shiny::div(
								class = "field-row field-row--session",
								shiny::conditionalPanel(
									condition = sprintf(
										"input['%s'] != 'examen_blanc' && (input['%s'] == 'ia_off' || (function(t){if(!t) return false; if(typeof t === 'string') t = [t]; return t.indexOf('qcm_single') >= 0 || t.indexOf('qcm_multiple') >= 0;})(input['%s']))",
										ns("profil"),
										ns("generation"),
										ns("types")
									),
									shiny::div(
										class = "field",
										shiny::tags$label("Propositions"),
										shiny::div(
											class = "segments cq-champ",
											shiny::radioButtons(
												ns("n_options"),
												label = NULL,
												choices = seance_config_choix_options(),
												selected = "4",
												inline = TRUE
											)
										)
									)
								),
								shiny::conditionalPanel(
									condition = pas_examen,
									shiny::div(
										class = "field",
										shiny::tags$label("Correction"),
										shiny::div(
											class = "segments cq-champ",
											shiny::radioButtons(
												ns("retour_differe"),
												label = NULL,
												choices = c("\u00c0 chaque question" = "immediat", "\u00c0 la fin" = "fin"),
												selected = "immediat",
												inline = TRUE
											)
										)
									)
								),
								shiny::conditionalPanel(
									condition = sprintf(
										"input['%s'] != 'examen_blanc' && input['%s'] != 'ia_off'",
										ns("profil"),
										ns("generation")
									),
									shiny::div(
										class = "field",
										shiny::tags$label("Langue"),
										shiny::div(
											class = "segments cq-champ",
											shiny::radioButtons(
												ns("langue"),
												label = NULL,
												choices = c("Habituelle" = "courant", "Simple" = "simple", "Administrative" = "administratif"),
												selected = "courant",
												inline = TRUE
											)
										)
									)
								),
								shiny::div(
									class = "field",
									shiny::tags$label("Noter la difficult\u00e9"),
									shiny::div(
										class = "segments cq-champ",
										shiny::radioButtons(
											ns("avis_moment"),
											label = NULL,
											choices = c("\u00c0 la fin" = "fin", "Pendant et \u00e0 la fin" = "pendant"),
											selected = "fin",
											inline = TRUE
										)
									)
								),
								shiny::conditionalPanel(
									condition = pas_examen,
									shiny::div(
										class = "field",
										shiny::tags$label("Nom de la s\u00e9ance"),
										shiny::textInput(ns("libelle"), label = NULL, placeholder = "Facultatif", width = "100%")
									)
								)
							),
							# Types de questions : seulement avec IA. La banque ne contient que
							# des QCM à réponse unique ; les autres types sont des réécritures
							# de la graine par le rédacteur.
							shiny::conditionalPanel(
								condition = sprintf("%s && input['%s'] != 'ia_off'", pas_examen, ns("generation")),
								shiny::div(
									class = "field field--types cq-types",
									shiny::tags$label("Types de questions"),
									shiny::checkboxGroupInput(
										ns("types"),
										label = NULL,
										choiceNames = lapply(question_type_ids(), function(id) {
											shiny::tags$span(class = "cq-type-copy", shiny::tags$strong(question_type(id)$libelle))
										}),
										choiceValues = question_type_ids(),
										selected = "qcm_single",
										width = "100%"
									)
								)
							)
						)
					)
				),

				# 3. Les thèmes : au choix (entraînement), automatiques (révision),
				#    tout le programme (examen blanc)
				seance_config_etape(
					"3",
					"Vos th\u00e8mes",
					shiny::conditionalPanel(
						condition = sprintf("input['%s'] == 'libre'", ns("profil")),
						shiny::div(
							class = "jeu-themes-barre",
							shiny::uiOutput(ns("compte_themes"), inline = TRUE),
							shiny::tags$span(
								class = "jeu-themes-actions",
								shiny::actionLink(ns("themes_tous"), "Tous"),
								shiny::actionLink(ns("themes_aucun"), "Aucun")
							)
						),
						shiny::div(
							class = "jeu-themes",
							shiny::checkboxGroupInput(
								ns("theme_ids"),
								label = NULL,
								choiceNames = picker$choiceNames,
								choiceValues = picker$choiceValues,
								selected = picker$selected
							)
						)
					),
					shiny::conditionalPanel(
						condition = sprintf("input['%s'] == 'revision_ciblee'", ns("profil")),
						shiny::tags$p(
							class = "jeu-note",
							"Les sous-th\u00e8mes sont choisis d'apr\u00e8s vos r\u00e9ponses pass\u00e9es."
						),
						shiny::uiOutput(ns("terrain_revision"))
					),
					shiny::conditionalPanel(
						condition = sprintf("input['%s'] == 'examen_blanc'", ns("profil")),
						shiny::tags$p(
							class = "jeu-note",
							"Tout le programme, comme le jour de l'examen."
						)
					)
				),
				shiny::uiOutput(ns("garde_fou"))
			),

			# La fiche de la séance, mise à jour en direct
			shiny::tags$aside(
				class = "jeu-ticket-zone",
				shiny::uiOutput(ns("recap_live")),
				shiny::tags$button(
					type = "button",
					class = "btn primary jeu-suivant",
					`data-aller` = "2",
					shiny::tags$span(class = "jeu-suivant-texte", "Continuer"),
					cq_icon("i-arrow")
				),
				shiny::actionButton(
					ns("demarrer"),
					label = shiny::tagList("Commencer la s\u00e9ance", cq_icon("i-arrow")),
					class = "btn primary jeu-prendre"
				),
				shiny::uiOutput(ns("cta_etat"))
			)
		)
	)
}

#' Serveur du paramétrage
#' @param id Identifiant du module.
#' @param demarrer Fonction appelée avec les paramètres validés.
#' @return Rien.
#' @noRd
mod_seance_config_server <- function(id,
																		 demarrer,
																		 utilisateur = shiny::reactive(NULL),
																		 donnees_maj = shiny::reactive(0),
																		 theme_preselection = shiny::reactive(NULL)) {
	shiny::moduleServer(id, function(input, output, session) {
		jouables <- seance_config_modes_jouables()

		output$avertissement <- shiny::renderUI({
			# Version à télécharger : pas de bandeau, le champ de clé s'affiche
			# quand l'utilisateur choisit « Avec IA ».
			if (llm_cle_disponible() || llm_cle_saisie_permise()) {
				return(NULL)
			}

			shiny::div(
				class = "cq-config-banner cq-config-banner--warn",
				role = "status",
				shiny::tags$span(class = "cq-config-banner-kicker", "Mode d\u00e9grad\u00e9"),
				shiny::tags$p(
					"Aucune cl\u00e9 d'API OpenAI n'a \u00e9t\u00e9 trouv\u00e9e. La s\u00e9ance reste jouable, ",
					"mais les questions seront servies telles quelles depuis la banque."
				)
			)
		})

		# Rédaction par IA : verrouillée jusqu'au seuil de questions différentes
		# (ia_etat(), fct_progression.R). Recalculée après chaque séance.
		ia <- shiny::reactive({
			donnees_maj()
			u <- utilisateur()
			if (is.null(u)) {
				return(list(n = 0L, seuil = ia_seuil(), debloque = FALSE))
			}
			ia_etat(u$user_id, admin = isTRUE(u$is_admin))
		})
		ia_ouverte <- shiny::reactive(isTRUE(ia()$debloque))

		output$ia_ouverte <- shiny::reactive(ia_ouverte())
		shiny::outputOptions(output, "ia_ouverte", suspendWhenHidden = FALSE)

		# Clé collée dans l'application (version à télécharger seulement).
		# `cle_maj` signale aux réactifs qu'une clé vient d'être enregistrée.
		cle_maj <- shiny::reactiveVal(0L)
		cle_erreur <- shiny::reactiveVal(NULL)

		cle_manquante <- shiny::reactive({
			cle_maj()
			llm_cle_saisie_permise() && !llm_cle_disponible()
		})
		output$cle_manquante <- cle_manquante
		shiny::outputOptions(output, "cle_manquante", suspendWhenHidden = FALSE)

		shiny::observeEvent(input$cle_utiliser, {
			tryCatch(
				{
					llm_cle_definir(input$cle_openai)
					cle_erreur(NULL)
					cle_maj(cle_maj() + 1L)
					shiny::showNotification(
						"Cl\u00e9 enregistr\u00e9e pour cette session. L'IA est pr\u00eate.",
						type = "message"
					)
				},
				error = function(e) cle_erreur(cq_message_utilisateur(e))
			)
			# Ne pas laisser la clé dans le champ.
			shiny::updateTextInput(session, "cle_openai", value = "")
		})

		output$cle_erreur <- shiny::renderUI({
			message <- cle_erreur()
			if (is.null(message)) {
				return(NULL)
			}
			shiny::tags$p(class = "cq-ia-cle-erreur", role = "alert", message)
		})

		shiny::observeEvent(input$cle_guide, {
			shiny::showModal(cq_cle_guide_modal())
		})

		# Verrouillée : forcer « Sans IA » (le choix par défaut peut être
		# « Avec IA » dans la version en ligne).
		shiny::observe({
			if (!ia_ouverte()) {
				shiny::updateRadioButtons(session, "generation", selected = "ia_off")
			}
		})

		output$ia_verrou <- shiny::renderUI({
			# Carte verrouillée : même place que le choix Avec / Sans IA, avec la
			# progression vers l'ouverture.
			etat <- ia()
			reste <- max(etat$seuil - etat$n, 0L)
			part <- min(etat$n / max(etat$seuil, 1L), 1)
			shiny::div(
				class = "cq-ia-carte-corps is-verrouillee",
				shiny::tags$span(class = "cq-ia-carte-icone", bsicons::bs_icon("lock")),
				shiny::div(
					class = "cq-ia-carte-texte",
					shiny::tags$p(class = "cq-ia-carte-titre", "Questions r\u00e9dig\u00e9es par l'IA"),
					shiny::tags$p(
						class = "cq-ia-carte-desc",
						sprintf(
							"S'ouvre apr\u00e8s %d questions diff\u00e9rentes. Encore %d : chaque nouvelle question compte.",
							etat$seuil, reste
						)
					),
					shiny::div(
						class = "cq-ia-progres",
						role = "progressbar",
						`aria-valuemin` = 0, `aria-valuemax` = etat$seuil, `aria-valuenow` = etat$n,
						`aria-label` = "Questions diff\u00e9rentes r\u00e9pondues",
						shiny::tags$span(style = sprintf("width: %.1f%%", 100 * part))
					),
					shiny::tags$p(class = "cq-ia-progres-chiffres", sprintf("%d / %d", etat$n, etat$seuil)),
					cq_ia_lien()
				)
			)
		})

		ciblee <- shiny::reactive({
			donnees_maj()
			u <- utilisateur()
			revision_preparer(seances = stats_charger(user_id = if (is.null(u)) NULL else u$user_id))
		})

		output$note_profil <- shiny::renderUI({
			if (identical(input$profil, "examen_blanc")) {
				limite <- as.integer(cq_config("seance", "examen_limite_s"))
				nb_examen <- as.integer(cq_config("seance", "examen_nb_questions"))

				return(shiny::div(
					class = "cq-config-callout cq-config-callout--info",
					shiny::tags$strong("Conditions d'examen"),
					shiny::tags$p(
						glue::glue(
							"{nb_examen} questions QCM, tout le programme, corrections \u00e0 la fin, ",
							"{limite %/% 60} minutes au chronom\u00e8tre."
						)
					)
				))
			}

			if (!identical(input$profil, "revision_ciblee")) {
				return(NULL)
			}

			prep <- ciblee()

			if (!isTRUE(prep$ok)) {
				return(shiny::div(
					class = "cq-config-callout cq-config-callout--locked",
					shiny::tags$strong("Pas encore de bilan"),
					shiny::tags$p(
						"Jouez d'abord une s\u00e9ance d'entra\u00eenement : la r\u00e9vision s'appuie sur vos r\u00e9sultats."
					)
				))
			}

			shiny::div(
				class = "cq-config-callout cq-config-callout--ok",
				shiny::tags$strong("Sous-th\u00e8mes retenus"),
				shiny::tags$ul(
					class = "cq-config-chip-list",
					lapply(seq_len(nrow(prep$apercu)), function(i) {
						shiny::tags$li(
							glue::glue(
								"{prep$apercu$sub_theme[[i]]} \u00b7 {round(100 * prep$apercu$reussite[[i]])} %"
							)
						)
					})
				)
			)
		})

		output$terrain_revision <- shiny::renderUI({
			prep <- ciblee()

			if (!isTRUE(prep$ok)) {
				return(shiny::div(
					class = "cq-config-empty",
					shiny::tags$strong("Terrain en attente"),
					shiny::tags$p(
						class = "muted",
						"D\u00e8s qu'un entra\u00eenement a \u00e9t\u00e9 jou\u00e9, les th\u00e8mes fragiles appara\u00eetront ici."
					)
				))
			}

			shiny::div(
				class = "cq-config-auto-themes",
				lapply(seq_len(nrow(prep$apercu)), function(i) {
					pct <- round(100 * prep$apercu$reussite[[i]])
					shiny::div(
						class = "cq-config-auto-theme",
						shiny::tags$strong(prep$apercu$sub_theme[[i]]),
						shiny::tags$span(
							class = "cq-config-auto-pct",
							glue::glue("{pct} %")
						),
						shiny::tags$span(
							class = "cq-config-auto-bar",
							`aria-hidden` = "true",
							shiny::tags$i(style = paste0("width:", pct, "%"))
						)
					)
				})
			)
		})

		# Th\u00e8me choisi depuis le carrousel de l'accueil.
		shiny::observeEvent(theme_preselection(), {
			choix <- theme_preselection()
			shiny::req(choix$theme_id)
			shiny::updateRadioButtons(session, "profil", selected = "libre")
			shiny::updateCheckboxGroupInput(session, "theme_ids", selected = choix$theme_id)
			session$sendCustomMessage("cq-config-etape", 3L)
		}, ignoreNULL = TRUE)

		shiny::observeEvent(input$themes_tous, {
			shiny::updateCheckboxGroupInput(session, "theme_ids", selected = seance_config_theme_ids())
		})

		shiny::observeEvent(input$themes_aucun, {
			shiny::updateCheckboxGroupInput(session, "theme_ids", selected = character(0))
		})

		# Types effectivement demandés. Sans IA, seul le QCM de la banque existe :
		# le choix de types (masqué) est ignoré.
		types_choisis <- shiny::reactive({
			if (identical(input$generation, "ia_off")) {
				return("qcm_single")
			}

			as.character(input$types %||% character())
		})

		output$avance_resume <- shiny::renderUI({
			if (identical(input$profil, "examen_blanc")) {
				return(shiny::tags$p(class = "cq-config-avance-resume muted", "R\u00e9glages fix\u00e9s par l'examen blanc."))
			}

			types <- types_choisis()
			morceaux <- c(
				if (length(types)) {
					paste(vapply(types, function(id) question_type(id)$libelle, character(1)), collapse = ", ")
				} else {
					"aucun type choisi"
				},
				if (any(types %in% c("qcm_single", "qcm_multiple"))) {
					if (identical(input$n_options, "varie")) "propositions vari\u00e9es" else paste(input$n_options %||% "4", "propositions")
				},
				if (identical(input$retour_differe, "fin")) "correction \u00e0 la fin" else "correction \u00e0 chaque question",
				if (identical(input$generation, "ia_off")) "sans IA" else "avec IA"
			)

			shiny::tags$p(class = "cq-config-avance-resume muted", paste(morceaux, collapse = " \u00b7 "))
		})

		# Numéro affiché sur la fiche : la prochaine séance de ce compte.
		numero_seance <- shiny::reactive({
			donnees_maj()
			u <- utilisateur()
			n <- tryCatch(
				nrow(store_lister(user_id = if (is.null(u)) NULL else u$user_id)),
				error = function(e) 0L
			)
			sprintf("%03d", as.integer(n) + 1L)
		})

		output$compte_themes <- shiny::renderUI({
			shiny::tags$span(
				class = "jeu-compte",
				shiny::tags$b(length(input$theme_ids)),
				glue::glue(" / {length(seance_config_theme_ids())} th\u00e8mes")
			)
		})

		output$recap_live <- shiny::renderUI({
			profil <- input$profil %||% "libre"
			examen <- identical(profil, "examen_blanc")
			profil_lab <- names(seance_config_profils())[match(profil, seance_config_profils())]
			guichet <- seance_config_guichets()[[profil]]

			nb <- if (examen) {
				as.character(cq_config("seance", "examen_nb_questions"))
			} else {
				as.character(input$nb_questions %||% "10")
			}
			duree <- if (examen) {
				paste(as.integer(cq_config("seance", "examen_limite_s")) %/% 60, "min")
			} else {
				seance_config_duree_estimee(nb)
			}
			themes_lab <- if (examen) {
				"tout le programme"
			} else if (identical(profil, "revision_ciblee")) {
				"vos points faibles"
			} else if (!length(input$theme_ids) || length(input$theme_ids) >= length(seance_config_theme_ids())) {
				# Aucune case cochée = aucun filtre : tous les thèmes.
				"tous"
			} else {
				glue::glue("{length(input$theme_ids)} / {length(seance_config_theme_ids())}")
			}
			corr_lab <- if (examen || identical(input$retour_differe, "fin")) "\u00e0 la fin" else "imm\u00e9diate"
			pret <- is.null(motif_blocage())

			shiny::div(
				class = paste("jeu-ticket", if (!pret) "is-incomplet"),
				`aria-live` = "polite",
				shiny::div(
					class = "jeu-ticket-haut",
					shiny::tags$span("Candide au guichet"),
					shiny::tags$span(paste0("S\u00e9ance n\u00b0 ", numero_seance()))
				),
				shiny::div(
					class = "jeu-ticket-corps",
					shiny::tags$span(class = "jeu-ticket-guichet", guichet$num),
					shiny::tags$strong(class = "jeu-ticket-titre", profil_lab),
					shiny::tags$dl(
						shiny::tags$dt("Questions"), shiny::tags$dd(nb),
						shiny::tags$dt("Dur\u00e9e"), shiny::tags$dd(duree),
						shiny::tags$dt("Th\u00e8mes"), shiny::tags$dd(themes_lab),
						shiny::tags$dt("Correction"), shiny::tags$dd(corr_lab)
					)
				),
				shiny::div(class = "jeu-ticket-coupe", `aria-hidden` = "true"),
				shiny::div(
					class = "jeu-ticket-talon",
					shiny::tags$span(class = "jeu-ticket-numero", numero_seance()),
					shiny::tags$span(
						class = "jeu-ticket-talon-texte",
						"Votre", shiny::tags$br(), "s\u00e9ance"
					)
				),
				shiny::tags$span(
					class = "jeu-tampon",
					`aria-hidden` = "true",
					if (pret) "Pr\u00eat" else "Incomplet"
				)
			)
		})

		parametres <- shiny::reactive({
			cle_maj()
			prep <- ciblee()
			revision <- identical(input$profil, "revision_ciblee")
			types <- types_choisis()
			mode <- if (identical(types, "qcm_single")) "qcm_classique" else "autres_types"

			tryCatch(
				parametres_seance(
					nb_questions = as.integer(input$nb_questions %||% 10L),
					mode = mode,
					n_options = if (identical(input$n_options, "varie")) {
						"varie"
					} else {
						as.integer(input$n_options %||% 4L)
					},
					types = if (identical(mode, "qcm_classique")) NULL else types,
					full_theme_ids = if (revision) {
						prep$full_theme_ids
					} else {
						seance_config_expand_theme_ids(input$theme_ids)
					},
					profil = input$profil %||% "libre",
					poids_themes = if (revision) prep$poids_themes else NULL,
					retour_differe = identical(input$retour_differe, "fin"),
					avis_pendant = identical(input$avis_moment, "pendant"),
					# Côté serveur aussi : pas d'IA tant qu'elle est verrouillée.
					generation = if (ia_ouverte()) input$generation %||% "auto" else "ia_off",
					langue = input$langue %||% "courant",
					libelle = input$libelle
				),
				error = function(e) cq_message_utilisateur(e)
			)
		})

		motif_blocage <- shiny::reactive({
			if (identical(input$profil, "revision_ciblee") && !isTRUE(ciblee()$ok)) {
				return("Jouez d'abord une s\u00e9ance d'entra\u00eenement pour d\u00e9bloquer la r\u00e9vision cibl\u00e9e.")
			}

			if (!identical(input$profil, "examen_blanc")) {
				types <- types_choisis()

				if (!length(types)) {
					return("Choisissez au moins un type de question.")
				}

				if (!all(types %in% question_type_ids(jouable = TRUE))) {
					return("Un type choisi n'a pas encore d'\u00e9cran de r\u00e9ponse.")
				}
			}

			if (is.character(parametres())) {
				return(parametres())
			}

			cle_maj()
			if (identical(input$generation, "ia_on") && ia_ouverte() &&
				llm_cle_saisie_permise() && !llm_cle_disponible()) {
				return("Collez votre cl\u00e9 OpenAI pour utiliser l'IA, ou choisissez \u00ab Sans IA \u00bb.")
			}

			NULL
		})

		output$garde_fou <- shiny::renderUI({
			motif <- motif_blocage()

			if (is.null(motif)) {
				return(NULL)
			}

			shiny::div(
				class = "cq-config-banner cq-config-banner--block",
				role = "alert",
				shiny::tags$span(class = "cq-config-banner-kicker", "Action bloqu\u00e9e"),
				shiny::tags$p(motif)
			)
		})

		output$cta_etat <- shiny::renderUI({
			motif <- motif_blocage()

			if (is.null(motif)) {
				return(shiny::tags$span(
					class = "cq-config-cta-ready muted",
					"Pr\u00eat \u00e0 d\u00e9marrer"
				))
			}

			shiny::tags$span(
				class = "cq-config-cta-blocked",
				"Indisponible pour l'instant"
			)
		})

		shiny::observe({
			bloque <- !is.null(motif_blocage())
			shiny::updateActionButton(
				session,
				"demarrer",
				disabled = bloque
			)
		})

		shiny::observeEvent(input$demarrer, {
			motif <- motif_blocage()

			if (!is.null(motif)) {
				shiny::showNotification(motif, type = "warning", duration = 6)
				return(invisible(NULL))
			}

			# Le r\u00e9capitulatif est d\u00e9j\u00e0 affich\u00e9 : on d\u00e9marre directement,
			# sauf pour l'examen blanc dont le chronom\u00e8tre part tout de suite.
			if (!identical(input$profil, "examen_blanc")) {
				demarrer(
					parametres = parametres(),
					retour_differe = identical(parametres()$retour_differe, TRUE)
				)
				return(invisible(NULL))
			}

			p <- parametres()
			themes <- if (is.null(p$full_theme_ids)) {
				"tout le programme"
			} else {
				glue::glue("{length(p$full_theme_ids)} sous-th\u00e8me(s) s\u00e9lectionn\u00e9(s)")
			}
			profil <- names(seance_config_profils())[match(p$profil, seance_config_profils())]

			shiny::showModal(shiny::modalDialog(
				title = "Pr\u00eat pour l'examen blanc ?",
				shiny::tags$ul(
					shiny::tags$li(glue::glue("Objectif : {profil}")),
					shiny::tags$li(glue::glue("{p$nb_questions} questions")),
					shiny::tags$li(glue::glue(
						"R\u00e9daction : {if (identical(p$generation, 'ia_off')) 'sans IA' else 'avec IA'}"
					)),
					shiny::tags$li(glue::glue(
						"Types : {paste(vapply(p$types, function(id) question_type(id)$libelle, character(1)), collapse = ', ')}"
					)),
					if (!identical(p$langue, "courant") && !identical(p$generation, "ia_off")) {
						shiny::tags$li(glue::glue("Langue : {p$langue}"))
					},
					if (!is.null(p$libelle)) {
						shiny::tags$li(glue::glue("Nom : {p$libelle}"))
					},
					shiny::tags$li(glue::glue("Programme : {themes}")),
					shiny::tags$li(if (isTRUE(p$avis_pendant)) {
						"Difficult\u00e9 not\u00e9e pendant la s\u00e9ance, puis confirm\u00e9e \u00e0 la fin"
					} else {
						"Difficult\u00e9 de chaque question not\u00e9e \u00e0 la fin"
					}),
					shiny::tags$li(if (isTRUE(p$retour_differe)) {
						"Corrections affich\u00e9es \u00e0 la fin"
					} else {
						"Correction affich\u00e9e apr\u00e8s chaque question"
					}),
					if (seance_chrono_actif(list(parametres = p))) {
						shiny::tags$li(glue::glue(
							"Chronom\u00e8tre : {seance_chrono_texte(p$limite_s)}"
						))
					}
				),
				footer = shiny::tagList(
					shiny::modalButton("Annuler"),
					shiny::actionButton(
						session$ns("confirmer"),
						shiny::tagList("C'est parti", cq_icon("i-arrow")),
						class = "btn primary"
					)
				),
				easyClose = TRUE
			))
		})

		shiny::observeEvent(input$confirmer, {
			shiny::removeModal()
			demarrer(
				parametres = parametres(),
				retour_differe = identical(parametres()$retour_differe, TRUE)
			)
		})

		invisible(NULL)
	})
}
