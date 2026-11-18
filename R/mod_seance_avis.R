# Écran « Vos avis » : entre la fin de la séance et le bilan.
# Chaque question NOUVELLE pour ce compte reçoit une note de difficulté (1 à
# 10, obligatoire) et, si l'on veut, un commentaire. Les questions déjà notées
# lors d'une séance précédente sont repliées en bas, avec l'ancienne note :
# on peut la changer, sans obligation. Le bilan ne s'ouvre qu'une fois toutes
# les questions nouvelles notées.
#
# Mise en page compacte : l'énoncé et la bonne réponse sur une ligne, les
# autres options et le commentaire derrière un repli (<details>).

#' Vos avis — interface
#'
#' @param id Identifiant du module.
#'
#' @return Un tag HTML.
#' @noRd
mod_seance_avis_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-avis",
		shiny::div(
			class = "cq-avis-entete",
			shiny::tags$p(class = "eyebrow muted", "Avant le bilan"),
			shiny::tags$h1("Vos avis sur les questions."),
			shiny::tags$p(
				class = "muted",
				"Notez la difficult\u00e9 de chaque question, de 1 (tr\u00e8s facile) \u00e0 10 (redoutable). ",
				"Un commentaire est le bienvenu : \u00e9nonc\u00e9 confus, r\u00e9ponse discutable, question trop facile\u2026 ",
				"Vos avis servent \u00e0 am\u00e9liorer la banque de questions."
			)
		),
		shiny::uiOutput(ns("liste")),
		shiny::div(
			class = "cq-actions cq-avis-actions",
			shiny::tags$span(class = "cq-avis-compteur", `aria-live` = "polite"),
			shiny::actionButton(
				ns("valider"),
				shiny::tagList("Valider mes avis et voir le bilan", bsicons::bs_icon("arrow-right")),
				class = "btn btn-primary cq-bouton-cta"
			)
		)
	)
}

#' Échelle de 1 à 10, présélectionnée si une note existe
#'
#' Les libellés des bornes encadrent l'échelle ; chaque case porte un liseré
#' coloré, du vert (facile) au rouge (redoutable), défini dans inst/app/www/css/10-avis.css.
#' Sert aussi à la fenêtre de notation pendant la séance (mod_seance_run.R).
#' @noRd
avis_echelle <- function(id, note = NA_integer_) {
	shiny::div(
		class = "segments cq-champ cq-avis-echelle",
		shiny::tags$span(class = "cq-avis-borne cq-avis-borne--min", "Tr\u00e8s facile"),
		shiny::radioButtons(
			id,
			label = NULL,
			choices = as.character(1:10),
			selected = if (is.na(note)) character(0) else as.character(note),
			inline = TRUE
		),
		shiny::tags$span(class = "cq-avis-borne cq-avis-borne--max", "Redoutable")
	)
}

#' Une fiche de l'écran « Vos avis »
#'
#' @param q Une question, telle que préparée par le serveur du module
#'   (voir [avis_questions()], plus `deja` et `note_ancienne`).
#' @param i Rang de la question dans la séance : il fixe les identifiants
#'   des entrées (`note_<i>`, `com_<i>`).
#' @param ns Fonction d'espace de noms du module.
#'
#' @return Un `<li>`.
#' @noRd
avis_fiche <- function(q, i, ns) {
	bonnes <- Filter(function(o) o$correcte, q$options)
	fausse <- Filter(function(o) o$choisie && !o$correcte, q$options)
	a_commentaire <- !is.na(q$commentaire) && nzchar(q$commentaire)

	shiny::tags$li(
		class = paste("cq-avis-item", if (isTRUE(q$deja)) "is-deja"),
		id = ns(paste0("item_", i)),
		`data-rang` = i,
		# Colonne gauche : la question, et de quoi s'en souvenir.
		shiny::div(
			class = "cq-avis-question",
			shiny::div(
				class = "cq-avis-tete",
				shiny::tags$span(class = "cq-avis-num", sprintf("N\u00b0 %02d", q$numero)),
				shiny::tags$span(
					class = paste("cq-avis-issue", if (q$passee) "is-passee" else if (q$is_correct) "is-juste" else "is-fausse"),
					if (q$passee) "pass\u00e9e" else if (q$is_correct) "juste" else "fausse"
				)
			),
			shiny::tags$p(class = "cq-avis-enonce", q$enonce),
			# La bonne réponse (et la réponse donnée si elle était fausse), sur une ligne.
			if (length(bonnes)) {
				shiny::tags$p(
					class = "cq-avis-reponse",
					shiny::tags$span(
						class = "cq-avis-bonne",
						"\u2713 ",
						paste(vapply(bonnes, `[[`, character(1), "texte"), collapse = " \u00b7 ")
					),
					if (length(fausse)) {
						shiny::tags$span(
							class = "cq-avis-votre",
							"Votre r\u00e9ponse : ",
							paste(vapply(fausse, `[[`, character(1), "texte"), collapse = " \u00b7 ")
						)
					}
				)
			},
			# Toutes les options, repliées.
			if (length(q$options) > 1L) {
				shiny::tags$details(
					class = "cq-avis-details",
					shiny::tags$summary("Voir toutes les r\u00e9ponses"),
					shiny::tags$ul(
						class = "cq-avis-options",
						lapply(q$options, function(o) {
							shiny::tags$li(
								class = paste(
									"cq-avis-option",
									if (o$correcte) "is-correcte",
									if (o$choisie && !o$correcte) "is-choisie-fausse"
								),
								shiny::tags$span(class = "cq-avis-lettre", o$lettre),
								shiny::tags$span(o$texte),
								if (o$correcte) shiny::tags$span(class = "visually-hidden", " (bonne r\u00e9ponse)"),
								if (o$choisie && !o$correcte) shiny::tags$span(class = "visually-hidden", " (votre r\u00e9ponse)")
							)
						})
					)
				)
			}
		),
		# Colonne droite : la note et, replié, le commentaire.
		shiny::div(
			class = "cq-avis-saisie",
			shiny::tags$p(class = "cq-avis-manque", role = "alert", "Note manquante"),
			if (isTRUE(q$deja)) {
				shiny::tags$p(
					class = "cq-avis-ancienne",
					sprintf("D\u00e9j\u00e0 not\u00e9e %d / 10. Changez-la si votre avis a chang\u00e9.", q$note_ancienne)
				)
			},
			avis_echelle(ns(paste0("note_", i)), q$note),
			shiny::tags$details(
				class = "cq-avis-com",
				open = if (a_commentaire) NA else NULL,
				shiny::tags$summary("+ Commentaire"),
				shiny::textAreaInput(
					ns(paste0("com_", i)),
					label = NULL,
					value = if (a_commentaire) q$commentaire else "",
					placeholder = "Facultatif : \u00e9nonc\u00e9 confus, r\u00e9ponse discutable\u2026",
					width = "100%",
					rows = 2
				)
			)
		)
	)
}

#' Vos avis — serveur
#'
#' @param id Identifiant du module.
#' @param seance `reactiveVal` de la séance.
#' @param valider Fonction appelée une fois les avis enregistrés.
#'
#' @return `NULL`, invisiblement.
#' @noRd
mod_seance_avis_server <- function(id, seance, valider) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Questions de la séance, marquées `deja` si le compte les a notées lors
		# d'une séance précédente (même graine, même énoncé : voir avis_cle()).
		# Ne se redessine que si l'on change de séance : redessiner effacerait
		# les notes en cours de saisie.
		questions <- shiny::reactive({
			s <- seance()
			shiny::req(s)
			anterieurs <- avis_anterieurs(s$user_id, sauf_session_id = s$session_id)

			lapply(avis_questions(s), function(q) {
				ancien <- anterieurs[[avis_cle(q$seed_id, q$enonce)]]
				q$deja <- !is.null(ancien)
				q$note_ancienne <- if (q$deja) ancien$note else NA_integer_
				q$commentaire_ancien <- if (q$deja) ancien$commentaire else NA_character_

				# Préremplir avec l'ancien avis, sauf s'il a déjà été noté pendant la séance.
				if (q$deja && is.na(q$note)) q$note <- ancien$note
				if (q$deja && is.na(q$commentaire)) q$commentaire <- ancien$commentaire
				q
			})
		}) |> shiny::bindEvent(seance()$session_id)

		output$liste <- shiny::renderUI({
			qs <- questions()
			deja <- vapply(qs, function(q) isTRUE(q$deja), logical(1))
			nouvelles <- which(!deja)
			anciennes <- which(deja)

			shiny::tagList(
				if (length(nouvelles)) {
					shiny::tags$ol(class = "cq-avis-liste", lapply(nouvelles, function(i) avis_fiche(qs[[i]], i, ns)))
				} else {
					shiny::tags$p(
						class = "cq-avis-vide muted",
						"Vous avez d\u00e9j\u00e0 not\u00e9 toutes les questions de cette s\u00e9ance. ",
						"Validez directement, ou revoyez vos notes ci-dessous."
					)
				},
				if (length(anciennes)) {
					shiny::tags$details(
						class = "cq-avis-deja",
						shiny::tags$summary(
							sprintf(
								"D\u00e9j\u00e0 not\u00e9es lors d'une s\u00e9ance pr\u00e9c\u00e9dente (%d) \u00b7 facultatif",
								length(anciennes)
							)
						),
						shiny::tags$ol(class = "cq-avis-liste", lapply(anciennes, function(i) avis_fiche(qs[[i]], i, ns)))
					)
				}
			)
		})

		shiny::observeEvent(input$valider, {
			s <- seance()
			shiny::req(s)
			qs <- questions()
			notes <- lapply(seq_along(qs), function(i) input[[paste0("note_", i)]])
			vide <- vapply(notes, function(n) is.null(n) || !nzchar(n), logical(1))
			deja <- vapply(qs, function(q) isTRUE(q$deja), logical(1))
			# Seules les questions nouvelles doivent être notées.
			manquantes <- which(vide & !deja)

			if (length(manquantes)) {
				# Marque les questions oubliées et amène l'écran sur la première.
				session$sendCustomMessage("cq-avis-manquantes", list(ids = ns(paste0("item_", manquantes))))
				shiny::showNotification(
					paste0(
						"Il manque une note pour ",
						if (length(manquantes) == 1L) "la question " else "les questions ",
						paste(vapply(qs[manquantes], `[[`, integer(1), "numero"), collapse = ", "),
						"."
					),
					type = "warning",
					duration = 6
				)
				return(invisible(NULL))
			}

			for (i in seq_along(qs)) {
				if (vide[[i]]) {
					next
				}

				commentaire <- trimws(input[[paste0("com_", i)]] %||% "")

				# Question déjà notée et avis inchangé : il est déjà enregistré dans
				# une séance précédente, ne pas le dupliquer.
				if (deja[[i]]) {
					ancien_com <- qs[[i]]$commentaire_ancien
					ancien_com <- if (is.na(ancien_com)) "" else trimws(ancien_com)
					if (identical(as.integer(notes[[i]]), as.integer(qs[[i]]$note_ancienne)) &&
						identical(commentaire, ancien_com)) {
						next
					}
				}

				s <- avis_enregistrer(
					s,
					qs[[i]]$item_id,
					note = notes[[i]],
					commentaire = commentaire,
					moment = "finale"
				)
			}
			s$avis_valides_le <- Sys.time()

			seance(s)
			store_sauver(s)
			valider()
		})

		invisible(NULL)
	})
}
