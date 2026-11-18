# Carnet de progression « à débloquer ».
#
# La section Progression n'apparaît qu'après `progression.seuil_questions`
# réponses (inst/golem-config.yml). D'ici là, une jauge à gauche de l'écran
# indique combien de séances il reste ; elle revient après chaque séance, une
# fois les avis sur les questions validés.

#' Nombre de réponses nécessaires pour ouvrir le carnet
#' @noRd
progression_seuil <- function() {
	as.integer(cq_config("progression", "seuil_questions"))
}

#' État du carnet pour un compte
#'
#' @param user_id Identifiant du compte.
#'
#' @return Une liste : `n` (questions répondues, toutes séances confondues),
#'   `seuil`, `taille` (longueur de la dernière séance, pour estimer les
#'   séances restantes) et `debloque`.
#' @noRd
progression_etat <- function(user_id = NULL) {
	seuil <- progression_seuil()
	tableau <- tryCatch(store_lister(user_id), error = function(e) NULL)

	n <- if (is.null(tableau) || !nrow(tableau)) 0L else as.integer(sum(tableau$n_repondues, na.rm = TRUE))
	taille <- if (is.null(tableau) || !nrow(tableau)) NA_integer_ else tableau$nb_questions[[1L]]
	if (is.na(taille) || taille < 1L) taille <- 20L

	list(n = n, seuil = seuil, taille = as.integer(taille), debloque = n >= seuil)
}

# --- Rédaction par IA : deuxième surprise ----------------------------------
#
# L'option « Avec IA » de la composition d'une séance s'ouvre après
# `ia.seuil_questions_distinctes` questions DIFFÉRENTES (golem-config.yml).
# Compter des graines distinctes, et non des réponses, garantit que
# l'utilisateur a vraiment parcouru la banque (et noté autant de questions
# différentes) avant de passer aux questions réécrites par l'IA.

#' Nombre de questions différentes nécessaires pour ouvrir l'IA
#' @noRd
ia_seuil <- function() {
	as.integer(cq_config("ia", "seuil_questions_distinctes"))
}

#' État du déverrouillage de l'IA pour un compte
#'
#' @param user_id Identifiant du compte.
#' @param admin L'administrateur a toujours accès à l'IA.
#'
#' @return Une liste : `n` (graines distinctes répondues, questions passées
#'   et réponses trop rapides exclues), `seuil` et `debloque`.
#' @noRd
ia_etat <- function(user_id = NULL, admin = FALSE) {
	seuil <- ia_seuil()
	seances <- tryCatch(stats_charger(user_id = user_id), error = function(e) list())

	graines <- unlist(lapply(seances, function(s) {
		# Questions passées et réponses données trop vite (sans lecture
		# possible, voir anti_abus_reponse_comptee()) ne comptent pas.
		repondues <- Filter(
			function(r) !isTRUE(r$passee) && anti_abus_reponse_comptee(r),
			s$reponses %||% list()
		)
		vapply(repondues, function(r) as.character(r$seed_id %||% NA_character_), character(1))
	}), use.names = FALSE)

	n <- length(unique(graines[!is.na(graines) & nzchar(graines)]))

	list(n = n, seuil = seuil, debloque = isTRUE(admin) || n >= seuil)
}

# La fenêtre de la surprise et l'explication de l'IA sont dans fct_ia_info.R.

#' Séances restantes avant l'ouverture du carnet
#' @noRd
progression_seances_restantes <- function(n, seuil, taille) {
	as.integer(ceiling(max(seuil - n, 0L) / max(taille, 1L)))
}

#' Jauge du carnet de progression (panneau à gauche)
#'
#' @param etat Résultat de [progression_etat()].
#' @param gain Questions répondues pendant la séance qui vient de finir (mises
#'   en valeur dans la jauge).
#' @param ouvert Le seuil vient d'être franchi : version « récompense ».
#'
#' @return Un tag HTML.
#' @noRd
cq_jauge_ui <- function(etat, gain = 0L, ouvert = FALSE) {
	n <- min(etat$n, etat$seuil)
	gain <- min(max(as.integer(gain), 0L), n)
	avant <- n - gain
	pas <- etat$seuil / 10
	restantes <- progression_seances_restantes(etat$n, etat$seuil, etat$taille)

	# Dix cases : la part acquise avant la séance, puis le gain de la séance.
	cases <- lapply(seq_len(10L), function(i) {
		bas <- (i - 1L) * pas
		plein_avant <- min(max((avant - bas) / pas, 0), 1)
		plein_total <- min(max((n - bas) / pas, 0), 1)
		shiny::tags$span(
			class = "cq-jauge-case",
			style = sprintf("--avant:%.3f;--total:%.3f;--rang:%d", plein_avant, plein_total, i),
			shiny::tags$span(class = "cq-jauge-gain"),
			shiny::tags$span(class = "cq-jauge-acquis")
		)
	})

	fermer <- shiny::tags$button(
		type = "button",
		class = "cq-jauge-fermer",
		`aria-label` = "Fermer la jauge",
		onclick = "Shiny.setInputValue('jauge_fermer', Math.random(), {priority: 'event'})",
		"\u2715"
	)

	titre <- if (ouvert) {
		shiny::tags$h2(id = "cq-jauge-titre", "Votre carnet ", shiny::tags$em("est ouvert."))
	} else if (restantes <= 1L) {
		shiny::tags$h2(id = "cq-jauge-titre", shiny::tags$em("Plus qu'une s\u00e9ance"), " pour ouvrir votre carnet.")
	} else {
		shiny::tags$h2(
			id = "cq-jauge-titre",
			"Encore ", shiny::tags$em(paste(restantes, "s\u00e9ances")),
			" pour ouvrir votre carnet."
		)
	}

	shiny::tags$aside(
		class = paste("cq-jauge", if (ouvert) "is-ouvert"),
		role = "dialog",
		`aria-labelledby` = "cq-jauge-titre",
		shiny::tags$div(
			class = "cq-jauge-tete",
			shiny::tags$span(
				class = "cq-jauge-eyebrow",
				if (!ouvert) cq_jauge_cadenas(),
				"Carnet de progression"
			),
			fermer
		),
		if (ouvert) shiny::tags$span(class = "cq-jauge-tampon", `aria-hidden` = "true", "OUVERT"),
		titre,
		shiny::tags$div(
			class = "cq-jauge-cases",
			role = "img",
			`aria-label` = sprintf("%d questions sur %d", n, etat$seuil),
			cases
		),
		shiny::tags$div(
			class = "cq-jauge-chiffres",
			shiny::tags$span(shiny::tags$strong(n), sprintf(" / %d questions", etat$seuil)),
			if (gain > 0L) shiny::tags$span(class = "cq-jauge-plus", sprintf("+%d cette s\u00e9ance", gain))
		),
		if (ouvert) {
			shiny::tagList(
				shiny::tags$p(
					class = "cq-jauge-texte",
					"Vos courbes, vos th\u00e8mes fragiles et votre pr\u00e9paration \u00e0 l'examen vous attendent."
				),
				shiny::tags$div(
					class = "cq-jauge-actions",
					shiny::tags$button(
						type = "button",
						class = "btn primary",
						onclick = "Shiny.setInputValue('nav_statistiques', Math.random());",
						"Ouvrir mon carnet \u2192"
					)
				)
			)
		} else {
			shiny::tagList(
				shiny::tags$p(
					class = "cq-jauge-texte",
					sprintf(
						"\u00c9volution, points fragiles, pr\u00e9paration \u00e0 l'examen : tout se d\u00e9voile \u00e0 %d r\u00e9ponses. Une case = %s questions.",
						etat$seuil, format(pas)
					)
				),
				shiny::tags$div(
					class = "cq-jauge-actions",
					shiny::tags$button(
						type = "button",
						class = "textbtn",
						onclick = "Shiny.setInputValue('jauge_fermer', Math.random(), {priority: 'event'})",
						"Continuer"
					)
				)
			)
		}
	)
}

#' Petit cadenas de la jauge
#' @noRd
cq_jauge_cadenas <- function() {
	shiny::tags$svg(
		class = "cq-jauge-cadenas",
		viewBox = "0 0 24 24",
		`aria-hidden` = "true",
		shiny::tags$rect(x = "5", y = "11", width = "14", height = "10", rx = "2"),
		shiny::tags$path(d = "M8 11V8a4 4 0 0 1 8 0v3")
	)
}
