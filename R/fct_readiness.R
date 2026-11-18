# Indicateur prudent de préparation à l'examen.
#
# Sans calibration psychométrique des items ni résultat réel d'examen, une
# probabilité de réussite serait trompeuse. Cet indicateur ne décrit donc que
# les examens blancs complets enregistrés sur l'appareil.

#' Examens blancs complets utilisables comme preuves
#'
#' @param seances Liste de séances.
#' @param n_questions Format attendu de l'examen.
#'
#' @return Une liste de séances complètes, de la plus récente à la plus ancienne.
#' @noRd
readiness_examens_valides <- function(
		seances,
		n_questions = cq_config("seance", "examen_nb_questions")) {
	Filter(function(s) {
		identical(as.character(s$parametres$profil %||% ""), "examen_blanc") &&
			identical(as.character(s$statut %||% ""), "terminee") &&
			length(s$reponses %||% list()) == as.integer(n_questions)
	}, seances)
}

#' Résumer le niveau de preuve fourni par les examens blancs
#'
#' @param seances Liste de séances.
#' @param n_questions Nombre de questions officiel.
#' @param seuil Nombre de réponses justes requis.
#'
#' @return Une liste prête à afficher. `calibre` reste toujours `FALSE` tant
#'   que les items et la prédiction n'ont pas été validés sur des résultats
#'   réels d'examen.
#' @noRd
readiness_estimer <- function(
		seances,
		n_questions = cq_config("seance", "examen_nb_questions"),
		seuil = cq_config("seance", "examen_seuil")) {
	n_questions <- as.integer(n_questions)
	seuil <- as.integer(seuil)
	examens <- readiness_examens_valides(seances, n_questions)

	if (!length(examens)) {
		return(list(
			niveau = "absent",
			n_examens = 0L,
			calibre = FALSE,
			seuil = seuil,
			n_questions = n_questions,
			message = "Terminez un examen blanc pour obtenir un premier rep\u00e8re."
		))
	}

	scores <- vapply(examens, function(s) {
		sum(vapply(s$reponses, function(r) isTRUE(r$is_correct), logical(1)))
	}, integer(1))

	commun <- list(
		n_examens = length(scores),
		scores = scores,
		score_recent = scores[[1L]],
		moyenne = mean(scores),
		minimum = min(scores),
		maximum = max(scores),
		n_reussis = sum(scores >= seuil),
		calibre = FALSE,
		seuil = seuil,
		n_questions = n_questions
	)

	if (length(scores) == 1L) {
		return(c(commun, list(
			niveau = "premier_repere",
			message = paste(
				"Un seul examen blanc ne suffit pas pour juger la r\u00e9gularit\u00e9.",
				"Refaites-en un dans des conditions comparables."
			)
		)))
	}

	niveau <- if (min(scores) >= seuil) {
		"regulier"
	} else if (mean(scores) >= seuil) {
		"seuil_fragile"
	} else if (max(scores) >= seuil || mean(scores) >= seuil - 4L) {
		"proche"
	} else {
		"a_consolider"
	}

	message <- switch(
		niveau,
		regulier = "Le seuil a \u00e9t\u00e9 atteint dans tous les examens blancs observ\u00e9s.",
		seuil_fragile = "La moyenne atteint le seuil, mais les r\u00e9sultats restent irr\u00e9guliers.",
		proche = "Le seuil est proche ou a \u00e9t\u00e9 atteint ponctuellement ; la r\u00e9gularit\u00e9 reste \u00e0 construire.",
		a_consolider = "Les examens blancs observ\u00e9s restent sous le seuil requis."
	)

	c(commun, list(niveau = niveau, message = message))
}

#' Carte d'indicateur de préparation
#'
#' @param indicateur Sortie de [readiness_estimer()].
#'
#' @return Un élément d'interface.
#' @noRd
readiness_ui <- function(indicateur) {
	titre <- if (identical(indicateur$niveau, "absent")) {
		"Pr\u00e9paration non mesur\u00e9e"
	} else if (identical(indicateur$niveau, "premier_repere")) {
		glue::glue("Premier rep\u00e8re : {indicateur$score_recent}/{indicateur$n_questions}")
	} else {
		glue::glue("Rep\u00e8re actuel : {round(indicateur$moyenne, 1)}/{indicateur$n_questions} en moyenne")
	}

	detail <- if ((indicateur$n_examens %||% 0L) >= 2L) {
		glue::glue(
			"{indicateur$n_reussis} examen(s) blanc(s) r\u00e9ussi(s) sur {indicateur$n_examens} \u00b7 ",
			"scores observ\u00e9s de {indicateur$minimum} \u00e0 {indicateur$maximum}."
		)
	} else {
		glue::glue("Seuil officiel utilis\u00e9 par l'application : {indicateur$seuil}/{indicateur$n_questions}.")
	}

	shiny::div(
		class = "report-panel span-two cq-readiness",
		shiny::div(
			class = "panel-title",
			shiny::div(
				shiny::tags$h2("Pr\u00e9paration \u00e0 l'examen"),
				shiny::tags$p("Un rep\u00e8re fond\u00e9 uniquement sur vos examens blancs termin\u00e9s.")
			)
		),
		shiny::tags$strong(class = "cq-readiness-value", titre),
		shiny::tags$p(indicateur$message),
		shiny::tags$p(class = "chart-note", detail),
		shiny::tags$p(
			class = "chart-note",
			"Indicateur descriptif, non calibr\u00e9 sur les r\u00e9sultats r\u00e9els de l'examen."
		)
	)
}
