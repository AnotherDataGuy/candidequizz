# Agent A-8 : synthétiseur de séance.
#
# Agent LLM, un appel unique en fin de séance. Il ne voit que des statistiques
# déjà agrégées. Sans clé ou en cas de panne, un repli déterministe nomme les
# sous-thèmes les plus et les moins réussis.

#' Schéma de sortie du synthétiseur
#'
#' @return Un `ellmer::type_object()`.
#' @noRd
synthetiseur_schema <- function() {
	ellmer::type_object(
		diagnostic = ellmer::type_string(
			"Paragraphe de diagnostic, 4 a 8 phrases, en francais."
		),
		points_forts = ellmer::type_array(
			ellmer::type_string("Un point fort, une phrase."),
			"Exactement trois points forts."
		),
		points_a_travailler = ellmer::type_array(
			ellmer::type_string("Un point a travailler, une phrase actionnable."),
			"Exactement trois points a travailler."
		),
		plan_revision = ellmer::type_string(
			"Mini-plan de revision pour la prochaine seance."
		)
	)
}

#' Repli déterministe quand le modèle est indisponible
#'
#' @param snapshot Instantané de [stats_snapshot_seance()].
#'
#' @return Une synthèse au même format que le modèle.
#' @noRd
synthetiseur_repli <- function(snapshot) {
	faibles <- utils::head(snapshot$par_sous_theme, 3L)
	forts <- utils::tail(snapshot$par_sous_theme, 3L)

	texte_liste <- function(tableau, vide) {
		if (!nrow(tableau)) {
			return(vide)
		}

		glue::glue(
			"{tableau$cle} ({round(100 * tableau$reussite)} %)"
		)
	}

	diagnostic <- paste(
		c(
			glue::glue(
				"Vous avez r\u00e9pondu \u00e0 {snapshot$n_questions} ",
				"{if (snapshot$n_questions == 1L) 'question' else 'questions'}, ",
				"avec {round(100 * (snapshot$reussite %||% 0))} % de r\u00e9ussite."
			),
			if (isTRUE(snapshot$abandonnee)) {
				"La s\u00e9ance a \u00e9t\u00e9 interrompue : ce bilan porte sur ce qui a \u00e9t\u00e9 fait."
			} else {
				NULL
			},
			if (nrow(faibles)) {
				glue::glue(
					"Les sous-th\u00e8mes les plus fragiles sont : ",
					"{paste(texte_liste(faibles, ''), collapse = ', ')}."
				)
			} else {
				"Pas encore assez de r\u00e9ponses pour d\u00e9gager un profil."
			}
		),
		collapse = " "
	)

	points_forts <- as.character(texte_liste(
		forts,
		"Continuer \u00e0 vous entra\u00eener pour faire \u00e9merger des points forts."
	))
	points_faibles <- as.character(texte_liste(
		faibles,
		"Revenir sur l'ensemble du programme, sans cibler un sous-th\u00e8me."
	))

	while (length(points_forts) < 3L) {
		points_forts <- c(points_forts, "Garder ce rythme de travail.")
	}
	while (length(points_faibles) < 3L) {
		points_faibles <- c(
			points_faibles,
			"Refaire une s\u00e9ance courte sur un sous-th\u00e8me encore peu vu."
		)
	}

	list(
		diagnostic = diagnostic,
		points_forts = utils::head(points_forts, 3L),
		points_a_travailler = utils::head(points_faibles, 3L),
		plan_revision = paste(
			"Pour la prochaine s\u00e9ance, commencez par le sous-th\u00e8me le plus fragile,",
			"puis revoyez un sous-th\u00e8me d\u00e9j\u00e0 r\u00e9ussi pour consolider.",
			"Gardez le m\u00eame format de questions que celui qui vous a le plus ralenti."
		),
		source = "repli"
	)
}

#' Produire la synthèse pédagogique d'une séance
#'
#' @param ctx Contexte portant `synthese_stats`.
#'
#' @return Le contexte, `synthese` renseigné.
#' @noRd
agent_synthetiseur <- function(ctx) {
	snapshot <- ctx$synthese_stats

	if (is.null(snapshot) || isTRUE(ctx$degrade) || !llm_cle_disponible()) {
		ctx$synthese <- synthetiseur_repli(
			snapshot %||% stats_snapshot_seance(list(
				reponses = list(),
				statut = "terminee",
				journal_llm = list(),
				parametres = list(nb_questions = 0L)
			))
		)
		return(ctx_detail(ctx, repli = TRUE))
	}

	appel <- llm_appel_structure(
		agent = "synthetiseur",
		role = "synthetiseur",
		systeme = prompt_rendre("synthetiseur_systeme", socle = prompt_socle()),
		invite = prompt_rendre(
			"synthetiseur_invite",
			statistiques = stats_snapshot_texte(snapshot)
		),
		schema = synthetiseur_schema()
	)

	ctx <- ctx_journaliser(ctx, appel$journal)

	if (!appel$ok || is.null(appel$donnees)) {
		ctx$synthese <- synthetiseur_repli(snapshot)
		return(ctx_detail(ctx, repli = TRUE))
	}

	donnees <- appel$donnees
	ctx$synthese <- list(
		diagnostic = as.character(donnees$diagnostic %||% ""),
		points_forts = as.character(donnees$points_forts %||% character(0)),
		points_a_travailler = as.character(donnees$points_a_travailler %||% character(0)),
		plan_revision = as.character(donnees$plan_revision %||% ""),
		source = "llm"
	)

	ctx_detail(ctx, repli = FALSE)
}

#' Compléter une séance avec sa synthèse, une seule fois
#'
#' @param seance La séance achevée ou abandonnée.
#'
#' @return La séance, `synthese` renseigné.
#' @noRd
synthetiseur_completer <- function(seance) {
	if (synthetiseur_a_jour(seance)) {
		return(seance)
	}

	snapshot <- stats_snapshot_seance(seance)
	ctx <- list(
		parametres = seance$parametres,
		synthese_stats = snapshot,
		synthese = NULL,
		journal_llm = list(),
		degrade = generation_degradee(seance$parametres),
		lot = 0L,
		trace = list()
	)

	ctx <- tryCatch(
		run_agent("synthetiseur", ctx),
		error = function(e) {
			ctx$synthese <- synthetiseur_repli(snapshot)
			ctx
		}
	)

	seance$synthese <- ctx$synthese
	seance$synthese$n_reponses <- length(seance$reponses)
	seance$journal_llm <- c(seance$journal_llm %||% list(), ctx$journal_llm %||% list())

	seance
}

# Les anciennes synthèses n'avaient pas d'effectif et pouvaient avoir été
# rédigées après la première réponse. Elles sont recalculées une seule fois.
synthetiseur_a_jour <- function(seance) {
	is.list(seance$synthese) && nzchar(seance$synthese$diagnostic %||% "") &&
		identical(as.integer(seance$synthese$n_reponses), as.integer(length(seance$reponses)))
}
