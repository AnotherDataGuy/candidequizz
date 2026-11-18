# Passation d'une séance.
#
# Un écran par question. Le module ne connaît aucun type de question : il
# instancie le module de chaque type jouable trouvé dans le registre, et
# n'active que celui de la question en cours. Ajouter un type ne demandera donc
# rien ici.
#
# Le chronométrage se déduit des événements Shiny, sans JavaScript : l'instant
# d'affichage est noté au rendu, la première interaction est le premier
# changement de la réponse, et la durée est arrêtée à la validation. Pour un
# QCM, le premier clic sur une option *est* la première interaction.

#' Identifiant de module du type de question
#'
#' @param type Identifiant du type.
#'
#' @return Une chaîne de caractères.
#' @noRd
seance_run_saisie_id <- function(type) {
	paste0("saisie_", type)
}

#' Interface de la passation
#'
#' @param id Identifiant du module.
#'
#' @return Un élément d'interface.
#' @noRd
mod_seance_run_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-passation quiz-wrap",
		# Filtre « encre » du tampon : bords irréguliers et manques d'encre.
		htmltools::HTML(paste0(
			'<svg width="0" height="0" aria-hidden="true" focusable="false" style="position:absolute">',
			'<filter id="cq-encre" x="-10%" y="-10%" width="120%" height="120%">',
			'<feTurbulence type="fractalNoise" baseFrequency="0.85" numOctaves="2" seed="7" result="bruit"/>',
			'<feDisplacementMap in="SourceGraphic" in2="bruit" scale="2.6" result="deforme"/>',
			'<feColorMatrix in="bruit" type="matrix" values="0 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 -2.4 2.35" result="manques"/>',
			'<feComposite in="deforme" in2="manques" operator="in"/>',
			'</filter></svg>'
		)),
		shiny::uiOutput(ns("entete")),
		# Chronomètre dans sa propre sortie : seul ce texte est redessiné chaque
		# seconde, pas tout l'en-tête (sinon l'écran « clignote »).
		shiny::uiOutput(ns("chrono"), class = "cq-chrono-sortie"),
		shiny::uiOutput(ns("corps")),
		shiny::uiOutput(ns("pied"))
	)
}

#' En-tête « guichet » : ticket de la question et talons de la séance
#'
#' Le numéro de la question est présenté comme un ticket de guichet ; la
#' progression comme une rangée de talons, un par question, colorés selon le
#' résultat (juste, faux, passée). Aucune étiquette de thème ni de type :
#' l'utilisateur ne voit pas le classement des questions.
#'
#' @param position Rang de la question en cours.
#' @param total Nombre de questions de la séance.
#' @param resultats Vecteur nommé par rang (`"1"`, `"2"`…) : `"juste"`,
#'   `"faux"` ou `"passee"` pour les questions déjà traitées.
#' @param libelle Ligne de contexte (type de séance).
#'
#' @return Un élément d'interface.
#' @noRd
seance_run_progression <- function(position, total, resultats = character(), libelle = NULL) {
	part <- if (total > 0L) round(100 * (position - 1L) / total) else 0
	numero <- sprintf("%02d", as.integer(position))
	repondue <- !is.na(resultats[as.character(position)])

	talons <- lapply(seq_len(total), function(i) {
		etat <- resultats[as.character(i)]
		classe <- if (!is.na(etat)) {
			paste0("cq-talon cq-talon--", etat, if (i == position) " is-nouveau" else "")
		} else if (i == position) {
			"cq-talon cq-talon--courant"
		} else {
			"cq-talon"
		}
		shiny::tags$li(class = classe, sprintf("%02d", i))
	})

	shiny::div(
		class = "cq-guichet-entete",
		shiny::div(
			class = "cq-guichet-ligne",
			shiny::div(
				class = "cq-guichet-contexte",
				shiny::span(class = "cq-guichet-lieu", paste(c("Guichet", libelle), collapse = " \u00b7 ")),
				shiny::span(class = "cq-guichet-mot", "Prenez votre temps. Personne ne fait la queue derri\u00e8re vous.")
			),
			shiny::div(
				class = "cq-ticket",
				shiny::span(class = "cq-ticket-mot", "Ticket"),
				shiny::span(
					class = "cq-ticket-numero",
					"N\u00b0 ",
					shiny::span(class = if (repondue) "cq-ticket-chiffres" else "cq-ticket-chiffres is-flip", numero),
					shiny::span(class = "cq-ticket-total", paste0(" / ", total))
				)
			)
		),
		shiny::tags$ol(
			class = "cq-talons",
			role = "progressbar",
			`aria-label` = glue::glue("Question {position} sur {total}"),
			`aria-valuenow` = part,
			`aria-valuemin` = "0",
			`aria-valuemax` = "100",
			talons
		)
	)
}

#' Résultat de chaque question déjà traitée, par rang
#'
#' @param seance La séance.
#'
#' @return Vecteur nommé par rang : `"juste"`, `"faux"` ou `"passee"`.
#' @noRd
seance_run_resultats <- function(seance) {
	reps <- seance$reponses %||% list()
	if (!length(reps)) {
		return(character())
	}

	rangs <- vapply(reps, function(r) as.character(r$indice_seance %||% r$numero %||% NA), character(1))
	etats <- vapply(reps, function(r) {
		if (isTRUE(r$passee)) "passee" else if (isTRUE(r$is_correct)) "juste" else "faux"
	}, character(1))
	stats::setNames(etats, rangs)
}

#' Tampon de correction posé sur la fiche
#'
#' @param verdict Notation de la réponse.
#'
#' @return Un élément d'interface.
#' @noRd
seance_run_tampon <- function(verdict) {
	etat <- if (isTRUE(verdict$is_correct)) {
		"juste"
	} else if ((verdict$score %||% 0) > 0) {
		"partiel"
	} else {
		"faux"
	}
	texte <- c(juste = "Correct", partiel = "Partiel", faux = "\u00c0 revoir")[[etat]]

	shiny::div(
		class = paste0("cq-tampon cq-tampon--", etat),
		role = "status",
		shiny::span(class = "cq-tampon-bavure", `aria-hidden` = "true"),
		shiny::div(
			class = "cq-tampon-cadre",
			shiny::span(class = "cq-tampon-haut", "Guichet Candide"),
			shiny::span(class = "cq-tampon-texte", texte),
			shiny::span(class = "cq-tampon-bas", format(Sys.Date(), "%d \u00b7 %m \u00b7 %Y"))
		)
	)
}

#' Ce que les séances antérieures apportent à la génération
#'
#' Ne change pas pendant une séance : lu une fois, puis réutilisé à chaque
#' lot, au lieu de relire tous les fichiers de séance à chaque préchargement.
#'
#' @param seance La séance en cours.
#'
#' @return `list(session_id, reponses, seeds)`.
#' @noRd
seance_run_anterieur <- function(seance, user_id = NULL) {
	user_id <- user_id %||% seance$user_id
	list(
		session_id = seance$session_id,
		reponses = if (parametres_adaptatif_actif(seance$parametres)) {
			store_reponses_historiques(sauf_session_id = seance$session_id, user_id = user_id)
		} else {
			list()
		},
		seeds = store_seeds_historiques(sauf_session_id = seance$session_id, user_id = user_id)
	)
}

#' Arguments de génération d'un lot
#'
#' Unique construction des arguments de [generer_lot()] en cours de séance,
#' pour que le préchargement voie exactement le même historique que le
#' premier lot : réponses antérieures pour le ciblage thématique, réponses de
#' la séance pour la difficulté.
#'
#' @param seance La séance.
#' @param lot Numéro du lot à générer.
#' @param anterieur Résultat de [seance_run_anterieur()].
#'
#' @return Une liste nommée.
#' @noRd
seance_run_arguments_lot <- function(seance, lot, anterieur = seance_run_anterieur(seance)) {
	historique <- if (parametres_adaptatif_actif(seance$parametres)) {
		list(
			reponses = c(anterieur$reponses, seance$reponses),
			reponses_session = seance$reponses
		)
	} else {
		list(reponses = list(), reponses_session = list())
	}

	list(
		parametres = seance$parametres,
		plan = seance$plan,
		lot = as.integer(lot),
		exclure_seed_ids = unique(c(seance_seeds_servies(seance), anterieur$seeds)),
		historique = historique,
		cache = cache_parametres(seance)
	)
}

#' Réordonner les questions prêtes après une réponse
#'
#' Applique [adapt_aiguiller()] aux questions déjà écrites et pas encore
#' posées, toutes tranches confondues : seules leurs places (`indice_seance`)
#' sont échangées. Les questions déjà posées ne bougent pas, et une tranche
#' encore en préparation arrive à ses propres rangs. L'examen blanc garde son
#' ordre.
#'
#' @param seance La séance, la réponse à `position` déjà enregistrée.
#' @param position Rang de la question qui vient d'être répondue.
#'
#' @return La séance, éventuellement réordonnée.
#' @noRd
seance_aiguiller <- function(seance, position) {
	if (!parametres_adaptatif_actif(seance$parametres) || is.null(seance$plan)) {
		return(seance)
	}

	position <- as.integer(position)
	lot <- seance_lot_de_position(seance$plan, position)

	if (is.na(position) || !length(seance$items)) {
		return(seance)
	}

	rangs <- vapply(
		seance$items,
		function(i) as.integer(i$indice_seance %||% NA_integer_)[[1L]],
		integer(1)
	)
	restants <- which(!is.na(rangs) & rangs > position)

	if (length(restants) < 2L) {
		return(seance)
	}

	restants <- restants[order(rangs[restants])]
	reponses <- seance$reponses
	dernier <- if (length(reponses)) reponses[[length(reponses)]]$sub_theme %||% NA_character_ else NA_character_

	choix <- adapt_aiguiller(
		difficultes = vapply(
			seance$items[restants],
			function(i) as.integer(i$difficulte %||% NA_integer_)[[1L]],
			integer(1)
		),
		sous_themes = vapply(
			seance$items[restants],
			function(i) as.character(i$sub_theme %||% NA_character_)[[1L]],
			character(1)
		),
		resultats = vapply(reponses, function(r) isTRUE(r$is_correct), logical(1)),
		dernier_sous_theme = dernier
	)

	if (identical(choix$rang, 1L)) {
		return(seance)
	}

	ordre <- c(restants[[choix$rang]], restants[-choix$rang])
	places <- sort(rangs[restants])

	for (k in seq_along(ordre)) {
		seance$items[[ordre[[k]]]]$indice_seance <- places[[k]]
	}

	seance$aiguillages <- c(seance$aiguillages %||% list(), list(list(
		apres = position,
		lot = lot,
		item_id = seance$items[[ordre[[1L]]]]$item_id,
		motif = choix$motif
	)))

	seance
}

#' Diagnostic prudent d'une réponse erronée
#'
#' Un distracteur signale une confusion possible, pas un diagnostic certain de
#' la personne. Le libellé garde cette nuance tout en rendant le tag technique
#' utile immédiatement.
#'
#' @param verdict Notation de la réponse courante.
#'
#' @return Un élément d'interface, ou `NULL`.
#' @noRd
seance_run_diagnostic_ui <- function(verdict) {
	if (is.null(verdict) || isTRUE(verdict$is_correct)) {
		return(NULL)
	}

	tags <- unique(as.character(verdict$detail$diagnostic_tags %||% character(0)))
	tags <- tags[!is.na(tags) & nzchar(tags)]

	if (!length(tags)) {
		return(NULL)
	}

	libelles <- vapply(tags, stats_libelle_confusion, character(1))

	shiny::div(
		class = "cq-diagnostic-confusion",
		shiny::tags$strong("Pi\u00e8ge possible rep\u00e9r\u00e9"),
		shiny::tags$p(
			paste(libelles, collapse = " "),
			" Cette indication sera confirm\u00e9e seulement si elle se r\u00e9p\u00e8te."
		)
	)
}

#' Intégrer un lot préchargé si c'est bien le suivant
#'
#' @param seance La séance.
#' @param ctx Contexte renvoyé par [seance_run_produire_lot()].
#'
#' @return La séance, inchangée si le lot n'est pas le prochain attendu.
#' @noRd
seance_run_integrer_lot <- function(seance, ctx) {
	if (!is.list(ctx) || is.null(ctx$lot)) {
		return(seance)
	}

	if (!identical(as.integer(ctx$lot), as.integer(seance$lots_generes) + 1L)) {
		return(seance)
	}

	seance_ajouter_lot(seance, ctx)
}

#' Écran d'attente de la question suivante
#'
#' N'apparaît que si l'utilisateur a rattrapé l'écriture : la passation
#' reprend d'elle-même dès que la tranche arrive. L'indicateur tourne sans
#' geler le reste de l'interface.
#'
#' @param lot Numéro de la tranche attendue.
#' @param n_lots Nombre total de tranches.
#' @param pret La tranche attendue est-elle déjà intégrée.
#' @param erreur Message d'échec, ou `NULL`.
#'
#' @return Un élément d'interface.
#' @noRd
seance_run_transition <- function(lot, n_lots, pret, erreur = NULL) {
	if (!is.null(erreur)) {
		return(shiny::div(
			class = "cq-carte cq-attente",
			shiny::div(
				class = "cq-attente-titre",
				bsicons::bs_icon("exclamation-triangle"),
				"Impossible de pr\u00e9parer la suite"
			),
			shiny::p(class = "cq-aide cq-aide--alerte", erreur)
		))
	}

	if (isTRUE(pret)) {
		return(shiny::div(
			class = "cq-carte cq-attente",
			shiny::div(
				class = "cq-attente-titre",
				bsicons::bs_icon("check-circle-fill"),
				"Question suivante pr\u00eate"
			),
			shiny::p(class = "cq-aide", "Les questions suivantes sont pr\u00eates.")
		))
	}

	shiny::div(
		class = "cq-carte cq-attente cq-impression",
		shiny::div(
			class = "cq-imprimante",
			`aria-hidden` = "true",
			shiny::div(class = "cq-imprimante-tete", shiny::span(class = "cq-imprimante-voyant")),
			shiny::div(
				class = "cq-imprimante-sortie",
				shiny::div(
					class = "cq-ticket-papier",
					shiny::span(class = "cq-ticket-papier-haut", "Candide au guichet"),
					shiny::span(class = "cq-ticket-papier-lot", glue::glue("Lot {lot}")),
					shiny::span(class = "cq-ticket-papier-bas", glue::glue("sur {n_lots}"))
				)
			)
		),
		shiny::div(
			class = "cq-attente-titre",
			"Pr\u00e9paration des questions suivantes"
		),
		shiny::p(
			class = "cq-aide",
			"Le guichet imprime la suite",
			shiny::span(class = "cq-points", `aria-hidden` = "true", shiny::span("."), shiny::span("."), shiny::span(".")),
			" Vos r\u00e9ponses sont enregistr\u00e9es."
		),
		shiny::div(
			class = "cq-attente-piste",
			role = "progressbar",
			`aria-label` = "Pr\u00e9paration en cours",
			shiny::div(class = "cq-attente-jauge")
		)
	)
}

#' Faut-il quitter la passation pour le bilan
#'
#' @param seance La séance.
#' @param position Rang affiché.
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
seance_run_vers_bilan <- function(seance, position) {
	if (is.null(seance)) {
		return(FALSE)
	}

	seance_closee(seance) || seance_achevee(seance)
}

#' Signaler un échec de génération à l'utilisateur
#'
#' Un message en français et actionnable, jamais une trace R brute.
#'
#' @param condition La condition d'erreur.
#'
#' @return Rien, appelé pour ses effets.
#' @noRd
seance_run_signaler_echec <- function(condition) {
	shiny::showNotification(
		shiny::tagList(
			shiny::strong("Impossible de pr\u00e9parer les questions."),
			shiny::br(),
			cq_message_utilisateur(condition)
		),
		type = "error",
		duration = NULL
	)

	invisible(NULL)
}

#' Serveur de la passation
#'
#' @param id Identifiant du module.
#' @param seance Valeur réactive portant la séance en cours.
#' @param retour_differe Réactif indiquant si les corrections sont différées.
#' @param terminer Fonction appelée quand la séance s'achève.
#'
#' @return Rien, appelé pour ses effets.
#' @noRd
mod_seance_run_server <- function(id, seance, retour_differe, terminer, utilisateur = shiny::reactive(NULL)) {
	shiny::moduleServer(id, function(input, output, session) {
		position <- shiny::reactiveVal(1L)
		notation <- shiny::reactiveVal(NULL)
		affiche_le <- shiny::reactiveVal(Sys.time())
		premiere_interaction <- shiny::reactiveVal(NA_real_)
		n_changements <- shiny::reactiveVal(0L)
		phase <- shiny::reactiveVal("question")
		prefetch_erreur <- shiny::reactiveVal(NULL)
		lot_demande <- shiny::reactiveVal(NA_integer_)
		chrono_reste <- shiny::reactiveVal(NA_real_)

		# Chronom\u00e8tre : le temps restant vit dans `chrono_reste()`, pas dans
		# `seance()`. R\u00e9\u00e9crire la s\u00e9ance chaque seconde redessinait toute la
		# question (et pouvait effacer la r\u00e9ponse en cours). Le temps \u00e9coul\u00e9 est
		# report\u00e9 dans la s\u00e9ance \u00e0 chaque enregistrement (`seance_chrono_tick()`).
		shiny::observe({
			s <- seance()
			if (is.null(s) || !seance_chrono_actif(s) || seance_closee(s)) {
				chrono_reste(NA_real_)
				return(invisible(NULL))
			}

			marque <- s$chrono_marque_le
			if (!inherits(marque, "POSIXt") || is.na(marque)) {
				seance(seance_chrono_tick(s))
				return(invisible(NULL))
			}

			shiny::invalidateLater(1000, session)
			s <- seance_chrono_tick(s)
			chrono_reste(seance_chrono_reste(s))

			if (seance_chrono_reste(s) <= 0) {
				s <- seance_expirer_chrono(s)
				seance(s)
				store_sauver(s)
				shiny::showNotification(
					"Le temps imparti est \u00e9coul\u00e9. Voici votre bilan.",
					type = "warning",
					duration = 8
				)
				terminer()
				return(invisible(NULL))
			}
		})

		# T-10 : le lot N+1 s'écrit pendant que l'utilisateur répond au lot N,
		# dans un processus à part (voir `async_generer_lot()`). ExtendedTask
		# seul ne suffit pas : il exécute sa fonction dans le processus
		# principal, et n'attend qu'une promesse.
		prefetch <- shiny::ExtendedTask$new(function(args) {
			async_generer_lot(args)
		})

		anterieur <- NULL

		anterieur_de <- function(s) {
			if (is.null(anterieur) || !identical(anterieur$session_id, s$session_id)) {
				u <- utilisateur()
				anterieur <<- seance_run_anterieur(s, if (is.null(u)) s$user_id else u$user_id)
			}

			anterieur
		}

		essayer_precharger <- function() {
			s <- shiny::isolate(seance())
			pos <- shiny::isolate(position())

			if (is.null(s) || !seance_doit_precharger(s, pos)) {
				return(invisible(NULL))
			}

			cible <- as.integer(s$lots_generes) + 1L
			statut <- prefetch$status()

			if (identical(statut, "running")) {
				return(invisible(NULL))
			}

			# Déjà produit, en attente d'intégration : ne pas relancer le même lot.
			if (identical(lot_demande(), cible) && identical(statut, "success")) {
				return(invisible(NULL))
			}

			prefetch_erreur(NULL)
			lot_demande(cible)
			prefetch$invoke(seance_run_arguments_lot(s, cible, anterieur_de(s)))

			invisible(NULL)
		}

		item_courant <- shiny::reactive({
			s <- seance()
			shiny::req(s)

			seance_item(s, position())
		})

		# Un module de saisie par type jouable, instancié une fois pour toutes.
		# Chacun ne reçoit l'item que lorsqu'il est du type qu'il sait afficher,
		# ce qui évite de recréer des modules à chaque question — et donc
		# d'accumuler des observateurs au fil d'une séance de 60 questions.
		types_jouables <- question_type_ids(jouable = TRUE)

		saisies <- stats::setNames(
			lapply(types_jouables, function(type) {
				question_type(type)$server(
					seance_run_saisie_id(type),
					item = shiny::reactive({
						it <- item_courant()
						shiny::req(it, identical(it$type, type))

						it
					}),
								notation = notation
				)
			}),
			types_jouables
		)

		reponse_courante <- shiny::reactive({
			it <- item_courant()
			shiny::req(it)

			saisie <- saisies[[it$type]]
			shiny::req(saisie)

			saisie$reponse()
		})

		# --- Cycle de vie d'une question ---------------------------------------

		shiny::observeEvent(position(), {
			notation(NULL)
			premiere_interaction(NA_real_)
			n_changements(0L)
			affiche_le(Sys.time())
			essayer_precharger()
		})

		shiny::observe({
			seance()
			position()
			essayer_precharger()
		})

		shiny::observe({
			if (!identical(prefetch$status(), "success")) {
				return(invisible(NULL))
			}

			ctx <- prefetch$result()
			s <- shiny::isolate(seance())
			shiny::req(s)

			integree <- seance_run_integrer_lot(s, ctx)

			if (!identical(integree$lots_generes, s$lots_generes)) {
				seance(integree)
				store_sauver(integree)
				cache_integrer(ctx$items)
			}

			invisible(NULL)
		})

		shiny::observe({
			if (!identical(prefetch$status(), "error")) {
				return(invisible(NULL))
			}

			erreur <- tryCatch(prefetch$result(), error = function(e) e)
			prefetch_erreur(cq_message_utilisateur(erreur))
			seance_run_signaler_echec(erreur)
		})

		shiny::observeEvent(reponse_courante(), {
			# Une réponse déjà validée ne compte plus de changement d'avis.
			if (!is.null(notation())) {
				return(invisible(NULL))
			}

			if (is.na(premiere_interaction())) {
				premiere_interaction(
					as.numeric(difftime(Sys.time(), affiche_le(), units = "secs")) * 1000
				)
			} else {
				n_changements(n_changements() + 1L)
			}
		}, ignoreNULL = TRUE, ignoreInit = TRUE)

		# --- Affichage ----------------------------------------------------------

		output$chrono <- shiny::renderUI({
			s <- seance()
			shiny::req(s, seance_chrono_actif(s))

			reste <- chrono_reste()
			if (!is.finite(reste)) {
				reste <- seance_chrono_reste(s)
			}

			shiny::div(
				class = paste(
					"cq-chrono",
					if (is.finite(reste) && reste <= 60) "cq-chrono--urgent" else ""
				),
				bsicons::bs_icon("stopwatch"),
				glue::glue("{seance_chrono_texte(reste)} restantes")
			)
		})

		output$entete <- shiny::renderUI({
			s <- seance()
			shiny::req(s)

			profils <- seance_config_profils()
			libelle <- names(profils)[match(as.character(s$parametres$profil %||% "libre"), profils)]

			seance_run_progression(
				position(),
				s$parametres$nb_questions,
				resultats = seance_run_resultats(s),
				libelle = if (is.na(libelle)) NULL else libelle
			)
		})

		lot_en_attente <- shiny::reactive({
			s <- seance()
			if (is.null(s)) {
				return(NULL)
			}

			if (identical(phase(), "transition")) {
				return(seance_lot_de_position(s$plan, position()) + 1L)
			}

			it <- item_courant()
			if (is.null(it) && !seance_achevee(s) && s$lots_generes < s$plan$n_lots) {
				return(seance_lot_de_position(s$plan, position()))
			}

			NULL
		})

		output$corps <- shiny::renderUI({
			s <- seance()
			shiny::req(s)

			lot_cible <- lot_en_attente()

			if (!is.null(lot_cible) && !is.na(lot_cible)) {
				return(seance_run_transition(
					lot = lot_cible,
					n_lots = s$plan$n_lots,
					pret = s$lots_generes >= lot_cible,
					erreur = prefetch_erreur()
				))
			}

			it <- item_courant()

			if (is.null(it)) {
				if (seance_run_vers_bilan(s, position())) {
					return(shiny::div(
						class = "cq-carte cq-attente",
						shiny::div(
							class = "cq-attente-titre",
							bsicons::bs_icon("clipboard2-check"),
							"S\u00e9ance termin\u00e9e"
						),
						shiny::p(
							class = "cq-aide",
							"Le d\u00e9tail de vos r\u00e9ponses, les corrections et les explications sont dans le bilan."
						)
					))
				}

				return(shiny::div(
					class = "cq-carte",
					shiny::p(
						"Aucune question disponible pour le moment. ",
						"Revenez au param\u00e9trage pour d\u00e9marrer une nouvelle s\u00e9ance."
					)
				))
			}

			# Pas d'étiquette de thème ni de type : l'utilisateur ne voit pas le
			# classement des questions.
			shiny::div(
				class = "cq-carte cq-question cq-fiche",
				shiny::uiOutput(session$ns("tampon"), class = "cq-tampon-sortie"),
				shiny::span(class = "cq-fiche-mot", "Question"),
				shiny::h2(class = "cq-question-enonce", it$enonce),
				question_type(it$type)$ui(session$ns(seance_run_saisie_id(it$type)))
			)
		})

		# Le tampon ne dépend que de la notation : seul lui se redessine à la
		# validation, la fiche et sa saisie restent en place.
		output$tampon <- shiny::renderUI({
			verdict <- notation()
			shiny::req(verdict, !isTRUE(retour_differe()))
			seance_run_tampon(verdict)
		})

		output$pied <- shiny::renderUI({
			s <- seance()
			shiny::req(s)

			lot_cible <- lot_en_attente()

			if (is.null(item_courant()) && seance_run_vers_bilan(s, position())) {
				return(shiny::div(
					class = "cq-actions",
					shiny::actionButton(
						session$ns("ouvrir_bilan"),
						shiny::tagList("Voir le bilan de la s\u00e9ance", bsicons::bs_icon("arrow-right")),
						class = "btn btn-primary cq-bouton-cta"
					)
				))
			}

			if (!is.null(lot_cible) && !is.na(lot_cible)) {
				if (!is.null(prefetch_erreur())) {
					return(shiny::div(
						class = "cq-actions",
						shiny::actionButton(
							session$ns("reessayer_lot"),
							shiny::tagList("R\u00e9essayer", bsicons::bs_icon("arrow-right")),
							class = "btn btn-primary cq-bouton-cta"
						)
					))
				}

				if (isTRUE(s$lots_generes >= lot_cible) && identical(phase(), "transition")) {
					return(shiny::div(
						class = "cq-actions",
						shiny::actionButton(
							session$ns("continuer_lot"),
							shiny::tagList("Continuer", bsicons::bs_icon("arrow-right")),
							class = "btn btn-primary cq-bouton-cta"
						),
						shiny::actionButton(
							session$ns("abandonner"),
							"Arr\u00eater et voir le bilan",
							class = "btn cq-btn-ghost cq-bouton-secondaire"
						)
					))
				}

				return(shiny::div(
					class = "cq-actions",
					shiny::actionButton(
						session$ns("abandonner"),
						"Arr\u00eater et voir le bilan",
						class = "btn cq-btn-ghost cq-bouton-secondaire"
					)
				))
			}

			it <- item_courant()
			shiny::req(it)

			verdict <- notation()
			dernier <- position() >= s$parametres$nb_questions

			if (is.null(verdict)) {
				return(shiny::div(
					class = "cq-actions",
					shiny::actionButton(
						session$ns("valider"),
						shiny::tagList("Valider", bsicons::bs_icon("check-lg")),
						class = "btn btn-primary cq-bouton-cta"
					),
					shiny::actionButton(
						session$ns("passer"),
						"Passer cette question",
						class = "btn cq-btn-ghost cq-bouton-secondaire"
					),
					shiny::span(
						class = "cq-raccourcis",
						"Clavier : ", shiny::tags$kbd("A"), "\u2013", shiny::tags$kbd("D"), " puis ", shiny::tags$kbd("Entr\u00e9e")
					),
					shiny::actionButton(
						session$ns("abandonner"),
						"Arr\u00eater et voir le bilan",
						class = "btn btn-link cq-bouton-discret"
					)
				))
			}

			shiny::tagList(
				# Retour différé : on confirme l'enregistrement sans rien dévoiler.
				if (isTRUE(retour_differe())) {
					shiny::div(
						class = "cq-carte cq-correction cq-correction--differee",
						shiny::div(
							class = "cq-correction-titre",
							bsicons::bs_icon("check2"),
							"R\u00e9ponse enregistr\u00e9e"
						),
						shiny::p("Les corrections vous seront pr\u00e9sent\u00e9es \u00e0 la fin de la s\u00e9ance.")
					)
				} else {
					shiny::div(
						class = paste0(
							"cq-carte cq-correction cq-correction--",
							if (isTRUE(verdict$is_correct)) {
								"juste"
							} else if ((verdict$score %||% 0) > 0) {
								"partiel"
							} else {
								"faux"
							}
						),
						shiny::div(
							class = "cq-correction-titre cq-marge-titre",
							shiny::span("Note en marge"),
							shiny::span(
								class = "visually-hidden",
								if (isTRUE(verdict$is_correct)) {
									" : r\u00e9ponse correcte"
								} else if ((verdict$score %||% 0) > 0) {
									" : r\u00e9ponse partiellement correcte"
								} else {
									" : r\u00e9ponse incorrecte"
								}
							),
							htmltools::HTML('<svg class="cq-marge-trait" width="120" height="10" viewBox="0 0 120 10" aria-hidden="true" focusable="false"><path d="M2 6 C 30 2, 60 9, 118 4"/></svg>')
						),
						cq_explication_ui(it$explication),
						seance_run_diagnostic_ui(verdict)
					)
				},
				shiny::div(
					class = "cq-actions",
					shiny::actionButton(
						session$ns("suivant"),
						shiny::tagList(
							if (dernier) "Voir mon bilan" else "Question suivante",
							bsicons::bs_icon("arrow-right")
						),
						class = "btn btn-primary cq-bouton-cta"
					)
				)
			)
		})

		# --- Validation ---------------------------------------------------------

		avancer <- function() {
			if (seance_run_vers_bilan(seance(), position())) {
				return(terminer())
			}

			s <- seance()
			pos <- position()

			# File glissante : pas de pause entre tranches. L'écran d'attente
			# n'apparaît que si la question suivante n'est pas encore écrite.
			if (pos < s$parametres$nb_questions && is.null(seance_item(s, pos + 1L))) {
				phase("transition")
				essayer_precharger()
				return(invisible(NULL))
			}

			position(pos + 1L)
		}

		shiny::observe({
			if (!identical(phase(), "transition")) {
				return(invisible(NULL))
			}

			s <- seance()
			shiny::req(s)
			suivante <- shiny::isolate(position()) + 1L

			if (!is.null(seance_item(s, suivante))) {
				phase("question")
				position(suivante)
			}
		})

		enregistrer <- function(passee, reponse = NULL, repondu_le = Sys.time()) {
			it <- item_courant()

			if (is.null(it)) {
				return(invisible(NULL))
			}

			if (!passee && is.null(reponse)) {
				reponse <- shiny::isolate(reponse_courante())
			}

			enregistrement <- reponse_nouvelle(
				item = it,
				reponse = if (passee) NULL else reponse,
				affiche_le = affiche_le(),
				premiere_interaction_ms = premiere_interaction(),
				n_changements = n_changements(),
				passee = passee,
				repondu_le = repondu_le
			)

			s <- seance_ajouter_reponse(seance_chrono_tick(seance()), enregistrement)
			s <- seance_aiguiller(s, position())

			if (seance_achevee(s)) {
				s <- seance_clore(s, "terminee")
			}

			seance(s)
			store_sauver(s)

			# Retour différé : on n'affiche pas la correction (y compris les
			# options colorées du type) et on passe tout de suite à la suite.
			if (isTRUE(retour_differe())) {
				if (seance_achevee(s)) {
					terminer()
				} else {
					avancer()
				}

				return(invisible(NULL))
			}

			# La réponse donnée voyage avec la notation : le module du type peut
			# ainsi afficher la correction sans dépendre d'une valeur d'`input`
			# dont l'élément vient d'être retiré de la page.
			notation(list(
				score = enregistrement$score,
				is_correct = enregistrement$is_correct,
				detail = enregistrement$detail,
				reponse = reponse,
				passee = passee
			))

			invisible(NULL)
		}

		shiny::observeEvent(input$valider, {
			reponse <- shiny::isolate(reponse_courante())

			if (is.null(reponse) || !length(reponse) || !nzchar(as.character(reponse)[[1L]])) {
				shiny::showNotification(
					"Choisissez une r\u00e9ponse, ou passez la question.",
					type = "warning",
					duration = 4
				)

				return(invisible(NULL))
			}

			s <- seance()
			if (!isTRUE(s$parametres$avis_pendant)) {
				enregistrer(passee = FALSE)
				return(invisible(NULL))
			}

			# Option « Pendant et à la fin » : la note est demandée avant la
			# correction. L'heure de réponse est figée maintenant, pour que le
			# temps passé dans la fenêtre ne compte pas dans la durée.
			avis_en_attente(list(reponse = reponse, repondu_le = Sys.time(), position = position()))
			pos <- position()
			shiny::showModal(shiny::modalDialog(
				title = "Difficult\u00e9 de cette question",
				class = "cq-avis-modal",
				shiny::tags$p(class = "muted", "De 1 (tr\u00e8s facile) \u00e0 10 (redoutable). Vous pourrez la corriger \u00e0 la fin."),
				avis_echelle(session$ns(paste0("avis_note_", pos))),
				shiny::textAreaInput(
					session$ns(paste0("avis_com_", pos)),
					label = NULL,
					placeholder = "Commentaire (facultatif)",
					width = "100%",
					rows = 2
				),
				footer = shiny::actionButton(
					session$ns("avis_continuer"),
					shiny::tagList("Continuer", bsicons::bs_icon("arrow-right")),
					class = "btn primary"
				),
				size = "l",
				easyClose = FALSE
			))
		})

		avis_en_attente <- shiny::reactiveVal(NULL)

		shiny::observeEvent(input$avis_continuer, {
			attente <- avis_en_attente()
			it <- item_courant()
			shiny::req(attente, it)
			pos <- attente$position
			note <- input[[paste0("avis_note_", pos)]]

			if (is.null(note) || !nzchar(note)) {
				shiny::showNotification("Choisissez une note de 1 \u00e0 10.", type = "warning", duration = 4)
				return(invisible(NULL))
			}

			seance(avis_enregistrer(
				seance(),
				it$item_id,
				note = note,
				commentaire = input[[paste0("avis_com_", pos)]],
				moment = "seance"
			))
			avis_en_attente(NULL)
			shiny::removeModal()
			enregistrer(passee = FALSE, reponse = attente$reponse, repondu_le = attente$repondu_le)
		})

		shiny::observeEvent(input$passer, {
			enregistrer(passee = TRUE)
		})

		shiny::observeEvent(input$ouvrir_bilan, {
			terminer()
		})

		shiny::observeEvent(input$suivant, {
			avancer()
		})

		shiny::observeEvent(input$continuer_lot, {
			phase("question")
			position(position() + 1L)
		})

		shiny::observeEvent(input$reessayer_lot, {
			s <- seance()
			shiny::req(s)
			s$parametres$generation_retenter <- TRUE
			seance(s)
			prefetch_erreur(NULL)
			lot_demande(NA_integer_)
			essayer_precharger()
		})

		shiny::observeEvent(input$abandonner, {
			s <- seance()
			shiny::req(s)

			s <- seance_clore(seance_chrono_tick(s), "abandonnee")
			seance(s)
			store_sauver(s)
			terminer()
		})

		# Reprise : uniquement quand l'identifiant change, pas à chaque
		# sauvegarde. Sinon, après la dernière réponse, on sauterait au rang
		# n+1 et l'écran vide remplacerait le bilan.
		id_charge <- shiny::reactiveVal(NULL)

		shiny::observeEvent(seance(), {
			s <- seance()
			if (is.null(s)) {
				id_charge(NULL)
				return(invisible(NULL))
			}

			if (identical(id_charge(), s$session_id)) {
				return(invisible(NULL))
			}

			id_charge(s$session_id)
			position(seance_position_reprise(s))
			phase("question")
			prefetch_erreur(NULL)
			lot_demande(NA_integer_)
		}, ignoreNULL = TRUE)

		shiny::observe({
			s <- seance()
			if (is.null(s) || !seance_run_vers_bilan(s, position())) {
				return(invisible(NULL))
			}

			if (!is.null(seance_item(s, position())) && !is.null(notation())) {
				return(invisible(NULL))
			}

			if (is.null(seance_item(s, position()))) {
				terminer()
			}
		})

		invisible(NULL)
	})
}
