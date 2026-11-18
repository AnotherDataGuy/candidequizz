# Agrégations statistiques intra et inter-séances.
#
# Les modules d'interface ne calculent rien : ils lisent ces tableaux. Un
# changement de formule se teste ici, sans ouvrir Shiny.

#' Charger des séances pour les statistiques
#'
#' @param session_ids Identifiants à retenir. `NULL` pour toutes.
#'
#' @return Une liste de séances, de la plus récente à la plus ancienne.
#' @noRd
stats_charger <- function(session_ids = NULL, user_id = NULL) {
	# Base SQL : une seule requête. Passer par store_lister() puis
	# store_charger() relisait et désérialisait chaque séance deux fois.
	# Même périmètre (séances résumables), même ordre, mêmes migrations.
	if (identical(store_backend(), "sqlite")) {
		seances <- store_sqlite_seances(user_id)
		seances <- Filter(function(s) !is.null(store_resume(s)), seances)

		if (!is.null(session_ids)) {
			seances <- Filter(function(s) as.character(s$session_id) %in% session_ids, seances)
		}

		seances <- lapply(seances, function(s) {
			tryCatch(
				{
					s <- store_migrer(s)
					s$user_id <- as.character(user_id)
					s
				},
				error = function(e) NULL
			)
		})

		return(seances[!vapply(seances, is.null, logical(1))])
	}

	liste <- store_lister(user_id = user_id)

	if (!is.null(session_ids)) {
		liste <- liste[liste$session_id %in% session_ids, , drop = FALSE]
	}

	if (!nrow(liste)) {
		return(list())
	}

	seances <- lapply(liste$session_id, function(id) {
		tryCatch(store_charger(id, user_id = user_id), error = function(e) NULL)
	})

	seances[!vapply(seances, is.null, logical(1))]
}

#' Réponses de plusieurs séances, en un tableau
#'
#' @param seances Liste de séances.
#'
#' @return Un tibble, éventuellement vide, avec `session_id` et `cree_le`.
#' @noRd
stats_reponses <- function(seances) {
	vide <- reponses_tibble(list())
	vide$session_id <- character(0)
	vide$cree_le <- as.POSIXct(character(0))

	if (!length(seances)) {
		return(vide)
	}

	blocs <- lapply(seances, function(s) {
		tableau <- reponses_tibble(s$reponses)

		if (!nrow(tableau)) {
			return(NULL)
		}

		tableau$session_id <- s$session_id
		tableau$cree_le <- as.POSIXct(s$cree_le)
		tableau
	})

	blocs <- blocs[!vapply(blocs, is.null, logical(1))]

	if (!length(blocs)) {
		return(vide)
	}

	do.call(rbind, blocs)
}

#' Synthèse globale
#'
#' @param reponses Tableau issu de [stats_reponses()].
#' @param n_seances Nombre de séances du périmètre.
#'
#' @return Un tibble d'une ligne.
#' @noRd
stats_synthese <- function(reponses, n_seances = 0L) {
	tibble::tibble(
		n_seances = as.integer(n_seances),
		n_questions = nrow(reponses),
		reussite = if (nrow(reponses)) mean(reponses$score) else NA_real_,
		temps_total_s = if (nrow(reponses)) sum(reponses$duree_s, na.rm = TRUE) else 0,
		temps_median_s = if (nrow(reponses)) stats::median(reponses$duree_s, na.rm = TRUE) else NA_real_
	)
}

#' Agrégation par une colonne de découpage
#'
#' @param reponses Tableau de réponses.
#' @param colonne Nom de la colonne (`theme`, `sub_theme`, `type`, `difficulte`).
#'
#' @return Un tibble trié du moins réussi au mieux réussi.
#' @noRd
stats_par <- function(reponses, colonne) {
	vide <- tibble::tibble(
		cle = character(0),
		n = integer(0),
		n_justes = integer(0),
		reussite = numeric(0),
		temps_median_s = numeric(0)
	)

	if (!nrow(reponses) || !colonne %in% names(reponses)) {
		return(vide)
	}

	cles <- as.character(reponses[[colonne]])
	groupes <- split(reponses, cles)

	lignes <- lapply(names(groupes), function(nom) {
		bloc <- groupes[[nom]]

		tibble::tibble(
			cle = nom,
			n = nrow(bloc),
			n_justes = as.integer(sum(bloc$is_correct, na.rm = TRUE)),
			reussite = mean(bloc$score),
			temps_median_s = stats::median(bloc$duree_s, na.rm = TRUE)
		)
	})

	tableau <- do.call(rbind, lignes)
	tableau[order(tableau$reussite, -tableau$n), , drop = FALSE]
}

#' Évolution du score d'une séance à l'autre
#'
#' @param seances Liste de séances, déjà triée ou non.
#'
#' @return Un tibble `cree_le`, `session_id`, `libelle`, `reussite`, `n`.
#' @noRd
stats_evolution <- function(seances) {
	vide <- tibble::tibble(
		cree_le = as.POSIXct(character(0)),
		session_id = character(0),
		libelle = character(0),
		reussite = numeric(0),
		n = integer(0)
	)

	if (!length(seances)) {
		return(vide)
	}

	lignes <- lapply(seances, function(s) {
		r <- reponses_tibble(s$reponses)

		tibble::tibble(
			cree_le = as.POSIXct(s$cree_le),
			session_id = s$session_id,
			libelle = as.character(s$libelle %||% s$session_id),
			reussite = if (nrow(r)) mean(r$score) else NA_real_,
			n = nrow(r)
		)
	})

	tableau <- do.call(rbind, lignes)
	tableau[order(tableau$cree_le), , drop = FALSE]
}

#' Coût et volume LLM d'une séance
#'
#' @param seance Une séance.
#'
#' @return Le tibble d'une ligne de [llm_synthese_journal()].
#' @noRd
stats_cout_seance <- function(seance) {
	llm_synthese_journal(seance$journal_llm %||% list())
}

#' Libellé lisible d'une confusion
#'
#' @param tag Identifiant `diagnostic_tag`.
#' @param bank Banque chargée.
#'
#' @return Une phrase courte, ou le tag humanisé.
#' @noRd
stats_libelle_confusion <- function(tag, bank = bank_load()) {
	tag <- as.character(tag[[1L]] %||% "")

	if (!nzchar(tag)) {
		return("")
	}

	for (i in seq_len(nrow(bank$graines))) {
		erreurs <- bank$graines$erreurs[[i]]

		if (is.null(erreurs) || !nrow(erreurs)) {
			next
		}

		rang <- match(tag, erreurs$diagnostic_tag)

		if (!is.na(rang)) {
			return(as.character(erreurs$description[[rang]]))
		}
	}

	gsub("_", " ", tag, fixed = TRUE)
}

#' Confusions les plus fréquentes, avec libellé
#'
#' @param reponses Liste brute de réponses, ou tableau (ignoré pour les tags).
#' @param brutes Liste de réponses d'origine, si `reponses` est un tableau.
#'
#' @return Un tibble `diagnostic_tag`, `libelle`, `n`.
#' @noRd
stats_confusions <- function(brutes) {
	tableau <- reponses_confusions(brutes)

	if (!nrow(tableau)) {
		return(tibble::tibble(
			diagnostic_tag = character(0),
			libelle = character(0),
			n = integer(0)
		))
	}

	tableau$libelle <- vapply(tableau$diagnostic_tag, stats_libelle_confusion, character(1))
	tableau$statut <- ifelse(tableau$n >= 2L, "R\u00e9currente", "\u00c0 confirmer")
	tableau
}

#' Analyse des temps de réponse
#'
#' @param reponses Tableau issu de [reponses_tibble()] ou [stats_reponses()].
#'
#' @return Une liste `median_s`, `latence_median_s`, `plus_lentes`, `reussite_lentes`.
#' @noRd
stats_temps <- function(reponses) {
	vide <- tibble::tibble(
		sub_theme = character(0),
		theme = character(0),
		duree_s = numeric(0),
		is_correct = logical(0)
	)

	if (!nrow(reponses)) {
		return(list(
			median_s = NA_real_,
			latence_median_s = NA_real_,
			plus_lentes = vide,
			reussite_lentes = NA_real_,
			reussite_rapides = NA_real_
		))
	}

	ordre <- order(reponses$duree_s, decreasing = TRUE, na.last = TRUE)
	lentes <- reponses[utils::head(ordre, 5L), , drop = FALSE]
	seuil <- stats::median(reponses$duree_s, na.rm = TRUE)

	list(
		median_s = seuil,
		latence_median_s = stats::median(reponses$latence_s, na.rm = TRUE),
		plus_lentes = lentes,
		reussite_lentes = mean(reponses$score[reponses$duree_s >= seuil], na.rm = TRUE),
		reussite_rapides = mean(reponses$score[reponses$duree_s < seuil], na.rm = TRUE)
	)
}

#' Carte de chaleur sous-thème × séance
#'
#' @param reponses Tableau issu de [stats_reponses()].
#'
#' @return Un tibble `sub_theme`, `session_id`, `reussite`, `n`.
#' @noRd
stats_chaleur <- function(reponses) {
	vide <- tibble::tibble(
		sub_theme = character(0),
		session_id = character(0),
		reussite = numeric(0),
		n = integer(0)
	)

	if (!nrow(reponses) || !"session_id" %in% names(reponses)) {
		return(vide)
	}

	cles <- paste(reponses$sub_theme, reponses$session_id, sep = "\r")
	groupes <- split(reponses, cles)

	lignes <- lapply(groupes, function(bloc) {
		tibble::tibble(
			sub_theme = bloc$sub_theme[[1L]],
			session_id = bloc$session_id[[1L]],
			reussite = mean(bloc$score),
			n = nrow(bloc)
		)
	})

	do.call(rbind, lignes)
}

#' Instantané envoyé au synthétiseur (jamais les réponses brutes)
#'
#' @param seance Une séance.
#'
#' @return Une liste de tableaux agrégés.
#' @noRd
stats_snapshot_seance <- function(seance) {
	reponses <- reponses_tibble(seance$reponses)
	cout <- stats_cout_seance(seance)
	temps <- stats_temps(reponses)

	list(
		n_questions = nrow(reponses),
		reussite = if (nrow(reponses)) mean(reponses$score) else NA_real_,
		temps_median_s = temps$median_s,
		abandonnee = identical(seance$statut, "abandonnee"),
		par_theme = stats_par(reponses, "theme"),
		par_sous_theme = stats_par(reponses, "sub_theme"),
		par_type = stats_par(reponses, "type"),
		par_difficulte = stats_par(reponses, "difficulte"),
		par_difficulte_graine = if (
			nrow(reponses) && "difficulte_graine" %in% names(reponses)
		) {
			rg <- reponses[!is.na(reponses$difficulte_graine), , drop = FALSE]
			if (nrow(rg)) {
				rg$bande <- bank_difficulte_bande(rg$difficulte_graine)
				stats_par(rg, "bande")
			} else {
				stats_par(reponses[0, , drop = FALSE], "theme")
			}
		} else {
			stats_par(reponses[0, , drop = FALSE], "theme")
		},
		confusions = stats_confusions(seance$reponses),
		plus_lentes = temps$plus_lentes,
		cout_usd = cout$cout_usd[[1L]]
	)
}

#' Texte d'un snapshot, pour le prompt du synthétiseur
#'
#' @param snapshot Instantané de [stats_snapshot_seance()].
#'
#' @return Une chaîne de caractères.
#' @noRd
stats_snapshot_texte <- function(snapshot) {
	ligne <- function(tableau, nom) {
		if (!nrow(tableau)) {
			return(glue::glue("{nom} : (aucun)"))
		}

		paste(
			c(
				glue::glue("{nom} :"),
				glue::glue(
					"- {tableau$cle} : {round(100 * tableau$reussite)} % ({tableau$n} q.)"
				)
			),
			collapse = "\n"
		)
	}

	conf <- snapshot$confusions
	conf_txt <- if (!nrow(conf)) {
		"Aucune confusion r\u00e9currente."
	} else {
		paste(
			glue::glue("- {conf$libelle} ({conf$n})"),
			collapse = "\n"
		)
	}

	paste(
		c(
			glue::glue(
				"R\u00e9ussite globale : {round(100 * (snapshot$reussite %||% 0))} % ",
				"sur {snapshot$n_questions} question(s)."
			),
			if (isTRUE(snapshot$abandonnee)) "La s\u00e9ance a \u00e9t\u00e9 abandonn\u00e9e avant la fin." else NULL,
			glue::glue("Temps m\u00e9dian : {round(snapshot$temps_median_s %||% NA_real_)} s."),
			ligne(snapshot$par_theme, "Par th\u00e8me"),
			ligne(snapshot$par_sous_theme, "Par sous-th\u00e8me"),
			ligne(snapshot$par_type, "Par type"),
			ligne(snapshot$par_difficulte, "Par niveau de s\u00e9ance (1\u20135)"),
			ligne(snapshot$par_difficulte_graine, "Par difficult\u00e9 des graines (bandes /10)"),
			"Confusions :",
			conf_txt
		),
		collapse = "\n"
	)
}

#' Lecture condensée d'un périmètre
#'
#' @param reponses Tableau de [stats_reponses()].
#' @param seances Liste de séances du périmètre.
#'
#' @return Une liste d'indicateurs pour le tableau de bord.
#' @noRd
stats_lecture <- function(reponses, seances) {
	evo <- stats_evolution(seances)
	par_st <- stats_par(reponses, "sub_theme")
	n_evo <- nrow(evo)
	delta <- if (
		n_evo >= 2L &&
			is.finite(evo$reussite[[n_evo]]) &&
			is.finite(evo$reussite[[n_evo - 1L]])
	) {
		evo$reussite[[n_evo]] - evo$reussite[[n_evo - 1L]]
	} else {
		NA_real_
	}

	par_type <- stats_par(reponses, "type")
	n_justes <- if (nrow(reponses)) sum(reponses$is_correct, na.rm = TRUE) else 0L
	n_passees <- if (nrow(reponses) && "passee" %in% names(reponses)) {
		sum(reponses$passee, na.rm = TRUE)
	} else {
		0L
	}
	n_partielles <- if (nrow(reponses)) {
		as.integer(sum(
			!reponses$is_correct & !reponses$passee & reponses$score > 0,
			na.rm = TRUE
		))
	} else {
		0L
	}
	n_fausses <- if (nrow(reponses)) {
		as.integer(sum(
			!reponses$is_correct & !reponses$passee & reponses$score <= 0,
			na.rm = TRUE
		))
	} else {
		0L
	}
	tendance <- if (
		n_evo >= 2L &&
			is.finite(evo$reussite[[1L]]) &&
			is.finite(evo$reussite[[n_evo]])
	) {
		evo$reussite[[n_evo]] - evo$reussite[[1L]]
	} else {
		NA_real_
	}
	faible_type <- if (nrow(par_type)) par_type[1L, , drop = FALSE] else NULL

	if (!is.null(faible_type) && faible_type$cle[[1L]] %in% question_type_ids()) {
		faible_type$libelle <- question_type(faible_type$cle[[1L]])$libelle
	}

	list(
		synthese = stats_synthese(reponses, length(seances)),
		evolution = evo,
		delta = delta,
		tendance = tendance,
		faible = if (nrow(par_st)) par_st[1L, , drop = FALSE] else NULL,
		fort = if (nrow(par_st)) par_st[nrow(par_st), , drop = FALSE] else NULL,
		faible_type = faible_type,
		progres = stats_progres(reponses),
		n_sous_themes = nrow(par_st),
		n_justes = as.integer(n_justes),
		n_fausses = n_fausses,
		n_partielles = n_partielles,
		n_passees = as.integer(n_passees),
		temps = stats_temps(reponses)
	)
}

#' Sous-thème qui a le plus progressé entre le début et la fin du périmètre
#'
#' @param reponses Tableau de [stats_reponses()].
#'
#' @return Un tibble d'une ligne, ou `NULL`.
#' @noRd
stats_progres <- function(reponses) {
	if (
		!nrow(reponses) ||
			!"session_id" %in% names(reponses) ||
			!"cree_le" %in% names(reponses)
	) {
		return(NULL)
	}

	ordre <- order(reponses$cree_le)
	sessions <- unique(reponses$session_id[ordre])

	if (length(sessions) < 2L) {
		return(NULL)
	}

	coupure <- max(1L, floor(length(sessions) / 2))
	debut <- sessions[seq_len(coupure)]
	fin <- sessions[(coupure + 1L):length(sessions)]
	avant <- stats_par(reponses[reponses$session_id %in% debut, , drop = FALSE], "sub_theme")
	apres <- stats_par(reponses[reponses$session_id %in% fin, , drop = FALSE], "sub_theme")
	commun <- intersect(avant$cle, apres$cle)

	if (!length(commun)) {
		return(NULL)
	}

	deltas <- vapply(commun, function(cle) {
		apres$reussite[match(cle, apres$cle)] - avant$reussite[match(cle, avant$cle)]
	}, numeric(1))
	rang <- which.max(deltas)

	if (!length(rang) || !is.finite(deltas[[rang]]) || deltas[[rang]] <= 0) {
		return(NULL)
	}

	cle <- commun[[rang]]

	tibble::tibble(
		cle = cle,
		avant = avant$reussite[match(cle, avant$cle)],
		apres = apres$reussite[match(cle, apres$cle)],
		delta = deltas[[rang]]
	)
}

#' Issue d'une r\u00e9ponse : juste, partielle, fausse ou pass\u00e9e
#'
#' @param reponses Tableau de r\u00e9ponses.
#'
#' @return Un vecteur de cha\u00eenes.
#' @noRd
stats_issue <- function(reponses) {
	if (!nrow(reponses)) {
		return(character(0))
	}

	passee <- if ("passee" %in% names(reponses)) {
		reponses$passee
	} else {
		rep(FALSE, nrow(reponses))
	}

	ifelse(
		passee,
		"passee",
		ifelse(
			reponses$is_correct,
			"juste",
			ifelse(reponses$score > 0, "partielle", "fausse")
		)
	)
}

#' Ton d'une case de chaleur
#'
#' @param reussite Taux entre 0 et 1, ou `NA`.
#'
#' @return Un identifiant de classe.
#' @noRd
stats_chaleur_ton <- function(reussite) {
	if (!is.finite(reussite)) {
		return("vide")
	}

	if (reussite < 0.4) {
		"bas"
	} else if (reussite < 0.55) {
		"fragile"
	} else if (reussite < 0.75) {
		"moyen"
	} else {
		"haut"
	}
}

#' Exporter un tableau en CSV
#'
#' @param reponses Tableau \u00e0 \u00e9crire (r\u00e9ponses brutes ou export s\u00e9ance).
#' @param chemin Fichier de destination.
#'
#' @return `chemin`, invisiblement.
#' @noRd
stats_export_csv <- function(reponses, chemin) {
	utils::write.csv(reponses, chemin, row.names = FALSE, fileEncoding = "UTF-8")
	invisible(chemin)
}

#' Exporter une synth\u00e8se en XLSX
#'
#' @param reponses Tableau de r\u00e9ponses.
#' @param chemin Fichier de destination.
#' @param seance Si `TRUE`, une seule feuille p\u00e9dagogique (pas de stats
#'   agr\u00e9g\u00e9es ni de colonnes techniques).
#'
#' @return `chemin`, invisiblement.
#' @noRd
stats_export_xlsx <- function(reponses, chemin, seance = FALSE) {
	feuilles <- if (isTRUE(seance)) {
		list(questions = reponses)
	} else {
		list(
			reponses = reponses,
			themes = stats_par(reponses, "theme"),
			sous_themes = stats_par(reponses, "sub_theme"),
			types = stats_par(reponses, "type")
		)
	}

	writexl::write_xlsx(feuilles, chemin)
	invisible(chemin)
}

#' Rapport HTML autonome
#'
#' @param titre Titre du document.
#' @param synthese Ligne de [stats_synthese()].
#' @param par_theme,par_sous_theme,par_type Tableaux de [stats_par()].
#' @param chemin Fichier de destination.
#'
#' @return `chemin`, invisiblement.
#' @noRd
stats_export_html <- function(titre,
															synthese,
															par_theme,
															par_sous_theme,
															par_type,
															chemin) {
	esc <- function(x) {
		x <- as.character(x)
		x <- gsub("&", "&amp;", x, fixed = TRUE)
		x <- gsub("<", "&lt;", x, fixed = TRUE)
		x <- gsub(">", "&gt;", x, fixed = TRUE)
		x
	}

	pct <- function(x) if (!is.finite(x)) "\u2014" else paste0(round(100 * x), " %")
	ligne_tab <- function(t) {
		if (!nrow(t)) {
			return("<tr><td colspan='3'>Aucune donn\u00e9e</td></tr>")
		}

		paste0(
			"<tr><td>", esc(t$cle), "</td><td>",
			pct(t$reussite), "</td><td>", t$n, "</td></tr>",
			collapse = ""
		)
	}

	html <- paste0(
		"<!DOCTYPE html><html lang='fr'><head><meta charset='utf-8'><title>",
		esc(titre),
		"</title><style>body{font-family:system-ui,sans-serif;max-width:46rem;",
		"margin:2rem auto;padding:0 1rem;color:#1b1f24;} table{width:100%;",
		"border-collapse:collapse;} th,td{text-align:left;padding:.4rem .5rem;",
		"border-bottom:1px solid #e3e6ea;} h1{color:#1d3557;}</style></head><body>",
		"<h1>", htmltools::htmlEscape(titre), "</h1>",
		"<p>", synthese$n_seances, " s\u00e9ance(s), ", synthese$n_questions,
		" question(s), r\u00e9ussite ", pct(synthese$reussite), ".</p>",
		"<h2>Th\u00e8mes</h2><table><tr><th>Th\u00e8me</th><th>R\u00e9ussite</th><th>Volume</th></tr>",
		ligne_tab(par_theme), "</table>",
		"<h2>Sous-th\u00e8mes</h2><table><tr><th>Sous-th\u00e8me</th><th>R\u00e9ussite</th><th>Volume</th></tr>",
		ligne_tab(par_sous_theme), "</table>",
		"<h2>Types</h2><table><tr><th>Type</th><th>R\u00e9ussite</th><th>Volume</th></tr>",
		ligne_tab(par_type), "</table>",
		"</body></html>"
	)

	writeLines(html, chemin, useBytes = TRUE)
	invisible(chemin)
}
