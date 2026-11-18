# Régleur adaptatif.
#
# Décision validée : l'adaptation est automatique et invisible, l'utilisateur ne
# choisit pas de niveau. La difficulté est donc entièrement déduite de ses
# réponses : le niveau visé avance lot par lot, l'ordre des questions d'un lot
# s'ajuste après chaque réponse.
#
# Le réglage est déterministe et ne consomme aucun appel LLM. Ce choix est
# volontaire : une règle arithmétique se teste, s'explique à l'utilisateur dans
# le rapport de séance, et ne coûte rien. Le modèle n'intervient que pour
# rédiger les questions à la difficulté demandée.

#' Règles du régleur adaptatif
#'
#' @return La section `adaptatif` de la configuration.
#' @noRd
adapt_regles <- function() {
	cq_config("adaptatif")
}

#' État initial du régleur
#'
#' @param regles Règles du régleur.
#'
#' @return Une liste d'état.
#' @noRd
adapt_etat_initial <- function(regles = adapt_regles()) {
	list(
		difficulte = as.integer(regles$difficulte_initiale),
		erreurs_consecutives = 0L,
		confiance_restantes = 0L,
		taux_dernier_lot = NA_real_,
		n_reponses = 0L,
		fenetre = numeric(0),
		reponses_integrees = character(0),
		historique_difficulte = integer(0)
	)
}

#' Score d'une réponse, entre 0 et 1
#'
#' Le score partiel (QCM à plusieurs réponses, remise en ordre) est plus
#' informatif que le seul verdict juste/faux. Une réponse passée vaut 0.
#'
#' @param reponse Une réponse.
#'
#' @return Un nombre.
#' @noRd
adapt_score <- function(reponse) {
	score <- suppressWarnings(as.numeric(reponse$score %||% NA_real_))

	if (length(score) != 1L || is.na(score)) {
		return(as.numeric(isTRUE(reponse$is_correct)))
	}

	min(1, max(0, score))
}

#' Mettre à jour le régleur au vu de nouvelles réponses
#'
#' Trois mécanismes, dans cet ordre de priorité :
#'
#' 1. Une série d'erreurs consécutives déclenche une descente immédiate et
#'    quelques questions de remise en confiance. Elle prime sur le taux de
#'    réussite, car enchaîner les échecs décourage même si la moyenne reste
#'    correcte.
#' 2. Pendant la remise en confiance, la difficulté ne bouge pas : il faut
#'    laisser l'utilisateur réussir avant de le solliciter à nouveau.
#' 3. Sinon, le taux de réussite fait monter ou descendre d'un cran, mais
#'    seulement une fois `echantillon_min` réponses accumulées. Les réponses
#'    d'un échantillon trop petit restent dans la fenêtre et comptent à la
#'    décision suivante : aucune n'est perdue, aucune ne compte deux fois.
#'
#' @param etat État courant.
#' @param reponses Liste de réponses, avec `is_correct` et si possible `score`.
#' @param regles Règles du régleur.
#'
#' @return L'état mis à jour.
#' @noRd
adapt_maj <- function(etat, reponses, regles = adapt_regles()) {
	if (!length(reponses)) {
		return(etat)
	}

	justes <- vapply(reponses, function(r) isTRUE(r$is_correct), logical(1))
	scores <- vapply(reponses, adapt_score, numeric(1))

	for (juste in justes) {
		etat$erreurs_consecutives <- if (juste) 0L else etat$erreurs_consecutives + 1L
	}

	etat$n_reponses <- etat$n_reponses + length(justes)
	fenetre <- c(etat$fenetre %||% numeric(0), scores)
	etat$taux_dernier_lot <- mean(fenetre)

	if (etat$erreurs_consecutives >= as.integer(regles$erreurs_consecutives_max)) {
		etat$difficulte <- max(
			as.integer(regles$difficulte_min),
			etat$difficulte - 1L
		)
		etat$confiance_restantes <- as.integer(regles$questions_remise_en_confiance)
		etat$erreurs_consecutives <- 0L
		etat$fenetre <- numeric(0)

		return(etat)
	}

	if (etat$confiance_restantes > 0L) {
		etat$confiance_restantes <- max(0L, etat$confiance_restantes - length(justes))
		etat$fenetre <- numeric(0)

		return(etat)
	}

	if (length(fenetre) < as.integer(regles$echantillon_min %||% 4L)) {
		etat$fenetre <- fenetre

		return(etat)
	}

	etat$fenetre <- numeric(0)

	if (etat$taux_dernier_lot >= regles$seuil_montee) {
		etat$difficulte <- min(as.integer(regles$difficulte_max), etat$difficulte + 1L)
	} else if (etat$taux_dernier_lot <= regles$seuil_descente) {
		etat$difficulte <- max(as.integer(regles$difficulte_min), etat$difficulte - 1L)
	}

	etat
}

#' Difficultés à viser pour les questions d'une tranche
#'
#' Les questions de remise en confiance sont placées en tête de tranche, un cran
#' sous le niveau courant. Hors remise en confiance, la palette suit le rang
#' dans la séance, et non la taille de la tranche : une sonde un cran au-dessus
#' toutes les `sonde_tous_les` questions, pour détecter qu'un utilisateur est
#' prêt à monter ; un filet un cran en dessous au rang `filet_rang` de chaque
#' dizaine. Sonde et filet donnent à l'aiguillage ([adapt_aiguiller()]) de quoi
#' réagir sans attendre la tranche suivante.
#'
#' @param etat État courant.
#' @param taille Nombre de questions de la tranche.
#' @param regles Règles du régleur.
#' @param indices Rangs des questions dans la séance.
#'
#' @return Un vecteur d'entiers de longueur `taille`.
#' @noRd
adapt_difficulte_lot <- function(etat, taille, regles = adapt_regles(), indices = seq_len(taille)) {
	taille <- as.integer(taille)

	if (taille < 1L) {
		return(integer(0))
	}

	mini <- as.integer(regles$difficulte_min)
	maxi <- as.integer(regles$difficulte_max)
	indices <- as.integer(indices)

	difficultes <- rep(etat$difficulte, taille)
	n_confiance <- min(taille, as.integer(etat$confiance_restantes))

	if (n_confiance > 0L) {
		difficultes[seq_len(n_confiance)] <- max(mini, etat$difficulte - 1L)
	} else {
		sonde <- indices %% as.integer(regles$sonde_tous_les %||% 5L) == 0L
		filet <- !sonde & indices %% 10L == as.integer(regles$filet_rang %||% 7L) %% 10L

		difficultes[sonde] <- min(maxi, etat$difficulte + 1L)
		difficultes[filet] <- max(mini, etat$difficulte - 1L)
	}

	as.integer(pmin(maxi, pmax(mini, difficultes)))
}

#' Choisir la prochaine question parmi celles qui restent dans le lot
#'
#' Adaptation question par question, sans appel au modèle : le lot est déjà
#' écrit avec quelques niveaux d'écart (remise en confiance, filet, sonde), et
#' l'aiguillage choisit seulement l'ordre de passation. Après
#' `aiguillage_erreurs` erreurs d'affilée, la question la plus facile passe
#' devant ; après `aiguillage_reussites` réussites d'affilée, la plus
#' difficile. Sinon l'ordre prévu est conservé. À difficulté égale, on évite
#' de reprendre le sous-thème de la question précédente.
#'
#' @param difficultes Difficultés des questions restantes, dans l'ordre prévu.
#' @param sous_themes Sous-thèmes correspondants.
#' @param resultats Verdicts des réponses déjà données, dans l'ordre.
#' @param dernier_sous_theme Sous-thème de la dernière question posée.
#' @param regles Règles du régleur.
#'
#' @return `list(rang, motif)` : la position, dans les questions restantes, de
#'   celle à poser maintenant, et `"prevu"`, `"facile"` ou `"difficile"`.
#' @noRd
adapt_aiguiller <- function(difficultes,
														sous_themes,
														resultats,
														dernier_sous_theme = NA_character_,
														regles = adapt_regles()) {
	prevu <- list(rang = 1L, motif = "prevu")
	difficultes <- as.integer(difficultes)
	resultats <- as.logical(resultats)

	if (length(difficultes) < 2L || !length(resultats)) {
		return(prevu)
	}

	n_bas <- as.integer(regles$aiguillage_erreurs %||% 2L)
	n_haut <- as.integer(regles$aiguillage_reussites %||% 3L)
	serie <- function(n, valeur) {
		n > 0L && length(resultats) >= n && all(utils::tail(resultats, n) == valeur)
	}

	if (serie(n_bas, FALSE)) {
		cible <- min(difficultes)
		motif <- "facile"
	} else if (serie(n_haut, TRUE)) {
		cible <- max(difficultes)
		motif <- "difficile"
	} else {
		return(prevu)
	}

	if (cible == difficultes[[1L]]) {
		return(prevu)
	}

	candidats <- which(difficultes == cible)
	varies <- candidats[!as.character(sous_themes[candidats]) %in% as.character(dernier_sous_theme)]

	list(rang = if (length(varies)) varies[[1L]] else candidats[[1L]], motif = motif)
}

#' Réordonner un lot pour éviter les répétitions de sous-thème
#'
#' Règle anti-lassitude : pas plus de `max_consecutif_meme_sous_theme` questions
#' de suite sur le même sous-thème. L'algorithme est glouton, il place à chaque
#' rang le sous-thème le plus représenté parmi ceux qui restent admissibles ;
#' faute de candidat admissible, il accepte une répétition plutôt que d'échouer.
#'
#' @param sous_themes Vecteur des sous-thèmes, dans l'ordre initial.
#' @param maxi Nombre maximal de répétitions consécutives.
#'
#' @return Un vecteur de positions, permutation de `seq_along(sous_themes)`.
#' @noRd
adapt_ordre_anti_lassitude <- function(sous_themes, maxi = adapt_regles()$max_consecutif_meme_sous_theme) {
	n <- length(sous_themes)
	maxi <- max(1L, as.integer(maxi))

	if (n < 2L) {
		return(seq_len(n))
	}

	restants <- seq_len(n)
	ordre <- integer(0)
	dernier <- NA_character_
	serie <- 0L

	while (length(restants)) {
		candidats <- restants

		if (!is.na(dernier) && serie >= maxi) {
			admissibles <- restants[sous_themes[restants] != dernier]

			if (length(admissibles)) {
				candidats <- admissibles
			}
		}

		# Servir d'abord le sous-thème le plus abondant limite le risque de se
		# retrouver en fin de lot avec un seul sous-thème à placer en série.
		effectifs <- table(sous_themes[candidats])
		vise <- names(effectifs)[which.max(effectifs)]
		choix <- candidats[sous_themes[candidats] == vise][[1L]]

		ordre <- c(ordre, choix)
		restants <- setdiff(restants, choix)

		if (identical(sous_themes[[choix]], dernier)) {
			serie <- serie + 1L
		} else {
			dernier <- sous_themes[[choix]]
			serie <- 1L
		}
	}

	ordre
}

#' Temps de réponse rapporté à la référence du type
#'
#' @param reponse Une réponse d'utilisateur.
#'
#' @return Un nombre, ou `NA` si le temps n'est pas mesurable.
#' @noRd
adapt_temps_relatif <- function(reponse) {
	duree <- as.numeric(reponse$duree_ms %||% NA_real_) / 1000
	type <- as.character(reponse$type %||% "")
	ref <- if (nzchar(type) && type %in% question_type_ids()) {
		as.numeric(question_type(type)$temps_ref_s)
	} else {
		30
	}

	if (!is.finite(duree) || !is.finite(ref) || ref <= 0) {
		return(NA_real_)
	}

	duree / ref
}

#' Poids thématiques déduits des réponses
#'
#' Levier de `bank_seeds()` : un sous-thème raté est surpondéré, un sous-thème
#' réussi très vite est dépondéré, le reste reste à 1. Sans identifiant de
#' sous-thème, on n'invente pas de poids. Seules les `fenetre` réponses les plus
#' récentes de chaque sous-thème comptent : un sous-thème raté il y a trois
#' séances puis rattrapé ne doit plus être ciblé.
#'
#' @param reponses Liste de réponses.
#' @param fenetre Nombre de réponses récentes retenues par sous-thème.
#'
#' @return Un vecteur nommé par `full_theme_id`, ou `NULL`.
#' @noRd
adapt_poids_themes <- function(reponses, fenetre = adapt_regles()$poids_fenetre %||% 6L) {
	if (!length(reponses)) {
		return(NULL)
	}

	horodatages <- vapply(
		reponses,
		function(r) suppressWarnings(as.numeric(r$repondu_le %||% NA_real_))[[1L]],
		numeric(1)
	)

	if (!anyNA(horodatages)) {
		reponses <- reponses[order(horodatages)]
	}

	ids <- vapply(
		reponses,
		function(r) as.character(r$full_theme_id %||% ""),
		character(1)
	)
	utiles <- nzchar(ids)

	if (!any(utiles)) {
		return(NULL)
	}

	groupes <- split(reponses[utiles], ids[utiles])
	fenetre <- max(1L, as.integer(fenetre))

	vapply(groupes, function(bloc) {
		bloc <- utils::tail(bloc, fenetre)
		taux <- mean(vapply(bloc, function(r) isTRUE(r$is_correct), logical(1)))
		rapport <- mean(vapply(bloc, adapt_temps_relatif, numeric(1)), na.rm = TRUE)
		tags <- unlist(
			lapply(bloc, function(r) as.character(r$diagnostic_tags %||% character(0))),
			use.names = FALSE
		)
		tags <- tags[!is.na(tags) & nzchar(tags)]
		confusion_recurrente <- length(tags) > 0L && any(table(tags) >= 2L)

		poids <- if (taux < 0.55) {
			2
		} else if (taux >= 0.85 && is.finite(rapport) && rapport < 0.5) {
			0.5
		} else {
			1
		}

		# Une même confusion observée deux fois est un signal plus précis qu'un
		# simple taux d'échec. Elle renforce le sous-thème, sans qu'une erreur
		# isolée suffise à piloter la séance.
		if (confusion_recurrente) max(poids, 2.5) else poids
	}, numeric(1))
}

#' Ajuster les types d'un lot selon le niveau
#'
#' En mode varié seulement : une séance trop facile échange un type simple
#' contre un type exigeant ; une remise en confiance ou une descente fait
#' l'inverse. Le mode QCM classique n'a qu'un type, il n'est pas touché.
#'
#' @param types Types déjà planifiés pour le lot.
#' @param etat État du régleur.
#' @param disponibles Types autorisés par les paramètres.
#'
#' @return Le vecteur de types, éventuellement modifié.
#' @noRd
adapt_ajuster_types <- function(types, etat, disponibles) {
	types <- as.character(types)
	disponibles <- as.character(disponibles)
	exigeants <- intersect(c("ordre", "qcm_multiple"), disponibles)
	simples <- intersect(c("vrai_faux", "qcm_single"), disponibles)

	if (etat$confiance_restantes > 0L || etat$difficulte <= 2L) {
		if (length(simples)) {
			rang <- which(types %in% exigeants)[1L]

			if (!is.na(rang)) {
				types[[rang]] <- simples[[1L]]
			}
		}

		return(types)
	}

	if (etat$difficulte >= 4L && length(exigeants)) {
		rang <- which(types %in% simples)[1L]

		if (!is.na(rang)) {
			types[[rang]] <- exigeants[[1L]]
		}
	}

	types
}
