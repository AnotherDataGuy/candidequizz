# Agent A-2 : sélecteur de graines.
#
# Agent déterministe. Il produit les consignes du lot : une par question, avec
# sa graine, son type, son nombre d'options et sa difficulté visée. C'est le
# seul agent qui décide quoi demander ; le rédacteur ne fait qu'exécuter.
#
# Le tirage reste du ressort du code R et non du modèle : il doit respecter des
# quotas par sous-thème, éviter les graines déjà servies et lisser les
# répétitions, ce qu'un tirage vérifiable fait mieux qu'un modèle.

#' Valeurs de vérité à demander pour les items « vrai ou faux »
#'
#' Laissé libre, un générateur produit très majoritairement des affirmations
#' vraies, et l'utilisateur apprend vite à répondre « vrai » sans réfléchir. On
#' impose donc l'alternance, en la décalant selon le rang de la question dans
#' la séance pour que deux lots successifs ne commencent pas pareil.
#'
#' @param indices Rangs des questions concernées dans la séance.
#'
#' @return Un vecteur logique.
#' @noRd
selecteur_verites <- function(indices) {
	indices %% 2L == 1L
}

#' Sélectionner les graines et composer les consignes d'un lot
#'
#' @param ctx Contexte de génération.
#'
#' @return Le contexte, `consignes` renseigné.
#' @noRd
agent_selecteur_graines <- function(ctx) {
	plan <- ctx$plan

	if (ctx$lot < 1L || ctx$lot > plan$n_lots) {
		rlang::abort(
			c(
				glue::glue("Lot {ctx$lot} hors du plan de s\u00e9ance."),
				i = glue::glue("La s\u00e9ance compte {plan$n_lots} lot(s).")
			),
			class = "candidequizz_pipeline_lot_inconnu"
		)
	}

	indices <- plan$indices_lots[[ctx$lot]]
	taille <- length(indices)

	adaptatif <- isTRUE(ctx$plan$directive$adaptatif %||% ctx$plan$adaptatif_actif)

	graines <- bank_seeds(
		n = taille,
		full_theme_ids = ctx$parametres$full_theme_ids,
		exclude_ids = ctx$exclure_seed_ids,
		poids = if (adaptatif) {
			ctx$plan$directive$poids_themes %||% ctx$parametres$poids_themes
		} else {
			NULL
		},
		difficulte = if (adaptatif) ctx$plan$etat_adaptatif$difficulte else NULL,
		bank = ctx$bank
	)

	ordre <- adapt_ordre_anti_lassitude(graines$full_theme_id)
	graines <- graines[ordre, , drop = FALSE]

	etat <- ctx$plan$etat_adaptatif
	difficultes <- if (adaptatif) {
		adapt_difficulte_lot(etat, taille, indices = indices)
	} else {
		rep(as.integer(etat$difficulte), taille)
	}

	types <- plan$types[indices]
	n_options <- plan$n_options[indices]
	verites <- selecteur_verites(indices)

	ctx$consignes <- lapply(seq_len(taille), function(i) {
		list(
			numero = i,
			indice_seance = indices[[i]],
			lot = ctx$lot,
			type = types[[i]],
			n_options = n_options[[i]],
			difficulte = difficultes[[i]],
			verite_attendue = verites[[i]],
			graine = graines[i, , drop = FALSE]
		)
	})

	ctx_detail(
		ctx,
		n_consignes = taille,
		difficulte = paste(unique(difficultes), collapse = "/"),
		types = paste(sort(unique(types)), collapse = ", ")
	)
}

#' Regrouper des consignes par type de question
#'
#' Le rédacteur et le réparateur appellent le modèle une fois par type. Chaque
#' schéma de sortie reste ainsi simple et strict, au lieu d'une union de types
#' que l'API applique mal ; et ajouter un type ne peut pas dégrader la
#' génération des autres.
#'
#' @param consignes Liste de consignes.
#'
#' @return Une liste de listes de consignes, nommée par type.
#' @noRd
consignes_par_type <- function(consignes) {
	types <- vapply(consignes, function(c) c$type, character(1))

	split(consignes, factor(types, levels = unique(types)))
}
