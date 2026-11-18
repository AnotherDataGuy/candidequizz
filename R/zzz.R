# Peuplement des registres au chargement du package.
#
# Les registres sont remplis ici et nulle part ailleurs. Ce fichier est donc la
# table des matières de l'application : la liste des types de questions
# disponibles et le schéma d'agents s'y lisent d'un coup d'œil, et ajouter une
# brique se voit dans le diff.

#' Enregistrer les types de questions
#'
#' @return `NULL`, invisiblement.
#' @noRd
enregistrer_types_questions <- function() {
	register_question_type(
		id = "qcm_single",
		libelle = "QCM \u00e0 option unique",
		modes = c("qcm_classique", "autres_types"),
		n_options = 2:4,
		n_options_defaut = 4L,
		schema = qtype_qcm_single_schema,
		consigne = qtype_qcm_single_consigne,
		valider = qtype_qcm_single_valider,
		noter = qtype_qcm_single_noter,
		ui = qtype_qcm_single_ui,
		server = qtype_qcm_single_server,
		solution = qtype_qcm_single_solution,
		temps_ref_s = 25
	)

	register_question_type(
		id = "qcm_multiple",
		libelle = "QCM \u00e0 options multiples",
		modes = "autres_types",
		n_options = 4L,
		n_options_defaut = 4L,
		schema = qtype_qcm_multiple_schema,
		consigne = qtype_qcm_multiple_consigne,
		valider = qtype_qcm_multiple_valider,
		noter = qtype_qcm_multiple_noter,
		ui = qtype_qcm_multiple_ui,
		server = qtype_qcm_multiple_server,
		solution = qtype_qcm_multiple_solution,
		temps_ref_s = 45
	)

	register_question_type(
		id = "vrai_faux",
		libelle = "Vrai ou faux",
		modes = "autres_types",
		n_options = NULL,
		schema = qtype_vrai_faux_schema,
		consigne = qtype_vrai_faux_consigne,
		valider = qtype_vrai_faux_valider,
		noter = qtype_vrai_faux_noter,
		ui = qtype_vrai_faux_ui,
		server = qtype_vrai_faux_server,
		solution = qtype_vrai_faux_solution,
		temps_ref_s = 15
	)

	register_question_type(
		id = "ordre",
		libelle = "Remise en ordre",
		modes = "autres_types",
		n_options = 3:6,
		n_options_defaut = 4L,
		options_pilotees = FALSE,
		schema = qtype_ordre_schema,
		consigne = qtype_ordre_consigne,
		valider = qtype_ordre_valider,
		noter = qtype_ordre_noter,
		ui = qtype_ordre_ui,
		server = qtype_ordre_server,
		solution = qtype_ordre_solution,
		temps_ref_s = 60
	)

	register_question_type(
		id = "texte_a_trous",
		libelle = "Texte \u00e0 trous",
		modes = "autres_types",
		n_options = 1:3,
		n_options_defaut = 2L,
		options_pilotees = FALSE,
		schema = qtype_texte_a_trous_schema,
		consigne = qtype_texte_a_trous_consigne,
		valider = qtype_texte_a_trous_valider,
		noter = qtype_texte_a_trous_noter,
		ui = qtype_texte_a_trous_ui,
		server = qtype_texte_a_trous_server,
		solution = qtype_texte_a_trous_solution,
		temps_ref_s = 50
	)

	invisible(NULL)
}

#' Enregistrer les agents
#'
#' @return `NULL`, invisiblement.
#' @noRd
enregistrer_agents <- function() {
	register_agent(
		id = "planificateur",
		libelle = "A-1 Planificateur de s\u00e9ance",
		nature = "deterministe",
		fn = agent_planificateur,
		requiert = "parametres",
		produit = "plan",
		description = paste(
			"Traduit les param\u00e8tres en plan de s\u00e9ance : d\u00e9coupage en lots,",
			"r\u00e9partition des types et des options, \u00e9tat initial du r\u00e9gleur."
		)
	)

	register_agent(
		id = "regleur_adaptatif",
		libelle = "A-7 R\u00e9gleur adaptatif",
		nature = "deterministe",
		fn = agent_regleur_adaptatif,
		requiert = c("parametres", "plan"),
		produit = "plan",
		description = paste(
			"Avance l'\u00e9tat adaptatif d'apr\u00e8s les r\u00e9ponses du lot pr\u00e9c\u00e9dent",
			"et \u00e9met la directive du lot \u00e0 tirer : difficult\u00e9, poids, types."
		)
	)

	register_agent(
		id = "selecteur_graines",
		libelle = "A-2 S\u00e9lecteur de graines",
		nature = "deterministe",
		fn = agent_selecteur_graines,
		requiert = c("parametres", "plan", "bank"),
		produit = "consignes",
		description = paste(
			"Tire les graines du lot avec quotas par sous-th\u00e8me, fixe la",
			"difficult\u00e9 vis\u00e9e et compose une consigne par question."
		)
	)

	register_agent(
		id = "cache_questions",
		libelle = "A-2b Cache de questions v\u00e9rifi\u00e9es",
		nature = "deterministe",
		fn = agent_cache_questions,
		requiert = "consignes",
		produit = "items",
		description = paste(
			"Sert les questions d\u00e9j\u00e0 r\u00e9dig\u00e9es et relues sur la m\u00eame graine ;",
			"le r\u00e9dacteur n'\u00e9crit que ce qui manque."
		)
	)

	register_agent(
		id = "redacteur",
		libelle = "A-3 R\u00e9dacteur d'items",
		nature = "llm",
		role = "redacteur",
		fn = agent_redacteur,
		requiert = "consignes",
		produit = "items",
		description = paste(
			"R\u00e9dige les items, un appel par type de question, sous sch\u00e9ma",
			"strict. Se rabat sur les graines si le mod\u00e8le est indisponible."
		)
	)

	register_agent(
		id = "conformite",
		libelle = "A-4 Contr\u00f4leur de conformit\u00e9",
		nature = "deterministe",
		fn = agent_conformite,
		requiert = c("items", "consignes"),
		produit = c("items", "rejets"),
		description = paste(
			"V\u00e9rifie sans co\u00fbt la forme des items : comptages, unicit\u00e9 de la",
			"bonne r\u00e9ponse, doublons, r\u00e9ponse divulgu\u00e9e, contraintes du type."
		)
	)

	register_agent(
		id = "verificateur_factuel",
		libelle = "A-5 V\u00e9rificateur factuel",
		nature = "llm",
		role = "verificateur",
		fn = agent_verificateur_factuel,
		requiert = c("items", "rejets", "consignes"),
		produit = c("items", "rejets"),
		description = paste(
			"Juge l'exactitude et le p\u00e9rim\u00e8tre des items conformes, en un seul",
			"appel par lot sur un mod\u00e8le modeste."
		)
	)

	register_agent(
		id = "reparateur",
		libelle = "A-6 R\u00e9parateur",
		nature = "llm",
		role = "reparateur",
		fn = agent_reparateur,
		requiert = c("items", "rejets", "consignes"),
		produit = "items",
		description = paste(
			"Corrige les items rejet\u00e9s au vu du motif, en deux passes au plus,",
			"puis se rabat sur la graine pour ce qui r\u00e9siste."
		)
	)

	register_agent(
		id = "synthetiseur",
		libelle = "A-8 Synth\u00e9tiseur de s\u00e9ance",
		nature = "llm",
		role = "synthetiseur",
		fn = agent_synthetiseur,
		requiert = "synthese_stats",
		produit = "synthese",
		description = paste(
			"R\u00e9dige le diagnostic de fin de s\u00e9ance \u00e0 partir de statistiques",
			"d\u00e9j\u00e0 calcul\u00e9es, jamais des r\u00e9ponses brutes."
		)
	)

	invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
	enregistrer_types_questions()
	enregistrer_agents()

	invisible(NULL)
}
