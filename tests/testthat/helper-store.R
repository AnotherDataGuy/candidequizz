# Chaque test de persistance travaille dans un répertoire temporaire, pour
# ne jamais toucher aux séances réelles de l'utilisateur.

local_store <- function(env = parent.frame()) {
	repertoire <- withr::local_tempdir(.local_envir = env)
	withr::local_envvar(
		c(
			CANDIDEQUIZZ_STORE_DIR = repertoire,
			CANDIDEQUIZZ_STORE_BACKEND = "rds"
		),
		.local_envir = env
	)

	repertoire
}

seance_test <- function(nb_questions = 10L, types = "qcm_single") {
	parametres <- parametres_seance(
		nb_questions = nb_questions,
		mode = "qcm_classique",
		n_options = 4L
	)

	plan <- list(
		nb_questions = nb_questions,
		tailles_lots = nb_questions,
		n_lots = 1L,
		indices_lots = list(seq_len(nb_questions)),
		types = rep(types, nb_questions),
		n_options = rep(4L, nb_questions),
		etat_adaptatif = adapt_etat_initial()
	)

	seance_nouvelle(parametres = parametres, plan = plan)
}
