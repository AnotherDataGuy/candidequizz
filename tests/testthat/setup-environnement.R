# Les tests ne dépendent pas de la session R qui les lance.
#
# dev/run_dev.R (VERSION "prod") laisse dans la session GOLEM_CONFIG_ACTIVE,
# CANDIDEQUIZZ_STORE_DIR, voire CANDIDEQUIZZ_DATABASE_URL : sans ce fichier,
# les tests tournaient alors avec la configuration en ligne (échecs
# trompeurs), et auraient pu écrire dans la vraie base. Ici, tout est remis à
# la configuration par défaut, dans un dossier temporaire, sans clé OpenAI ;
# restauré à la fin des tests. Chaque test peut ensuite régler ce qu'il veut.

withr::local_envvar(
	c(
		GOLEM_CONFIG_ACTIVE = NA,
		R_CONFIG_ACTIVE = NA,
		CANDIDEQUIZZ_AUTH_MODE = NA,
		CANDIDEQUIZZ_DATABASE_URL = NA,
		CANDIDEQUIZZ_STORE_BACKEND = NA,
		CANDIDEQUIZZ_ADMIN_USER = NA,
		CANDIDEQUIZZ_ADMIN_PASSWORD = NA,
		OPENAI_API_KEY = NA,
		CANDIDEQUIZZ_STORE_DIR = withr::local_tempdir(.local_envir = testthat::teardown_env())
	),
	.local_envir = testthat::teardown_env()
)
