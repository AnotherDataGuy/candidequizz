#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(
	...
) {
  system.file(..., package = "candidequizz")
}


# Fichier de configuration déjà lu, une entrée par couple (configuration
# active, chemin du fichier). `config::get()` relit et analyse le YAML à
# chaque appel ; or `cq_config()` est appelé partout, y compris avant chaque
# requête SQL (via `store_backend()`). Conséquence : une modification de
# `golem-config.yml` demande de relancer l'application (ou `load_all()`).
.golem_config_cache <- new.env(parent = emptyenv())

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
	value,
	config = Sys.getenv(
		"GOLEM_CONFIG_ACTIVE",
		Sys.getenv(
			"R_CONFIG_ACTIVE",
			"default"
		)
	),
	use_parent = TRUE,
	# If you don't want to use the default config file,
	# set a `GOLEM_CONFIG_PATH` environment variable that points
	# to your custom config file.
	file = Sys.getenv(
		"GOLEM_CONFIG_PATH",
		app_sys(
			"golem-config.yml"
		)
	)
) {
	cle <- paste(config, file, use_parent, sep = "|")

	if (!exists(cle, envir = .golem_config_cache, inherits = FALSE)) {
		assign(
			cle,
			config::get(config = config, file = file, use_parent = use_parent),
			envir = .golem_config_cache
		)
	}

	get(cle, envir = .golem_config_cache, inherits = FALSE)[[value]]
}
