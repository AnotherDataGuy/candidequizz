# Génération hors du processus principal.
#
# `shiny::ExtendedTask` n'exécute rien en arrière-plan par lui-même : il
# appelle sa fonction de façon synchrone et n'attend qu'une promesse. Pour que
# le lot suivant s'écrive vraiment pendant que l'utilisateur répond, la
# génération tourne dans un processus R à part, via {future}. Le travailleur
# reçoit des données simples (paramètres, plan, historique) et renvoie le
# contexte : aucun état partagé, aucun accès au stockage.

# `cle`, `dev` et `chemin` sont transmis au travailleur par `globals`
# (voir async_appeler()) : R CMD check ne les voit pas définis.
utils::globalVariables(c("cle", "dev", "chemin"))

#' La génération asynchrone est-elle activée
#'
#' @return `TRUE` ou `FALSE`.
#' @noRd
async_active <- function() {
	isTRUE(cq_config("seance", "prefetch_async")) &&
		requireNamespace("future", quietly = TRUE) &&
		requireNamespace("promises", quietly = TRUE)
}

#' Préparer les processus de travail
#'
#' Ne remplace pas un plan {future} déjà choisi par l'utilisateur. Deux
#' travailleurs au minimum : avec un seul, `multisession` exécute tout dans le
#' processus principal, ce qui revient au synchrone.
#'
#' @param travailleurs Nombre de processus.
#'
#' @return `TRUE` si un plan asynchrone est en place, invisiblement.
#' @noRd
async_preparer <- function(travailleurs = 2L) {
	if (!async_active()) {
		return(invisible(FALSE))
	}

	if (!inherits(future::plan(), "sequential")) {
		return(invisible(TRUE))
	}

	ok <- tryCatch(
		{
			future::plan(future::multisession, workers = max(2L, as.integer(travailleurs)))
			TRUE
		},
		error = function(e) FALSE
	)

	if (ok) {
		async_prechauffer()
	}

	invisible(ok)
}

#' D'où le travailleur doit charger le paquet
#'
#' En développement (`pkgload::load_all()`), la version installée, si elle
#' existe, est périmée : le travailleur recharge les sources.
#'
#' @return `list(dev, chemin)`.
#' @noRd
async_source_paquet <- function() {
	dev <- requireNamespace("pkgload", quietly = TRUE) &&
		isTRUE(pkgload::is_dev_package("candidequizz"))

	list(
		dev = dev,
		chemin = if (dev) getNamespaceInfo(asNamespace("candidequizz"), "path") else NA_character_
	)
}

#' Exécuter une fonction du paquet dans le travailleur
#'
#' L'expression n'embarque aucune fonction du paquet : sérialiser une fonction
#' dont l'environnement est l'espace de noms forcerait le travailleur à charger
#' la version installée, avant même d'avoir pu charger les sources.
#'
#' @param fonction Nom de la fonction du paquet.
#' @param args Arguments, liste nommée.
#'
#' @return Une promesse.
#' @noRd
async_appeler <- function(fonction, args) {
	source <- async_source_paquet()

	promises::future_promise(
		{
			# Les travailleurs sont lancés au démarrage de l'application : une clé
			# collée ensuite (llm_cle_definir()) ne leur parvient que par ici.
			if (nzchar(cle)) {
				Sys.setenv(OPENAI_API_KEY = cle)
			}

			if (!"candidequizz" %in% loadedNamespaces()) {
				if (dev) {
					pkgload::load_all(chemin, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
				} else {
					loadNamespace("candidequizz")
				}
			}

			do.call(utils::getFromNamespace(fonction, "candidequizz"), args)
		},
		globals = list(
			fonction = fonction,
			args = args,
			dev = source$dev,
			chemin = source$chemin,
			cle = Sys.getenv("OPENAI_API_KEY", "")
		),
		packages = character(0),
		seed = TRUE
	)
}

#' Charger le paquet dans le travailleur dès le démarrage
#'
#' Le premier lot préchargé n'a ainsi pas à payer le chargement du paquet et
#' de la banque. Un échec est silencieux : il se reproduira, visible, au
#' premier préchargement, qui se rabattra sur une génération synchrone.
#'
#' @return Rien, appelé pour ses effets.
#' @noRd
async_prechauffer <- function() {
	promesse <- tryCatch(async_appeler("bank_load", list()), error = function(e) NULL)

	if (!is.null(promesse)) {
		promises::catch(promesse, function(e) NULL)
	}

	invisible(NULL)
}

#' Générer un lot, en arrière-plan si possible
#'
#' Une erreur de la chaîne (condition `candidequizz_*`) est propagée telle
#' quelle. Une panne du travailleur lui-même (processus mort, paquet
#' introuvable) se rabat sur une génération synchrone : la séance continue,
#' au prix d'une attente.
#'
#' @param args Arguments de [generer_lot()].
#'
#' @return Une promesse si l'asynchrone est actif, sinon le contexte.
#' @noRd
async_generer_lot <- function(args) {
	if (!async_active() || inherits(future::plan(), "sequential")) {
		return(do.call(generer_lot, args))
	}

	promesse <- tryCatch(async_appeler("generer_lot", args), error = function(e) NULL)

	if (is.null(promesse)) {
		return(do.call(generer_lot, args))
	}

	promises::catch(promesse, function(e) {
		if (inherits(e, "FutureError")) {
			return(do.call(generer_lot, args))
		}

		stop(e)
	})
}
