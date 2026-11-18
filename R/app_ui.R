#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
	# Mode local : l'application s'ouvre directement, sans écran de connexion.
	local <- identical(auth_mode(), "local")

	bslib::page_fluid(
		theme = cq_theme(),
		title = cq_app_title(),
		lang = "fr",
		class = "cq-app",
		golem_add_external_resources(),
		cq_icon_sprite(),
		if (local) {
			shiny::conditionalPanel(condition = "true", cq_ui_principale())
		} else {
			# Version en ligne : l'interface n'est envoyée au navigateur qu'après
			# la connexion (output$cq_principale, app_server.R). Ses dépendances
			# (CSS et JS des composants) sont attachées dès maintenant : ajoutées
			# à l'arrivée de l'interface, elles passeraient après nos feuilles de
			# style et changeraient l'ordre de la cascade.
			# Résolues au rendu de la page (tagFunction), comme le reste : certaines
			# dépendances (selectize) dépendent du thème {bslib} actif.
			htmltools::tagFunction(function() {
				htmltools::attachDependencies(
					shiny::uiOutput("cq_principale"),
					htmltools::findDependencies(cq_ui_principale())
				)
			})
		},
		if (!local) {
			shiny::conditionalPanel(
				condition = "!output.connecte",
				mod_auth_ui("auth")
			)
		}
	)
}

#' Interface principale (dock, en-tête, sections, pied de page)
#'
#' Affichée directement en mode local ; en mode comptes, envoyée par le
#' serveur une fois l'utilisateur connecté.
#'
#' @noRd
cq_ui_principale <- function() {
	shiny::tagList(
		shiny::tags$a(class = "skip-link", href = "#main", "Aller au contenu"),
		cq_bottom_nav(active = "accueil"),
		shiny::div(
			class = "shell",
			cq_entete(),
			shiny::uiOutput("compte"),
			cq_page_rail(),
			shiny::uiOutput("jauge"),
			cq_themes_carousel(),
			shiny::tags$main(
				id = "main",
				tabindex = "-1",
				bslib::navset_hidden(
					id = "cq_section",
					bslib::nav_panel_hidden("accueil", mod_home_ui("accueil")),
					bslib::nav_panel_hidden("seances", mod_seances_ui("seances")),
					bslib::nav_panel_hidden("statistiques", mod_stats_ui("statistiques")),
					bslib::nav_panel_hidden("ressources", mod_ressources_ui("ressources"))
				)
			),
			shiny::tags$footer(
				class = "footer",
				shiny::tags$span("Candide au guichet \u00b7 Avancer \u00e0 son rythme."),
				shiny::tags$span("Version d'essai, questions en test \u00b7 Sans lien avec un organisme public.")
			)
		),
		cq_manifesto_dialog()
	)
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon
#' @noRd
golem_add_external_resources <- function() {
	add_resource_path(
		"www",
		app_sys("app/www")
	)

	tags$head(
		favicon(),
		# Scripts seulement : `bundle_resources()` ajoutait aussi toutes les feuilles
		# de style, d\u00e9j\u00e0 charg\u00e9es ci-dessous dans un ordre ma\u00eetris\u00e9 (chaque
		# fichier CSS \u00e9tait donc charg\u00e9 deux fois).
		htmltools::htmlDependency(
			name = "candidequizz-scripts",
			version = as.character(cq_config("golem_version")),
			src = app_sys("app/www"),
			script = sort(list.files(app_sys("app/www"), pattern = "\\.js$"))
		),
		# Feuilles de style : une par écran, dans www/css/, chargées dans l'ordre
		# de leur numéro (01-jetons.css d'abord). L'ordre compte : voir l'en-tête
		# de chaque fichier. `?v=` = date du fichier, pour que le navigateur
		# recharge une feuille modifiée.
		lapply(cq_feuilles_style(), function(fichier) {
			tags$link(
				rel = "stylesheet",
				type = "text/css",
				href = paste0(
					"www/css/", fichier, "?v=",
					as.integer(file.mtime(app_sys("app/www/css", fichier)))
				)
			)
		}),
		tags$meta(
			name = "viewport",
			content = "width=device-width, initial-scale=1, viewport-fit=cover"
		),
		tags$meta(name = "color-scheme", content = "light dark"),
		# Thème clair / sombre appliqué AVANT le premier affichage, d'où le
		# script en ligne (un fichier .js arriverait trop tard et l'écran
		# clignoterait). Ordre : choix enregistré par theme.js (« nuit » et
		# « jour » sont les anciens noms), sinon réglage du système.
		tags$script(htmltools::HTML(paste(
			"(function(){try{",
			"var t=localStorage.getItem('cq-theme');",
			"if(t==='nuit')t='dark';if(t==='jour')t='light';",
			"if(!t){t=(window.matchMedia&&window.matchMedia('(prefers-color-scheme: dark)').matches)?'dark':'light';}",
			"document.documentElement.setAttribute('data-theme',t);",
			"document.documentElement.style.colorScheme=t==='dark'?'dark':'light';",
			"}catch(e){}})();",
			sep = "\n"
		)))
		# Le jeton de connexion est géré par www/auth-token.js.
	)
}

#' Feuilles de style de l'application, dans l'ordre de chargement
#'
#' Les fichiers de `inst/app/www/css/` sont numérotés (`01-jetons.css`,
#' `02-base-1.css`...) : l'ordre alphabétique est l'ordre de la cascade.
#'
#' @return Un vecteur de noms de fichiers.
#' @noRd
cq_feuilles_style <- function() {
	sort(list.files(app_sys("app/www/css"), pattern = "\\.css$"), method = "radix")
}
