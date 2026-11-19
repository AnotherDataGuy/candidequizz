# Composants d'interface partagés (charte « Candide au guichet »).

#' Thème de l'application
#'
#' Aucune police distante n'est chargée : pile système uniquement.
#'
#' @return Un objet `bslib::bs_theme()`.
#' @noRd
cq_theme <- function() {
	bslib::bs_theme(
		version = 5,
		primary = "#29352a",
		secondary = "#68705e",
		success = "#25663d",
		info = "#718c54",
		warning = "#be994f",
		danger = "#a34e38",
		"body-bg" = "#f3f1e5",
		"body-color" = "#29352a",
		"border-radius" = "0.5rem",
		"btn-border-radius" = "100px",
		"card-border-width" = "1px",
		"card-cap-bg" = "transparent"
	)
}

#' Sections de l'application, dans l'ordre du dock
#'
#' @return Une liste de listes `id`, `libelle`, `icone` (id sprite SVG).
#' @noRd
cq_sections <- function() {
	list(
		list(id = "accueil", libelle = "Accueil", icone = "i-home", crumb = "Touchez un th\u00e8me pour en savoir plus."),
		list(id = "seances", libelle = "S'entra\u00eener", icone = "i-play", crumb = "\u00c0 votre rythme"),
		list(id = "statistiques", libelle = "Progression", icone = "i-chart", crumb = "Le carnet de progression"),
		list(id = "ressources", libelle = "Ressources", icone = "i-book", crumb = "Pour aller plus loin")
	)
}

#' Identifiant d'entrée du bouton de navigation d'une section
#'
#' @param section Identifiant de section.
#'
#' @return Une chaîne de caractères.
#' @noRd
cq_nav_input_id <- function(section) {
	paste0("nav_", section)
}

#' Icône SVG du sprite
#'
#' @param id Identifiant de symbole (ex. `i-home`).
#' @param class Classes CSS supplémentaires.
#'
#' @return Un tag HTML.
#' @noRd
cq_icon <- function(id, class = "icon") {
	shiny::tags$svg(
		class = class,
		shiny::tags$use(href = paste0("#", id))
	)
}

#' Sprite SVG des icônes (caché)
#'
#' @return Un tag HTML.
#' @noRd
cq_icon_sprite <- function() {
	chemin <- app_sys("app/www/icons.svg")

	if (!nzchar(chemin) || !file.exists(chemin)) {
		return(NULL)
	}

	shiny::HTML(paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
}

#' En-tête topbar « candide. / AU GUICHET »
#'
#' @return Un tag HTML.
#' @noRd
cq_entete <- function() {
	shiny::tags$header(
		class = "topbar",
		shiny::tags$a(
			href = "#",
			class = "brand",
			`aria-label` = "Candide au guichet, accueil",
			onclick = "Shiny.setInputValue('nav_accueil', Math.random()); return false;",
			shiny::tags$span(
				class = "wordmark",
				"candide",
				shiny::tags$span(class = "brand-period", ".")
			),
			shiny::tags$span(
				class = "brand-sub",
				"AU GUICHET",
				shiny::tags$br(),
				shiny::tags$span("CURIEUX DE TOUT, \u00c0 VOTRE RYTHME.")
			)
		),
		shiny::tags$div(
			class = "header-tools",
			# Icône « Gérer les comptes » : envoyée par le serveur aux seuls
			# administrateurs (output$admin_bouton, app_server.R).
			# « Un problème ? Une suggestion ? » : version en ligne seulement
			# (output$retour_bouton, app_server.R).
			shiny::uiOutput("retour_bouton", inline = TRUE),
			shiny::uiOutput("admin_bouton", inline = TRUE),
			shiny::tags$button(
				id = "cq-theme-toggle",
				class = "iconbtn theme-toggle",
				type = "button",
				`aria-label` = "Activer le th\u00e8me sombre",
				cq_icon("i-moon", class = "icon theme-icon theme-icon--to-dark"),
				cq_icon("i-sun", class = "icon theme-icon theme-icon--to-light")
			),
			shiny::tags$button(
				type = "button",
				class = "textbtn why",
				`data-manifesto` = "true",
				shiny::tags$span(class = "why-label", "Pourquoi Candide ?"),
				shiny::tags$span(class = "why-arrow", "\u2197")
			)
		)
	)
}

#' Rail de contexte sous la topbar
#'
#' @param crumb Texte du fil d'Ariane.
#'
#' @return Un tag HTML.
#' @noRd
cq_page_rail <- function(crumb = "Touchez un th\u00e8me pour en savoir plus.") {
	shiny::tags$div(
		class = "page-rail",
		shiny::tags$span(class = "eyebrow", id = "cq-crumb", crumb),
		shiny::tags$span(
			class = "rail-note",
			"PR\u00c9PARATION \u00c0 L'EXAMEN CIVIQUE ",
			shiny::tags$i(),
			" \u00c0 VOTRE RYTHME"
		)
	)
}

#' Carrousel compact des illustrations de th\u00e8mes
#'
#' Plac\u00e9 juste sous le rail de contexte. D\u00e9filement automatique (JS),
#' pause au survol, au toucher et au focus ; glissement au doigt et
#' fl\u00e8ches dans les deux sens.
#'
#' @param bank Banque optionnelle (libell\u00e9s des th\u00e8mes).
#'
#' @return Un tag HTML.
#' @noRd
cq_themes_carousel <- function(bank = NULL) {
	carte <- cq_theme_art_map()
	libelles <- c(
		REHF = "Rep\u00e8res d'histoire",
		PRVS = "Principes et valeurs",
		INPO = "Institutions",
		CIDD = "Citoyennet\u00e9",
		SOCF = "Soci\u00e9t\u00e9 et culture",
		GEOG = "G\u00e9ographie",
		ECON = "\u00c9conomie",
		FUEM = "France et Europe"
	)

	bank <- bank %||% tryCatch(bank_load(), error = function(e) NULL)
	if (!is.null(bank)) {
		themes <- bank_themes(bank)
		for (i in seq_len(nrow(themes))) {
			id <- themes$theme_id[[i]]
			if (id %in% names(libelles)) {
				libelles[[id]] <- themes$theme[[i]]
			}
		}
	}

	# Chaque vignette ouvre la fen\u00eatre du th\u00e8me (`cq_theme_modal()`).
	vignette <- function(id, copie = FALSE) {
		attrs <- list(
			type = "button",
			class = "cq-carousel-slide",
			`data-theme-id` = id,
			onclick = sprintf(
				"Shiny.setInputValue('theme_info', '%s', {priority: 'event'})",
				id
			),
			`aria-label` = paste0(libelles[[id]] %||% id, " : en savoir plus"),
			shiny::tags$img(
				src = paste0("www/", carte[[id]]),
				alt = "",
				loading = "lazy",
				decoding = "async",
				draggable = "false"
			),
			shiny::tags$span(class = "cq-carousel-caption", libelles[[id]] %||% id)
		)

		# La copie ne sert qu'\u00e0 la boucle visuelle : invisible au clavier et
		# aux lecteurs d'\u00e9cran.
		if (copie) {
			attrs[["aria-hidden"]] <- "true"
			attrs[["tabindex"]] <- "-1"
		}

		do.call(shiny::tags$button, attrs)
	}

	# Trois fois la bande pour une boucle continue sans trou, dans les deux
	# sens, même quand la fenêtre est presque aussi large qu'une bande
	# (voir www/themes-carousel.js).
	piste <- c(
		lapply(names(carte), vignette),
		lapply(names(carte), vignette, copie = TRUE),
		lapply(names(carte), vignette, copie = TRUE)
	)

	# Fl\u00e8ches gauche / droite (souris) ; au doigt, on fait glisser la bande
	# dans les deux sens. Le d\u00e9filement automatique est pilot\u00e9 par
	# www/themes-carousel.js.
	fleche <- function(sens) {
		shiny::tags$button(
			type = "button",
			class = paste0("cq-carousel-btn cq-carousel-btn--", sens),
			`data-carousel-dir` = if (identical(sens, "prev")) "-1" else "1",
			`aria-label` = if (identical(sens, "prev")) "Th\u00e8mes pr\u00e9c\u00e9dents" else "Th\u00e8mes suivants",
			shiny::HTML(if (identical(sens, "prev")) "&#8249;" else "&#8250;")
		)
	}

	shiny::tags$div(
		class = "cq-carousel",
		role = "region",
		`aria-label` = "Th\u00e8mes de l'examen civique",
		fleche("prev"),
		shiny::tags$div(
			class = "cq-carousel-viewport",
			shiny::tags$div(
				class = "cq-carousel-track",
				id = "cq-carousel-track",
				piste
			)
		),
		fleche("next")
	)
}

#' Un bouton du dock
#'
#' @param section Description de section issue de [cq_sections()].
#' @param actif La section est-elle active à l'ouverture.
#'
#' @return Un tag HTML.
#' @noRd
cq_nav_item <- function(section, actif = FALSE) {
	attrs <- list(
		id = cq_nav_input_id(section$id),
		class = paste("dock-item action-button", if (actif) "is-actif" else ""),
		type = "button",
		`data-section` = section$id,
		`aria-label` = section$libelle,
		cq_icon(section$icone),
		shiny::tags$span(section$libelle)
	)

	if (actif) {
		attrs[["aria-current"]] <- "page"
	}

	# Progression : cach\u00e9e jusqu'\u00e0 l'ouverture du carnet (app_server).
	if (identical(section$id, "statistiques")) {
		attrs[["hidden"]] <- NA
	}

	do.call(shiny::tags$button, attrs)
}

#' Dock flottant de navigation
#'
#' @param active Identifiant de la section active à l'ouverture.
#'
#' @return Un tag HTML.
#' @noRd
cq_bottom_nav <- function(active = "accueil") {
	shiny::tags$div(
		class = "dock",
		id = "cq-dock",
		shiny::tags$nav(
			class = "dock-nav",
			id = "cq-dock-nav",
			`aria-label` = "Navigation principale",
			lapply(
				cq_sections(),
				function(section) cq_nav_item(section, actif = identical(section$id, active))
			)
		),
		shiny::tags$button(
			id = "cq-dock-toggle",
			class = "dock-toggle",
			type = "button",
			`aria-expanded` = "true",
			`aria-controls` = "cq-dock-nav",
			`aria-label` = "R\u00e9duire le menu",
			shiny::tags$svg(
				class = "icon",
				viewBox = "0 0 24 24",
				`aria-hidden` = "true",
				shiny::tags$path(d = "m6 9 6 6 6-6")
			),
			shiny::tags$span(class = "dock-reopen", "Menu")
		)
	)
}

#' Explication pédagogique après réponse
#'
#' @param texte Texte d'explication.
#' @param class Classe CSS du conteneur.
#'
#' @return Un tag HTML, ou `NULL` si le texte est vide.
#' @noRd
cq_explication_ui <- function(texte, class = "cq-correction-explication") {
	brut <- trimws(gsub("\r\n?", "\n", as.character(texte %||% "")[[1L]]))

	if (is.na(brut) || !nzchar(brut)) {
		return(NULL)
	}

	shiny::div(
		class = paste(class, "cq-texte-riche"),
		cq_texte_blocs(brut)
	)
}

#' Le texte porte-t-il déjà une mise en forme explicite ?
#' @noRd
cq_texte_balise <- function(texte) {
	grepl("\\*\\*|__|==|(^|\n)[ \t]*([-*\u2022\u2013]|\\d{1,2}[.)])[ \t]+\\S", texte, perl = TRUE)
}

#' Mise en forme en ligne d'un fragment de texte
#'
#' Syntaxe : `**gras**`, `__souligné__`, `*italique*`, `==surligné==`. Sans
#' balisage explicite, les années et les siècles sont légèrement accentués.
#'
#' @param texte Fragment brut (non échappé).
#' @param auto Accentuer automatiquement dates et siècles.
#'
#' @return Du HTML.
#' @noRd
cq_texte_inline <- function(texte, auto = TRUE) {
	x <- htmltools::htmlEscape(texte)
	x <- gsub("\\*\\*(\\S(?:.*?\\S)?)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE)
	x <- gsub("__(\\S(?:.*?\\S)?)__", "<u>\\1</u>", x, perl = TRUE)
	x <- gsub("==(\\S(?:.*?\\S)?)==", "<mark>\\1</mark>", x, perl = TRUE)
	x <- gsub("(?<![\\w*])\\*(\\S(?:.*?\\S)?)\\*(?![\\w*])", "<em>\\1</em>", x, perl = TRUE)

	if (auto) {
		mois <- "(?:janvier|f\u00e9vrier|mars|avril|mai|juin|juillet|ao\u00fbt|septembre|octobre|novembre|d\u00e9cembre)"
		annee <- "(?:1[0-9]|20)[0-9]{2}(?![\\w/.-]|\\s?(?:km|m\\b|habitants|euros|\u20ac|%))"
		x <- gsub(
			glue::glue(
				"(?<![\\w/.-])((?:\\d{{1,2}}(?:er)?\\s{mois}(?:\\s{annee})?)|{annee})"
			),
			"<b class=\"cq-texte-date\">\\1</b>",
			x,
			perl = TRUE
		)
		x <- gsub(
			"\\b([IVXL]+(?:e|\u1d49|\u00e8me) si\u00e8cle)",
			"<b class=\"cq-texte-date\">\\1</b>",
			x,
			perl = TRUE
		)
	}

	shiny::HTML(x)
}

#' Découper un long paragraphe en groupes de phrases
#' @noRd
cq_texte_aerer <- function(paragraphe, seuil = 420L, par = 2L) {
	if (nchar(paragraphe) <= seuil) {
		return(paragraphe)
	}

	protege <- gsub("\\b(J\\.-C|av|apr|M|Mme|etc|art|cf|env|n\u00b0|p)\\.", "\\1\u00a7", paragraphe, perl = TRUE)
	phrases <- strsplit(protege, "(?<=[.!?\u00bb])\\s+(?=[A-Z\u00c0-\u00dd\u00ab])", perl = TRUE)[[1L]]
	phrases <- gsub("\u00a7", ".", phrases, fixed = TRUE)

	if (length(phrases) <= par) {
		return(paragraphe)
	}

	groupes <- split(phrases, ceiling(seq_along(phrases) / par))
	vapply(groupes, paste, character(1), collapse = " ")
}

#' Convertir un texte d'explication en blocs HTML
#'
#' Paragraphes séparés par une ligne vide ; listes `- ` / `1. ` ; encadré pour
#' les lignes commençant par `À retenir` ou `> `. Une première ligne courte
#' suivie d'un autre bloc devient un chapeau.
#'
#' @param texte Texte brut normalisé.
#'
#' @return Une `tagList`.
#' @noRd
cq_texte_blocs <- function(texte) {
	auto <- !cq_texte_balise(texte)
	blocs <- trimws(strsplit(texte, "\n\\s*\n")[[1L]])
	blocs <- blocs[nzchar(blocs)]
	sortie <- list()

	puce <- "^\\s*[-*\u2022\u2013]\\s+(?=\\S)"
	numero <- "^\\s*\\d{1,2}[.)]\\s+(?=\\S)"
	retenir <- "^\\s*(>\\s*|(\u00c0|A) retenir\\s*:?\\s*)"

	for (b in seq_along(blocs)) {
		lignes <- trimws(strsplit(blocs[[b]], "\n")[[1L]])
		lignes <- lignes[nzchar(lignes)]

		if (b == 1L && length(blocs) > 1L && length(lignes) == 1L &&
				nchar(lignes[[1L]]) <= 140L && !grepl(puce, lignes[[1L]], perl = TRUE)) {
			sortie[[length(sortie) + 1L]] <- shiny::p(
				class = "cq-texte-lead",
				cq_texte_inline(lignes[[1L]], auto = FALSE)
			)
			next
		}

		courant <- character(0)
		liste <- character(0)
		type_liste <- NULL

		vider_paragraphe <- function() {
			if (length(courant)) {
				texte_p <- paste(courant, collapse = " ")
				morceaux <- if (auto) cq_texte_aerer(texte_p) else texte_p
				for (m in morceaux) {
					sortie[[length(sortie) + 1L]] <<- shiny::p(cq_texte_inline(m, auto))
				}
				courant <<- character(0)
			}
		}

		vider_liste <- function() {
			if (length(liste)) {
				balise <- if (identical(type_liste, "ol")) shiny::tags$ol else shiny::tags$ul
				sortie[[length(sortie) + 1L]] <<- balise(lapply(liste, function(l) {
					shiny::tags$li(cq_texte_inline(l, auto))
				}))
				liste <<- character(0)
				type_liste <<- NULL
			}
		}

		for (ligne in lignes) {
			if (grepl(retenir, ligne, perl = TRUE)) {
				vider_paragraphe()
				vider_liste()
				sortie[[length(sortie) + 1L]] <- shiny::div(
					class = "cq-texte-retenir",
					shiny::span(class = "cq-texte-retenir-titre", "\u00c0 retenir"),
					shiny::p(cq_texte_inline(sub(retenir, "", ligne, perl = TRUE), auto))
				)
			} else if (grepl(puce, ligne, perl = TRUE) || grepl(numero, ligne, perl = TRUE)) {
				vider_paragraphe()
				type <- if (grepl(numero, ligne, perl = TRUE)) "ol" else "ul"
				if (!is.null(type_liste) && !identical(type, type_liste)) {
					vider_liste()
				}
				type_liste <- type
				liste <- c(liste, sub(if (type == "ol") numero else puce, "", ligne, perl = TRUE))
			} else {
				vider_liste()
				courant <- c(courant, ligne)
			}
		}

		vider_paragraphe()
		vider_liste()
	}

	shiny::tagList(sortie)
}

#' Dialog manifeste « Pourquoi Candide ? »
#'
#' @return Un tag HTML.
#' @noRd
cq_manifesto_dialog <- function() {
	shiny::tags$dialog(
		id = "manifesto",
		`aria-labelledby` = "manifesto-title",
		shiny::tags$div(
			class = "manifesto-inner",
			shiny::tags$div(
				class = "row between",
				shiny::tags$span(class = "eyebrow", "Candide au guichet / Notre d\u00e9marche"),
				shiny::tags$button(
					class = "iconbtn",
					id = "close-manifesto",
					type = "button",
					`aria-label` = "Fermer le manifeste",
					"\u2715"
				)
			),
			shiny::tags$h2(
				id = "manifesto-title",
				"Apprendre,",
				shiny::tags$br(),
				"comprendre,",
				shiny::tags$br(),
				shiny::tags$span(class = "serif", "se sentir pr\u00eat.")
			),
			shiny::tags$p(
				"Pr\u00e9parer l'examen civique, c'est d\u00e9couvrir l'histoire, les institutions et les valeurs d'un pays que l'on a choisi. Candide veut rendre ces rep\u00e8res accessibles \u00e0 toutes et \u00e0 tous, quel que soit son parcours."
			),
			shiny::tags$p(
				"Ici, des questions pour s'entra\u00eener, des explications pour comprendre, et le droit de se tromper pour apprendre."
			),
			shiny::tags$p(
				shiny::tags$strong(
					"Nous croyons que l'acc\u00e8s au savoir est une affaire d'\u00e9galit\u00e9."
				),
				" Chaque personne qui se pr\u00e9sente \u00e0 l'examen m\u00e9rite une pr\u00e9paration claire, gratuite et bienveillante : voil\u00e0 notre travail."
			),
			shiny::tags$div(
				class = "manifesto-note",
				"Libert\u00e9, \u00e9galit\u00e9, fraternit\u00e9 :",
				shiny::tags$br(),
				shiny::tags$span(class = "serif", "elles s'apprennent aussi en les pratiquant.")
			),
			# Pourquoi huit th\u00e8mes ici et cinq \u00e0 l'examen (mod_ressources.R).
			ressources_typologie_ui(),
			shiny::tags$p(
				class = "tiny muted",
				"Application ind\u00e9pendante et b\u00e9n\u00e9vole, sans lien avec aucun organisme public. Elle ne remplace pas les sources officielles (voir \u00ab Ressources \u00bb)."
			),
			shiny::tags$p(
				class = "tiny muted",
				shiny::tags$strong("Version en cours de d\u00e9veloppement. "),
				"Les questions ont \u00e9t\u00e9 relues mais pas encore toutes test\u00e9es : certaines peuvent contenir des erreurs ou des informations d\u00e9pass\u00e9es. En cas de doute, les sources officielles font foi."
			),
			# L'IA : une phrase ici, l'explication complète dans cq_ia_modal() (fct_ia_info.R).
			shiny::tags$p(
				class = "tiny muted",
				"Une partie des questions peut \u00eatre r\u00e9\u00e9crite par une IA \u00e0 partir de la banque, puis v\u00e9rifi\u00e9e. ",
				"Vos donn\u00e9es personnelles ne lui sont jamais envoy\u00e9es. ",
				cq_ia_lien(
					"Comment l'IA fonctionne ici",
					avant = "document.getElementById('manifesto').close(); "
				)
			),
			shiny::tags$button(
				class = "btn primary",
				id = "manifesto-train",
				type = "button",
				onclick = "Shiny.setInputValue('nav_seances', Math.random()); document.getElementById('manifesto').close();",
				"Commencer \u00e0 m'entra\u00eener ",
				cq_icon("i-arrow")
			)
		)
	)
}
