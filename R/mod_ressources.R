#' Chemin du fichier de ressources
#' @return Chemin absolu vers `inst/extdata/ressources.yml`.
#' @noRd
ressources_path <- function() {
	chemin <- app_sys("extdata", "ressources.yml")

	if (!nzchar(chemin) || !file.exists(chemin)) {
		rlang::abort(
			c(
				"Le fichier des liens utiles est introuvable.",
				i = "Fichier attendu : `inst/extdata/ressources.yml`."
			),
			class = "candidequizz_ressources_absentes"
		)
	}

	chemin
}

#' Lire et valider le fichier des liens utiles
#' @param chemin Chemin du fichier YAML.
#' @return Une liste de groupes.
#' @noRd
ressources_read <- function(chemin = ressources_path()) {
	contenu <- yaml::read_yaml(chemin)
	groupes <- contenu$groupes

	if (!length(groupes)) {
		rlang::abort(
			"Aucun groupe de liens dans `ressources.yml`.",
			class = "candidequizz_ressources_invalides"
		)
	}

	for (groupe in groupes) {
		manquants <- setdiff(c("id", "titre", "liens"), names(groupe))

		if (length(manquants)) {
			rlang::abort(
				c(
					"Groupe de liens incomplet dans `ressources.yml`.",
					x = glue::glue("Champs manquants : {paste(manquants, collapse = ', ')}.")
				),
				class = "candidequizz_ressources_invalides"
			)
		}

		for (lien in groupe$liens) {
			manquants <- setdiff(c("titre", "url"), names(lien))

			if (length(manquants)) {
				rlang::abort(
					c(
						glue::glue("Lien incomplet dans le groupe `{groupe$id}`."),
						x = glue::glue("Champs manquants : {paste(manquants, collapse = ', ')}.")
					),
					class = "candidequizz_ressources_invalides"
				)
			}
		}
	}

	groupes
}

#' Hôte d'une URL de ressource
#' @noRd
ressources_domaine <- function(url) {
	hote <- sub("^https?://", "", as.character(url[[1L]] %||% ""))
	hote <- sub("/.*$", "", hote)
	sub("^www\\.", "", hote)
}

#' Sigle d'aperçu pour une mosaïque
#' @noRd
ressources_sigle <- function(url) {
	connus <- c(
		"service-public.gouv.fr" = "sp.",
		"administration-etrangers-en-france.interieur.gouv.fr" = "ae.",
		"legifrance.gouv.fr" = "lf.",
		"conseil-constitutionnel.fr" = "cc.",
		"defenseurdesdroits.fr" = "dd.",
		"vie-publique.fr" = "vp.",
		"elysee.fr" = "el.",
		"assemblee-nationale.fr" = "an.",
		"senat.fr" = "se.",
		"gouvernement.fr" = "go.",
		"ccomptes.fr" = "cd.",
		"cnil.fr" = "cn.",
		"insee.fr" = "in.",
		"education.gouv.fr" = "en.",
		"france.fr" = "fr.",
		"european-union.europa.eu" = "ue."
	)

	hote <- ressources_domaine(url)
	if (hote %in% names(connus)) {
		return(unname(connus[[hote]]))
	}

	racine <- strsplit(hote, ".", fixed = TRUE)[[1L]][[1L]]
	paste0(tolower(substr(gsub("[^A-Za-z]", "", racine), 1L, 2L)), ".")
}

#' Extrait affichable d'un lien
#' @noRd
ressources_extrait <- function(lien) {
	texte <- trimws(as.character(lien$apercu %||% lien$description %||% ""))
	if (!nzchar(texte)) {
		return(ressources_domaine(lien$url))
	}

	if (nchar(texte) <= 160L) {
		return(texte)
	}

	paste0(substr(texte, 1L, 157L), "\u2026")
}

#' Sceau d'un lien : le sigle du site en capitales (« lf. » -> « LF »)
#' @noRd
ressources_sceau <- function(url) {
	toupper(sub("\\.$", "", ressources_sigle(url)))
}

#' Texte indexé par la recherche
#' @noRd
ressources_cle_recherche <- function(lien) {
	tolower(paste(lien$titre, lien$source %||% "", ressources_extrait(lien), ressources_domaine(lien$url)))
}

#' Rendu d'un lien : une ligne du dossier
#' @noRd
ressources_lien_ui <- function(lien) {
	shiny::tags$a(
		class = "res-lien",
		href = lien$url,
		target = "_blank",
		rel = "noopener noreferrer",
		`aria-label` = paste0(lien$titre, ", ouvrir dans un nouvel onglet"),
		`data-search` = ressources_cle_recherche(lien),
		shiny::tags$span(class = "res-sceau", `aria-hidden` = "true", ressources_sceau(lien$url)),
		shiny::tags$span(
			class = "res-texte",
			shiny::tags$strong(lien$titre),
			shiny::tags$span(ressources_extrait(lien))
		),
		shiny::tags$span(class = "res-source", paste(lien$source %||% ressources_domaine(lien$url), "\u2197"))
	)
}

#' Rendu d'un lien « à lire en premier » (`essentiel: true` dans le YAML)
#' @noRd
ressources_vedette_ui <- function(lien, sombre = FALSE) {
	shiny::tags$a(
		class = paste("res-vedette", if (sombre) "is-sombre"),
		href = lien$url,
		target = "_blank",
		rel = "noopener noreferrer",
		`data-search` = ressources_cle_recherche(lien),
		shiny::tags$span(class = "eyebrow", paste("\u00c0 lire en premier \u00b7", lien$source %||% ressources_domaine(lien$url))),
		shiny::tags$strong(lien$titre),
		shiny::tags$span(class = "res-vedette-texte", ressources_extrait(lien)),
		shiny::tags$span(class = "res-vedette-ouvrir", "Ouvrir \u2197")
	)
}

#' Un dossier : numéro, titre, description, puis ses liens
#' @noRd
ressources_groupe_ui <- function(groupe, rang) {
	liens <- Filter(function(l) !isTRUE(l$essentiel), groupe$liens)
	if (!length(liens)) {
		return(NULL)
	}

	shiny::tags$section(
		class = paste0("res-groupe res-g", ((rang - 1L) %% 5L) + 1L),
		`aria-labelledby` = paste0("res-", groupe$id),
		shiny::tags$div(
			class = "res-groupe-tete",
			shiny::tags$span(class = "res-numero", sprintf("%02d", rang)),
			shiny::tags$h2(id = paste0("res-", groupe$id), groupe$titre),
			if (!is.null(groupe$description)) shiny::tags$p(trimws(groupe$description))
		),
		shiny::tags$div(class = "res-liste", lapply(liens, ressources_lien_ui))
	)
}

#' Encadré : pourquoi huit thèmes ici et cinq à l'examen (fenêtre « Pourquoi Candide ? »)
#' @noRd
ressources_typologie_ui <- function() {
	shiny::tags$section(
		class = "res-typologie",
		`aria-labelledby` = "res-typologie-titre",
		shiny::tags$div(
			class = "res-typologie-tete",
			shiny::tags$p(class = "eyebrow", "Le programme"),
			shiny::tags$h2(
				id = "res-typologie-titre",
				"Huit th\u00e8mes ici, cinq \u00e0 l'examen. ",
				shiny::tags$em("Pourquoi ?")
			),
			shiny::tags$p(
				"L'examen officiel range ses questions en cinq th\u00e8mes. Candide en utilise huit : ",
				"ce n'est pas un autre programme, c'est le m\u00eame, d\u00e9coup\u00e9 plus finement."
			)
		),
		shiny::tags$ol(
			class = "res-typologie-raisons",
			shiny::tags$li(
				shiny::tags$strong("Le m\u00eame programme, rien en plus."),
				" Chaque question de la banque est rattach\u00e9e \u00e0 l'un des cinq th\u00e8mes officiels. ",
				"Nos huit th\u00e8mes red\u00e9coupent ce programme : ils n'y ajoutent rien et n'en retirent rien."
			),
			shiny::tags$li(
				shiny::tags$strong("Un th\u00e8me trop large cache ce qui coince."),
				" \u00ab Syst\u00e8me institutionnel et politique \u00bb rassemble \u00e0 lui seul pr\u00e8s d'un tiers des questions, ",
				"des institutions fran\u00e7aises \u00e0 l'Union europ\u00e9enne. Savoir que l'on est fragile sur l'Europe mais solide sur le Parlement ",
				"dit quoi r\u00e9viser : c'est sur ces th\u00e8mes plus fins que s'appuient la r\u00e9vision cibl\u00e9e et le bilan."
			),
			shiny::tags$li(
				shiny::tags$strong("On apprend par sujets."),
				" Les cinq th\u00e8mes officiels d\u00e9crivent ce que l'examen v\u00e9rifie ; nos huit th\u00e8mes suivent la fa\u00e7on dont on apprend : ",
				"une p\u00e9riode, une institution, une notion \u00e0 la fois."
			)
		),
		shiny::tags$p(
			class = "res-typologie-pied",
			"Pour s'entra\u00eener dans les conditions du jour J, l'examen blanc reprend le format officiel : ",
			"40 questions, 45 minutes, 32 bonnes r\u00e9ponses pour r\u00e9ussir."
		)
	)
}

#' Ressources — interface
#' @param id Identifiant du module.
#' @return Un tag HTML.
#' @noRd
mod_ressources_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::div(
		class = "cq-ressources res-v3",
		shiny::div(
			class = "res-entete",
			shiny::div(
				class = "res-entete-titre",
				shiny::p(class = "eyebrow res-accent", "Pour aller plus loin"),
				shiny::tags$h1("Lire. Comprendre. ", shiny::tags$em("Questionner.")),
				shiny::p(
					class = "res-chapeau",
					"Les sources officielles, rang\u00e9es par dossier. Chaque lien s'ouvre sur le site de l'institution."
				)
			),
			shiny::div(
				class = "res-recherche",
				shiny::tags$label(`for` = ns("search"), "Rechercher une ressource"),
				shiny::tags$input(
					id = ns("search"),
					type = "search",
					placeholder = "Institutions, textes, d\u00e9marches\u2026",
					# Filtre sans accents ; un dossier sans résultat disparaît.
					oninput = paste0(
						"var t=this.value.normalize('NFD').replace(/[\\u0300-\\u036f]/g,'').toLowerCase();",
						"var n=0;document.querySelectorAll('.res-v3 [data-search]').forEach(function(el){",
						"var ok=!t||el.getAttribute('data-search').normalize('NFD').replace(/[\\u0300-\\u036f]/g,'').includes(t);",
						"el.classList.toggle('hidden',!ok);if(ok)n++;});",
						"document.querySelectorAll('.res-v3 .res-groupe, .res-v3 .res-vedettes').forEach(function(g){",
						"g.classList.toggle('hidden',!g.querySelector('[data-search]:not(.hidden)'));});",
						"var empty=document.getElementById('", ns("no-resources"), "');",
						"if(empty)empty.classList.toggle('hidden',n>0);"
					)
				)
			)
		),
		shiny::uiOutput(ns("groupes")),
		shiny::tags$p(
			id = ns("no-resources"),
			class = "muted hidden topspace",
			"Aucune ressource ne correspond \u00e0 votre recherche."
		)
	)
}

#' Ressources — serveur
#' @param id Identifiant du module.
#' @return `NULL`, invisiblement.
#' @noRd
mod_ressources_server <- function(id) {
	shiny::moduleServer(id, function(input, output, session) {
		output$groupes <- shiny::renderUI({
			groupes <- tryCatch(
				ressources_read(),
				error = function(e) {
					shiny::showNotification(
						"Les liens utiles n'ont pas pu \u00eatre charg\u00e9s.",
						type = "error"
					)
					NULL
				}
			)

			if (is.null(groupes)) {
				return(shiny::tags$p("Aucun lien disponible pour le moment."))
			}

			liens <- unlist(lapply(groupes, function(g) g$liens), recursive = FALSE)
			vedettes <- Filter(function(l) isTRUE(l$essentiel), liens)

			shiny::tagList(
				if (length(vedettes)) {
					shiny::tags$div(
						class = "res-vedettes",
						lapply(seq_along(vedettes), function(i) ressources_vedette_ui(vedettes[[i]], sombre = i %% 2L == 0L))
					)
				},
				lapply(seq_along(groupes), function(i) ressources_groupe_ui(groupes[[i]], i))
			)
		})
	})
}
