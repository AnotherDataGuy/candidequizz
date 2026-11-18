# Ce que l'utilisateur voit de l'IA : l'explication « Comment l'IA travaille
# ici », la fenêtre de la deuxième surprise (l'IA vient de s'ouvrir) et le
# lien qui ouvre l'explication depuis n'importe quel écran.
#
# Le texte décrit ce que fait réellement le code ; à tenir à jour si le
# fonctionnement change :
# - rédaction à partir des fiches de la banque : agent_redacteur.R ;
# - vérification, puis correction ou repli sur la banque :
#   agent_verificateur_factuel.R, agent_reparateur.R ;
# - bilan rédigé à partir de statistiques agrégées, jamais des réponses une à
#   une : agent_synthetiseur.R, stats_snapshot_seance() dans fct_stats.R ;
# - fournisseur et modèles : section `llm` de golem-config.yml.
# La politique d'OpenAI citée (pas d'entraînement sur les données de l'API,
# conservation jusqu'à 30 jours) a été vérifiée le 2026-11-12 sur
# https://developers.openai.com/api/docs/guides/your-data

#' Lien « Comment l'IA fonctionne ici » (ouvre l'explication)
#'
#' @param texte Libellé du lien.
#' @param avant Code JavaScript à exécuter d'abord (fermer une autre fenêtre).
#'
#' @return Un `<button>` stylé comme un lien.
#' @noRd
cq_ia_lien <- function(texte = "Comment l'IA fonctionne ici", avant = "") {
	shiny::tags$button(
		type = "button",
		class = "cq-ia-lien",
		onclick = paste0(avant, "Shiny.setInputValue('ia_info', Math.random(), {priority: 'event'});"),
		bsicons::bs_icon("info-circle"),
		texte
	)
}

#' Fenêtre « Comment l'IA travaille ici »
#'
#' @return Un `shiny::modalDialog()`.
#' @noRd
cq_ia_modal <- function() {
	etape <- function(num, titre, ...) {
		shiny::tags$li(
			class = "cq-ia-etape",
			shiny::tags$span(class = "cq-ia-etape-num", num),
			shiny::div(shiny::tags$strong(titre), shiny::tags$p(...))
		)
	}

	shiny::modalDialog(
		title = NULL,
		size = "l",
		easyClose = TRUE,
		class = "cq-ia-modal",
		shiny::tags$p(class = "eyebrow muted", "Transparence"),
		shiny::tags$h2("Comment l'IA travaille ici"),
		shiny::tags$p(
			class = "cq-ia-chapo",
			"Candide peut utiliser une intelligence artificielle (IA) : un mod\u00e8le de langage de la soci\u00e9t\u00e9 OpenAI. ",
			"Ce type d'IA \u00e9crit du texte en choisissant, mot apr\u00e8s mot, la suite la plus probable. ",
			"Elle ne \u00ab sait \u00bb pas les choses comme une personne et peut se tromper. ",
			"C'est pourquoi, ici, elle ne travaille jamais seule."
		),

		shiny::tags$h3("Ce qu'elle fait, \u00e9tape par \u00e9tape"),
		shiny::tags$ol(
			class = "cq-ia-etapes",
			etape(
				"1", "Elle part toujours d'une fiche de la banque.",
				"Chaque question vient d'une fiche de notre banque, pr\u00e9par\u00e9e \u00e0 partir du programme officiel de l'examen et relue : ",
				"le sujet, la bonne r\u00e9ponse et les erreurs fr\u00e9quentes. L'IA ne choisit pas le sujet et n'invente pas la r\u00e9ponse."
			),
			etape(
				"2", "Elle r\u00e9\u00e9crit la question.",
				"Elle la formule autrement et peut en changer la forme : vrai ou faux, remise en ordre, ",
				"texte \u00e0 trous, plusieurs bonnes r\u00e9ponses. Vous revoyez ainsi la m\u00eame notion sous un autre angle."
			),
			etape(
				"3", "Une deuxi\u00e8me lecture v\u00e9rifie les faits.",
				"Un second appel \u00e0 l'IA compare chaque question \u00e0 sa fiche. Si un fait est faux ou douteux, ",
				"la question est corrig\u00e9e, ou \u00e9cart\u00e9e : vous recevez alors la question d'origine de la banque. ",
				"Les questions v\u00e9rifi\u00e9es sont gard\u00e9es pour \u00eatre r\u00e9utilis\u00e9es."
			),
			etape(
				"4", "Elle peut r\u00e9diger votre bilan.",
				"En fin de s\u00e9ance, elle \u00e9crit le texte du bilan \u00e0 partir de vos chiffres : taux de r\u00e9ussite ",
				"par th\u00e8me, temps de r\u00e9ponse, confusions fr\u00e9quentes. Elle ne re\u00e7oit pas vos r\u00e9ponses une \u00e0 une."
			)
		),

		shiny::tags$h3("Ce qui est envoy\u00e9 \u00e0 OpenAI, et ce qui ne l'est jamais"),
		shiny::div(
			class = "cq-ia-donnees",
			shiny::div(
				class = "cq-ia-donnees-col is-envoye",
				shiny::tags$strong(bsicons::bs_icon("arrow-up-right"), " Envoy\u00e9"),
				shiny::tags$ul(
					shiny::tags$li("les fiches de la banque utilis\u00e9es pour la s\u00e9ance"),
					shiny::tags$li("les questions en cours de r\u00e9daction"),
					shiny::tags$li("pour le bilan : des chiffres d\u00e9j\u00e0 calcul\u00e9s (taux, temps, th\u00e8mes)")
				)
			),
			shiny::div(
				class = "cq-ia-donnees-col is-garde",
				shiny::tags$strong(bsicons::bs_icon("shield-lock"), " Jamais envoy\u00e9"),
				shiny::tags$ul(
					shiny::tags$li("votre nom, votre identifiant, votre mot de passe"),
					shiny::tags$li("vos r\u00e9ponses une \u00e0 une"),
					shiny::tags$li("vos notes de difficult\u00e9 et vos commentaires")
				)
			)
		),
		shiny::tags$p(
			class = "muted",
			"OpenAI indique ne pas utiliser les donn\u00e9es re\u00e7ues par son interface de programmation (API) ",
			"pour entra\u00eener ses mod\u00e8les, et les conserver jusqu'\u00e0 30 jours pour d\u00e9tecter les abus (",
			shiny::tags$a(
				href = "https://developers.openai.com/api/docs/guides/your-data",
				target = "_blank", rel = "noopener",
				"source"
			),
			")."
		),

		shiny::tags$h3("Ses limites"),
		shiny::tags$ul(
			class = "cq-ia-limites",
			shiny::tags$li(
				"La v\u00e9rification r\u00e9duit les erreurs, elle ne les supprime pas toutes. ",
				"En cas de doute, les sources officielles (onglet \u00ab Ressources \u00bb) font foi."
			),
			shiny::tags$li(
				"Une question vous semble fausse ou confuse ? Dites-le dans vos avis de fin de s\u00e9ance : ",
				"ils servent \u00e0 corriger la banque."
			),
			shiny::tags$li(
				"Sans IA, l'application fonctionne enti\u00e8rement : vous recevez les questions de la banque, ",
				"et le bilan est calcul\u00e9 sans texte r\u00e9dig\u00e9."
			)
		),
		footer = shiny::modalButton("J'ai compris")
	)
}

#' Fenêtre « deuxième surprise » : l'IA vient de s'ouvrir
#'
#' Affichée au centre de l'écran, une seule fois, quand le compte franchit le
#' seuil de questions différentes (voir ia_etat() dans fct_progression.R).
#'
#' @param etat Résultat de [ia_etat()].
#'
#' @return Un `shiny::modalDialog()`.
#' @noRd
cq_ia_surprise <- function(etat) {
	nouveaute <- function(icone, titre, texte) {
		shiny::div(
			class = "cq-ia-nouveaute",
			shiny::tags$span(class = "cq-ia-nouveaute-icone", bsicons::bs_icon(icone)),
			shiny::tags$strong(titre),
			shiny::tags$p(texte)
		)
	}

	shiny::modalDialog(
		title = NULL,
		size = "l",
		easyClose = TRUE,
		class = "cq-ia-surprise",
		shiny::tags$p(class = "eyebrow muted", "Deuxi\u00e8me surprise"),
		shiny::tags$h2(
			sprintf("%d questions diff\u00e9rentes. ", etat$seuil),
			shiny::tags$em("La r\u00e9daction par IA est ouverte.")
		),
		shiny::tags$p(
			class = "cq-ia-chapo",
			"Vous avez parcouru une bonne partie de la banque. L'IA peut maintenant r\u00e9\u00e9crire ",
			"ses questions pour vous les faire revoir autrement. Voici ce qui change."
		),
		shiny::div(
			class = "cq-ia-nouveautes",
			nouveaute(
				"ui-checks", "De nouvelles formes de questions",
				"Vrai ou faux, remise en ordre, texte \u00e0 trous, plusieurs bonnes r\u00e9ponses."
			),
			nouveaute(
				"arrow-repeat", "Des questions qui se renouvellent",
				"Les m\u00eames notions, formul\u00e9es autrement d'une s\u00e9ance \u00e0 l'autre."
			),
			nouveaute(
				"shield-check", "Des faits v\u00e9rifi\u00e9s",
				"Chaque question r\u00e9\u00e9crite est contr\u00f4l\u00e9e par rapport \u00e0 sa fiche, ou remplac\u00e9e."
			),
			nouveaute(
				"file-earmark-text", "Un bilan r\u00e9dig\u00e9",
				"Un texte personnalis\u00e9 en fin de s\u00e9ance, \u00e0 partir de vos r\u00e9sultats."
			)
		),
		shiny::tags$p(
			class = "cq-ia-ou",
			bsicons::bs_icon("stars"),
			" Pour l'activer : \u00ab S'entra\u00eener \u00bb, \u00e9tape 2, carte \u00ab Questions r\u00e9dig\u00e9es par l'IA \u00bb."
		),
		if (!llm_cle_disponible() && llm_cle_saisie_permise()) {
			shiny::tags$p(
				class = "muted",
				"Il faut pour cela votre propre cl\u00e9 OpenAI : l'application vous la demande quand vous choisissez \u00ab Avec IA \u00bb."
			)
		},
		footer = shiny::tagList(
			cq_ia_lien("Comment l'IA fonctionne ici"),
			shiny::tags$button(
				type = "button",
				class = "btn btn-primary primary",
				`data-bs-dismiss` = "modal",
				onclick = "Shiny.setInputValue('nav_seances', Math.random());",
				"Composer une s\u00e9ance"
			)
		)
	)
}

#' Schéma des 4 étapes pour obtenir une clé OpenAI
#'
#' Dessin simple, en SVG intégré : quatre cases reliées par des flèches. Les
#' couleurs suivent le texte (`currentColor`), clair comme sombre.
#'
#' @return Un `shiny::HTML()`.
#' @noRd
cq_cle_schema <- function() {
	etapes <- c("Compte", "Cr\u00e9dit", "Cl\u00e9", "Coller")
	sous <- c("platform.openai.com", "5 $ minimum", "sk-...", "dans l'appli")
	cases <- vapply(seq_along(etapes), function(i) {
		x <- 10 + (i - 1L) * 160
		fleche <- if (i < length(etapes)) {
			sprintf(
				'<path d="M%d 45 h22" stroke="currentColor" stroke-width="1.5" fill="none" marker-end="url(#cq-fleche)"/>',
				x + 128
			)
		} else {
			""
		}
		sprintf(
			paste0(
				'<rect x="%d" y="10" width="126" height="70" rx="10" fill="none" stroke="currentColor" stroke-width="1.2"/>',
				'<circle cx="%d" cy="10" r="11" fill="currentColor"/>',
				'<text x="%d" y="14" text-anchor="middle" font-size="12" font-weight="700" class="cq-cle-num">%d</text>',
				'<text x="%d" y="44" text-anchor="middle" font-size="15" font-weight="600" fill="currentColor">%s</text>',
				'<text x="%d" y="64" text-anchor="middle" font-size="11" fill="currentColor" opacity="0.75">%s</text>%s'
			),
			x, x + 18, x + 18, i, x + 63, etapes[[i]], x + 63, sous[[i]], fleche
		)
	}, character(1))

	shiny::HTML(paste0(
		'<svg class="cq-cle-schema" viewBox="0 -6 640 96" role="img" ',
		'aria-label="Quatre \u00e9tapes : cr\u00e9er un compte, ajouter du cr\u00e9dit, cr\u00e9er la cl\u00e9, la coller dans l\'application.">',
		'<defs><marker id="cq-fleche" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto">',
		'<path d="M0 0 L10 5 L0 10 z" fill="currentColor"/></marker></defs>',
		paste(cases, collapse = ""),
		"</svg>"
	))
}

#' Fenêtre « Obtenir une clé OpenAI en 4 étapes »
#'
#' Version à télécharger seulement. Les montants et libellés viennent de
#' l'aide d'OpenAI (help.openai.com, facturation prépayée) : à revérifier si
#' OpenAI change son site.
#'
#' @return Un `shiny::modalDialog()`.
#' @noRd
cq_cle_guide_modal <- function() {
	etape <- function(num, titre, ...) {
		shiny::tags$li(
			class = "cq-ia-etape",
			shiny::tags$span(class = "cq-ia-etape-num", num),
			shiny::div(shiny::tags$strong(titre), shiny::tags$p(...))
		)
	}
	lien <- function(url, texte) {
		shiny::tags$a(href = url, target = "_blank", rel = "noopener noreferrer", texte)
	}

	shiny::modalDialog(
		title = NULL,
		size = "l",
		easyClose = TRUE,
		class = "cq-ia-modal cq-cle-modal",
		shiny::tags$p(class = "eyebrow muted", "Votre propre cl\u00e9"),
		shiny::tags$h2("Obtenir une cl\u00e9 OpenAI en 4 \u00e9tapes"),
		shiny::tags$p(
			"Une cl\u00e9 est un long mot de passe qui permet \u00e0 l'application de demander des questions \u00e0 l'IA d'OpenAI, ",
			"sur votre compte. Comptez une dizaine de minutes, une seule fois."
		),
		cq_cle_schema(),
		shiny::tags$ol(
			class = "cq-ia-etapes",
			etape(
				"1", "Cr\u00e9er un compte d\u00e9veloppeur",
				"Sur ", lien("https://platform.openai.com", "platform.openai.com"),
				". C'est un compte diff\u00e9rent d'un abonnement ChatGPT : un abonnement ChatGPT ne donne pas de cl\u00e9."
			),
			etape(
				"2", "Ajouter un petit cr\u00e9dit pr\u00e9pay\u00e9",
				"Menu Settings, puis Billing : ajoutez une carte et achetez du cr\u00e9dit (5 $ minimum). ",
				"Laissez la recharge automatique d\u00e9sactiv\u00e9e : quand le cr\u00e9dit est \u00e9puis\u00e9, l'IA s'arr\u00eate, sans d\u00e9pense surprise. ",
				"Le cr\u00e9dit achet\u00e9 expire au bout d'un an."
			),
			etape(
				"3", "Cr\u00e9er la cl\u00e9",
				"Page ", lien("https://platform.openai.com/api-keys", "API keys"),
				", bouton \u00ab Create new secret key \u00bb. Copiez-la tout de suite : OpenAI ne l'affiche qu'une fois."
			),
			etape(
				"4", "La coller ici",
				"Dans le champ \u00ab Votre cl\u00e9 OpenAI \u00bb, puis \u00ab Utiliser cette cl\u00e9 \u00bb. ",
				"Elle reste sur votre ordinateur et n'est envoy\u00e9e qu'\u00e0 OpenAI. ",
				"\u00c0 la prochaine ouverture de l'application, collez-la de nouveau (ou voyez le README pour la garder)."
			)
		),
		shiny::tags$p(
			class = "muted",
			"Traitez-la comme un mot de passe : ne la partagez pas. En cas de doute, supprimez-la sur la page API keys et cr\u00e9ez-en une autre."
		),
		footer = shiny::modalButton("J'ai compris")
	)
}
