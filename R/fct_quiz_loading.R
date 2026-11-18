# Le décor ne contient que des énoncés de référence, jamais leurs réponses.
# Une sélection déterministe préserve le générateur aléatoire de la séance.
quiz_loading_questions <- function(bank = bank_load(), limite = 12L) {
	questions <- unique(trimws(bank$graines$question_ref))
	questions <- questions[!is.na(questions) & nzchar(questions)]
	if (!length(questions)) return(character(0))
	questions[unique(round(seq(1, length(questions), length.out = min(limite, length(questions)))))]
}

quiz_loading_ui <- function(id) {
	questions <- tryCatch(quiz_loading_questions(), error = function(e) character(0))
	shiny::tags$dialog(id = id, class = "cq-loading", `aria-label` = "Pr\u00e9paration de votre s\u00e9ance",
		shiny::div(class = "cq-loading-frame",
			shiny::div(class = "cq-loading-header",
				shiny::span(class = "eyebrow", "Candide au guichet"),
				shiny::span(class = "cq-loading-ticket", "Votre place se pr\u00e9pare")),
			shiny::div(class = "cq-loading-stage", `aria-hidden` = "true",
				shiny::div(class = "cq-loading-orbit"),
				shiny::div(class = "cq-loading-arch"),
				shiny::span(class = "cq-loading-star", "\u2733"),
				shiny::span(class = "cq-loading-stamp", "LA CURIOSIT\u00c9\nOUVRE DES PORTES"),
				lapply(seq_along(questions), function(i) {
					mots <- strsplit(questions[[i]], "\\s+")[[1L]]
					shiny::div(class = paste("cq-loading-question", if (i == 1L) "is-active" else ""),
						shiny::span(class = "cq-loading-index", sprintf("Dans le carnet / %02d", i)),
						shiny::div(class = "cq-loading-words", lapply(seq_along(mots), function(j) {
							shiny::span(class = paste0("cq-loading-word word-", (j - 1L) %% 4L),
								style = paste0("--word-delay:", (j - 1L) * 35, "ms"), mots[[j]])
						})))
				})),
			shiny::div(class = "cq-loading-footer",
				shiny::div(role = "status", `aria-live` = "polite",
					shiny::h2("Un instant, les id\u00e9es prennent place."),
					shiny::p("Nous pr\u00e9parons les questions de votre s\u00e9ance.")),
				shiny::div(class = "cq-loading-rail", `aria-hidden` = "true", shiny::span()),
				shiny::div(class = "cq-loading-bottom",
					shiny::span("Des questions du carnet, pour \u00e9veiller la curiosit\u00e9."),
					shiny::tags$button(type = "button", class = "cq-loading-pause", `aria-pressed` = "false", "Mettre l\u2019animation en pause")))))
}
