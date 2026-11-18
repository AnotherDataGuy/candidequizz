# Launch the ShinyApp (Do not remove this comment)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE, shiny.sanitize.errors = TRUE)
# Sans préfixe `candidequizz::` : sinon rsconnect::writeManifest() croit que
# l'application dépend d'un paquet « candidequizz » à installer depuis CRAN.
run_app()
