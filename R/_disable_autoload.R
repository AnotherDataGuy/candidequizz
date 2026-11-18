# Fichier vide voulu : sa seule présence empêche Shiny de sourcer le dossier R/
# au démarrage (app.R charge déjà le paquet avec pkgload::load_all()).
# Sans lui, run_app() pointe vers une copie hors du paquet et plante
# (« impossible de trouver la fonction with_golem_options »).
