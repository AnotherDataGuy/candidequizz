// Jeton de connexion (version en ligne, mode « comptes »).
//
// Le serveur ne garde que l'empreinte du jeton (voir R/fct_auth.R) ; le
// navigateur garde le jeton lui-même dans localStorage, pour rester connecté
// d'une visite à l'autre.
//
// - Au chargement, le jeton enregistré est envoyé à Shiny dans l'entrée
//   `cq_auth_token` ; app_server.R le résout en compte (auth_token_resoudre()).
// - Le serveur envoie le message `cq-auth-token` avec `action: "set"` après une
//   connexion réussie (mod_auth.R), ou `action: "clear"` à la déconnexion et
//   quand le jeton est refusé (app_server.R).
//
// Chargé automatiquement avec les autres fichiers .js de www/ (app_ui.R).
(function () {
  var CLE = 'cq-auth-token';

  // Envoyer le jeton enregistré au serveur, s'il y en a un.
  function envoyer() {
    try {
      var jeton = localStorage.getItem(CLE);
      if (jeton && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('cq_auth_token', jeton, { priority: 'event' });
      }
    } catch (e) {
      // localStorage indisponible (navigation privée stricte) : pas de reconnexion.
    }
  }

  function lier() {
    // Shiny et jQuery arrivent après ce script : attendre qu'ils soient là.
    if (!window.Shiny || !window.jQuery) {
      setTimeout(lier, 40);
      return;
    }
    // Ne lier qu'une fois.
    if (window.cqAuthLie) {
      return;
    }
    window.cqAuthLie = true;

    Shiny.addCustomMessageHandler('cq-auth-token', function (msg) {
      try {
        if (msg.action === 'set' && msg.token) {
          localStorage.setItem(CLE, msg.token);
        }
        if (msg.action === 'clear') {
          localStorage.removeItem(CLE);
        }
      } catch (e) {
        // Rien à faire : la session reste valable jusqu'à la fermeture de l'onglet.
      }
    });

    $(document).on('shiny:connected', envoyer);

    // Si la connexion Shiny est déjà établie (l'événement est passé),
    // réessayer quelques fois pendant ~3 s.
    var essais = 0;
    (function retenter() {
      essais += 1;
      if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
        envoyer();
      }
      if (essais < 25) {
        setTimeout(retenter, 120);
      }
    })();
  }

  lier();
})();
