// Carnet de progression (fct_progression.R) :
// - le serveur indique si la section est ouverte : le bouton du dock
//   apparaît (avec un court signal la première fois) ;
// - Échap ferme la jauge quand elle est affichée.
(function () {
  // Shiny envoie ses messages avant de dessiner les sorties : à la connexion
  // (version en ligne), le message peut arriver avant le dock. Il est alors
  // gardé et appliqué dès que le bouton existe.
  var enAttente = null;

  function appliquer(msg) {
    var bouton = document.getElementById('nav_statistiques');
    if (!bouton) { enAttente = msg; return; }
    enAttente = null;
    bouton.hidden = !msg.debloque;
    if (msg.nouveau) {
      bouton.classList.add('is-nouveau');
      setTimeout(function () { bouton.classList.remove('is-nouveau'); }, 4500);
    }
  }

  function lier() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler || !window.jQuery) { setTimeout(lier, 50); return; }
    Shiny.addCustomMessageHandler('cq-progression', appliquer);
    jQuery(document).on('shiny:idle', function () {
      if (enAttente) appliquer(enAttente);
    });
  }
  lier();

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape' && document.querySelector('.cq-jauge') && window.Shiny) {
      Shiny.setInputValue('jauge_fermer', Math.random(), { priority: 'event' });
    }
  });
})();
