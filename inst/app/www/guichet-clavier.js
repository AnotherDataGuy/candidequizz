// Raccourcis clavier de l'écran de séance : A à D (ou 1 à 4) cochent une
// option, Entrée valide puis passe à la question suivante.
// Inactifs pendant la saisie d'un texte ou quand une fenêtre est ouverte.
(function () {
  function visible(el) {
    return !!(el && el.offsetParent !== null && !el.disabled);
  }

  document.addEventListener('keydown', function (e) {
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    var passation = document.querySelector('.cq-passation');
    if (!visible(passation)) return;
    if (document.querySelector('.modal.show')) return;
    var cible = e.target;
    if (cible && (cible.tagName === 'TEXTAREA' || (cible.tagName === 'INPUT' && cible.type === 'text') || cible.isContentEditable)) return;

    var touche = e.key.toLowerCase();
    var rang = 'abcd'.indexOf(touche);
    if (rang < 0) rang = '1234'.indexOf(touche);

    if (rang >= 0) {
      var options = passation.querySelectorAll('.cq-fiche .cq-reponse .radio input, .cq-fiche .cq-reponse .checkbox input');
      if (options[rang]) {
        options[rang].click();
        e.preventDefault();
      }
      return;
    }

    if (e.key === 'Enter' && !(cible && cible.tagName === 'BUTTON')) {
      var bouton = ['valider', 'suivant', 'continuer_lot']
        .map(function (id) { return passation.querySelector('[id$="-run-' + id + '"]'); })
        .filter(visible)[0];
      if (bouton) {
        bouton.click();
        e.preventDefault();
      }
    }
  });
})();
