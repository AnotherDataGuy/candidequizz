// Écran « Vos avis » :
// - compteur « 7 / 10 questions notées », mis à jour à chaque note ;
// - à la validation, le serveur envoie les questions sans note : elles sont
//   marquées et l'écran défile jusqu'à la première ;
// - une question marquée perd sa marque dès qu'on lui donne une note.
(function () {
  function compter() {
    // Seules les questions nouvelles comptent : celles déjà notées lors d'une
    // séance précédente (.is-deja) sont facultatives.
    var items = document.querySelectorAll('.cq-avis-item:not(.is-deja)');
    var compteur = document.querySelector('.cq-avis-compteur');
    if (!compteur) return;
    if (!items.length) {
      compteur.textContent = document.querySelector('.cq-avis-item') ? 'Questions déjà notées' : '';
      compteur.classList.add('is-complet');
      return;
    }
    var notees = 0;
    items.forEach(function (it) { if (it.querySelector('.cq-avis-echelle input:checked')) notees += 1; });
    compteur.textContent = notees + ' / ' + items.length + ' questions notées';
    compteur.classList.toggle('is-complet', notees === items.length);
  }

  document.addEventListener('change', function (e) {
    var item = e.target.closest && e.target.closest('.cq-avis-item');
    if (!item || !e.target.matches('.cq-avis-echelle input')) return;
    item.classList.remove('is-manquante');
    compter();
  });

  function lier() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler || !window.jQuery) { setTimeout(lier, 50); return; }
    // La liste est dessinée par Shiny (événement jQuery) : recompter quand elle arrive.
    jQuery(document).on('shiny:value', function (e) {
      if (e.name && /avis-liste$/.test(e.name)) { setTimeout(compter, 60); }
    });
    Shiny.addCustomMessageHandler('cq-avis-manquantes', function (msg) {
      var ids = [].concat(msg.ids || []);
      ids.forEach(function (id) {
        var el = document.getElementById(id);
        if (el) el.classList.add('is-manquante');
      });
      var premier = ids.length && document.getElementById(ids[0]);
      if (premier) {
        premier.scrollIntoView({ behavior: 'smooth', block: 'center' });
        var radio = premier.querySelector('.cq-avis-echelle input');
        if (radio) radio.focus({ preventScroll: true });
      }
    });
  }
  lier();
})();
