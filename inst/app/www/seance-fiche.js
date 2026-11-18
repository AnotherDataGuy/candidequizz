// Paramétrage séquentiel : une étape à la fois (guichet → longueur → thèmes).
// Toutes les entrées restent dans la page : on ne fait qu'afficher l'étape
// courante (attribut data-etape du conteneur .cq-jeu). Le serveur ne change pas.

(function () {
  var TOTAL = 3;
  var NOMS = { 2: 'longueur', 3: 'th\u00e8mes' };

  function racine(el) {
    return el ? el.closest('.cq-jeu') : document.querySelector('.cq-jeu');
  }

  // Le choix retenu à chaque étape, affiché sous son nom dans le fil.
  function resumer(root) {
    if (!root) return;
    var profil = root.querySelector('.jeu-guichets input:checked');
    var examen = profil && profil.value === 'examen_blanc';
    var titre = profil && profil.closest('label').querySelector('.jeu-guichet-titre');

    var nb = root.querySelector('.jeu-longueurs input:checked');
    var themes = root.querySelectorAll('.jeu-themes input');
    var coches = root.querySelectorAll('.jeu-themes input:checked').length;

    var textes = {
      1: titre ? titre.textContent : '—',
      2: examen ? '40 questions' : (nb ? nb.value + ' questions' : '—'),
      3: !profil ? '—'
        : examen ? 'tout le programme'
        : profil.value === 'revision_ciblee' ? 'automatiques'
        : (coches === 0 || coches === themes.length) ? 'tous'
        : coches + ' / ' + themes.length
    };

    Object.keys(textes).forEach(function (k) {
      var cible = root.querySelector('[data-resume="' + k + '"]');
      if (cible) cible.textContent = textes[k];
    });
  }

  function aller(root, n, silencieux) {
    if (!root) return;
    n = Math.max(1, Math.min(TOTAL, parseInt(n, 10) || 1));
    root.setAttribute('data-etape', String(n));

    // Le bouton sous la fiche mène à l'étape suivante et la nomme.
    var suivant = root.querySelector('.jeu-suivant');
    if (suivant && n < TOTAL) {
      suivant.setAttribute('data-aller', String(n + 1));
      var texte = suivant.querySelector('.jeu-suivant-texte');
      if (texte) texte.textContent = 'Continuer : ' + NOMS[n + 1];
    }

    root.querySelectorAll('.jeu-fil button').forEach(function (b) {
      var k = parseInt(b.getAttribute('data-aller'), 10);
      b.setAttribute('aria-current', k === n ? 'step' : 'false');
      b.classList.toggle('is-fait', k < n);
    });

    // Remonter en haut du paramétrage si on l'a dépassé, puis donner le
    // focus au titre de l'étape (lecteurs d'écran, clavier).
    if (silencieux) { resumer(root); return; }
    var haut = root.getBoundingClientRect().top + window.scrollY - 16;
    if (window.scrollY > haut) window.scrollTo({ top: haut, behavior: 'smooth' });
    var titre = root.querySelector('.jeu-etape[data-etape-num="' + n + '"] h2');
    if (titre) titre.focus({ preventScroll: true });

    resumer(root);
  }

  $(document).on('click', '.cq-jeu [data-aller]', function () {
    aller(racine(this), this.getAttribute('data-aller'));
  });

  $(document).on('change shiny:inputchanged', function () {
    resumer(racine());
  });

  // Thème choisi depuis l'accueil : le serveur demande l'étape des thèmes.
  $(function () {
    var root = racine();
    if (root) aller(root, root.getAttribute('data-etape') || 1, true);
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('cq-config-etape', function (n) {
        aller(racine(), n);
      });
    }
  });

  // Au départ, la fiche reçoit son tampon « Prêt » pendant que les
  // questions se préparent.
  $(document).on('click', '.jeu-prendre', function () {
    var fiche = document.querySelector('.jeu-ticket');
    if (fiche && !fiche.classList.contains('is-incomplet')) {
      fiche.classList.add('is-tamponne');
    }
  });
})();
