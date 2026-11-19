/* Carrousel thématique.
 *
 * La bande est une zone qui défile (CSS : overflow-x auto) : au doigt ou au
 * pavé tactile, on la fait glisser dans les deux sens. Ce script ajoute :
 *  - le défilement automatique lent, en pause pendant un toucher, un survol,
 *    un focus clavier ou une fenêtre ouverte, puis reprise après quelques
 *    secondes ;
 *  - les flèches gauche / droite (une vignette à la fois) ;
 *  - la boucle sans fin : la piste contient trois fois les vignettes
 *    (R/utils_ui.R), on saute d'une copie à l'autre sans que cela se voie.
 * Réduction de mouvement demandée : pas de défilement automatique, le
 * glissement et les flèches restent.
 */
(function () {
  var VITESSE = 28;      // pixels par seconde
  var REPRISE_MS = 3500; // délai avant la reprise après une interaction

  function init() {
    var root = document.querySelector(".cq-carousel");
    if (!root || root.dataset.cqInit) return;
    var viewport = root.querySelector(".cq-carousel-viewport");
    var slides = root.querySelectorAll(".cq-carousel-slide");
    if (!viewport || slides.length < 2) return;
    root.dataset.cqInit = "1";

    var reduce = window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    // Largeur d'une copie de la bande : écart entre la 1re vignette et sa
    // copie (première vignette marquée aria-hidden).
    function periode() {
      var copie = root.querySelector(".cq-carousel-slide[aria-hidden='true']");
      return copie ? copie.offsetLeft - slides[0].offsetLeft : 0;
    }

    // Garder la position dans ]0, p] : on peut toujours glisser dans les
    // deux sens, et le saut d'une copie à l'autre ne se voit pas.
    var glisseJusqua = 0; // flèche en cours : pas de saut de boucle

    function boucler() {
      if (Date.now() < glisseJusqua) return;
      var p = periode();
      if (p <= 0) return;
      if (viewport.scrollLeft > p) {
        viewport.scrollLeft -= p;
      } else if (viewport.scrollLeft <= 0) {
        viewport.scrollLeft += p;
      }
    }

    // Départ sur la copie : même image qu'au début, mais on peut aussi
    // glisser vers la gauche tout de suite.
    viewport.scrollLeft = periode();

    var survol = false;
    var pauseJusqua = 0;
    var position = viewport.scrollLeft;

    function pauser() {
      pauseJusqua = Date.now() + REPRISE_MS;
    }

    root.addEventListener("mouseenter", function () { survol = true; });
    root.addEventListener("mouseleave", function () { survol = false; });
    ["touchstart", "touchmove", "touchend", "pointerdown", "wheel", "focusin"].forEach(function (ev) {
      root.addEventListener(ev, pauser, { passive: true });
    });

    viewport.addEventListener("scroll", function () {
      boucler();
      // Ne reprendre la position que si elle a vraiment bougé (doigt, flèche,
      // boucle) : sinon l'arrondi de scrollLeft mangerait les pas < 1 px.
      if (Math.abs(viewport.scrollLeft - position) > 1) {
        position = viewport.scrollLeft;
      }
    }, { passive: true });

    root.querySelectorAll("[data-carousel-dir]").forEach(function (btn) {
      btn.addEventListener("click", function () {
        pauser();
        var sens = Number(btn.getAttribute("data-carousel-dir")) || 1;
        var pas = slides[0].getBoundingClientRect().width + 14;
        // Vers la gauche près du début : sauter d'abord sur la copie
        // équivalente, sinon le défilement buterait sur le bord.
        if (sens < 0 && viewport.scrollLeft - pas <= 0) {
          viewport.scrollLeft += periode();
        }
        // Pendant le glissement doux, la boucle attend (sinon elle
        // l'interromprait) ; elle reprend juste après.
        glisseJusqua = Date.now() + 700;
        viewport.scrollBy({ left: sens * pas, behavior: reduce ? "auto" : "smooth" });
        window.setTimeout(function () {
          boucler();
          position = viewport.scrollLeft;
        }, 750);
      });
    });

    if (reduce) return;

    var avant = null;
    function tick(t) {
      var dt = avant === null ? 0 : Math.min((t - avant) / 1000, 0.1);
      avant = t;
      // Bande entièrement visible (très grand écran) : rien à faire défiler.
      var tient = viewport.scrollWidth - viewport.clientWidth < periode();
      var arrete = tient || survol || Date.now() < pauseJusqua || root.hidden ||
        document.hidden || document.body.classList.contains("modal-open");
      if (!arrete) {
        // Position gardée en décimales : certains navigateurs arrondissent
        // scrollLeft, ce qui bloquerait un pas de moins d'un pixel.
        position += VITESSE * dt;
        viewport.scrollLeft = position;
      }
      window.requestAnimationFrame(tick);
    }
    window.requestAnimationFrame(tick);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }

  // Version en ligne : le carrousel n'arrive qu'après la connexion.
  if (window.jQuery) {
    jQuery(document).on("shiny:idle", init);
  }
})();
