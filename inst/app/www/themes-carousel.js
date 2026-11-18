/* Carrousel thématique : pause si réduction de mouvement, sinon CSS. */
(function () {
  function init() {
    var root = document.querySelector(".cq-carousel");
    if (!root || root.dataset.cqInit) return;
    root.dataset.cqInit = "1";

    var reduce = window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    if (reduce) {
      root.classList.add("cq-carousel--static");
    }
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
