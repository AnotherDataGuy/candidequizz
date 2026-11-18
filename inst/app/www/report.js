// Onglets des rapports d'analytics (bilan + progression).

(function () {
  function activate(root, panel) {
    if (!root) {
      return;
    }

    root.querySelectorAll(".report-tabs [data-report-panel]").forEach(function (btn) {
      var on = btn.getAttribute("data-report-panel") === panel;
      btn.setAttribute("aria-selected", on ? "true" : "false");
    });

    root.querySelectorAll(".report-panel-pane").forEach(function (pane) {
      pane.classList.toggle("hidden", pane.getAttribute("data-panel") !== panel);
    });
  }

  function filtrer(chip) {
    var deck = chip.closest(".revoir-deck");
    if (!deck) {
      return;
    }

    var valeur = chip.getAttribute("data-revoir-filtre");
    deck.setAttribute("data-filtre", valeur);
    deck.querySelectorAll("[data-revoir-filtre]").forEach(function (btn) {
      btn.setAttribute("aria-pressed", btn === chip ? "true" : "false");
    });

    var plus = deck.querySelector(".revoir-plus");
    if (plus && valeur !== "tout") {
      plus.open = true;
    }
  }

  document.addEventListener("click", function (event) {
    var chip = event.target.closest("[data-revoir-filtre]");
    if (chip) {
      filtrer(chip);
      return;
    }

    var tab = event.target.closest("[data-report-panel]");
    if (tab) {
      var root = tab.closest(".analytics");
      activate(root, tab.getAttribute("data-report-panel"));
      return;
    }

    var jump = event.target.closest("[data-report-tab]");
    if (jump) {
      var analytics = jump.closest(".analytics");
      activate(analytics, jump.getAttribute("data-report-tab"));

      var cible = jump.getAttribute("data-revoir-cible");
      var carte = cible ? document.getElementById(cible) : null;
      if (carte) {
        var tout = carte.closest(".revoir-deck");
        tout = tout ? tout.querySelector('[data-revoir-filtre="tout"]') : null;
        if (tout) {
          filtrer(tout);
        }
        carte.open = true;
        carte.scrollIntoView({ behavior: "smooth", block: "start" });
      }
    }
  });
})();
