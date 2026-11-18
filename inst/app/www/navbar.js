// Dock flottant : section active, collapse, crumb, manifeste.

var CQ_CRUMBS = {
  accueil: "Touchez un thème pour en savoir plus.",
  seances: "À votre rythme",
  statistiques: "Le carnet de progression",
  ressources: "Pour aller plus loin"
};

function cqRelancerEntree(pane) {
  if (!pane) {
    return;
  }

  pane.classList.remove("cq-enter");
  void pane.offsetWidth;
  pane.classList.add("cq-enter");
}

function cqMajCrumb(section) {
  var crumb = document.getElementById("cq-crumb");
  if (crumb && CQ_CRUMBS[section]) {
    crumb.textContent = CQ_CRUMBS[section];
  }
}

function cqActiverNav(section) {
  document.querySelectorAll(".dock-item").forEach(function (item) {
    var actif = item.getAttribute("data-section") === section;
    item.classList.toggle("is-actif", actif);
    if (actif) {
      item.setAttribute("aria-current", "page");
    } else {
      item.removeAttribute("aria-current");
    }
  });

  var carousel = document.querySelector(".cq-carousel");
  if (carousel) {
    carousel.hidden = section !== "accueil";
  }

  cqMajCrumb(section);
  cqRelancerEntree(document.querySelector(".tab-pane.active"));
}

$(function () {
  Shiny.addCustomMessageHandler("cq_nav_active", function (section) {
    cqActiverNav(section);
  });

  // Délégation : en version en ligne, le dock n'arrive qu'après la connexion.
  document.addEventListener("click", function (event) {
    var toggle = event.target.closest("#cq-dock-toggle");
    var dock = document.getElementById("cq-dock");
    if (toggle && dock) {
      var collapsed = dock.classList.toggle("is-collapsed");
      toggle.setAttribute("aria-expanded", collapsed ? "false" : "true");
      toggle.setAttribute(
        "aria-label",
        collapsed ? "Ouvrir le menu" : "Réduire le menu"
      );
      return;
    }

    var opener = event.target.closest("[data-manifesto]");
    if (opener) {
      var dlg = document.getElementById("manifesto");
      if (dlg && typeof dlg.showModal === "function") {
        dlg.showModal();
      }
      return;
    }

    if (event.target.closest("#close-manifesto")) {
      var m = document.getElementById("manifesto");
      if (m) {
        m.close();
      }
    }
  });
});
