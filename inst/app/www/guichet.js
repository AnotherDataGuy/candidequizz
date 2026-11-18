// Illustration du guichet : ouverture / fermeture.

(function () {
  function bind() {
    var scene = document.getElementById("guichet-scene");
    var btn = document.getElementById("open-guichet");
    var label = document.getElementById("guichet-label");
    var caption = document.getElementById("scene-caption");

    if (!scene || !btn || btn.dataset.bound === "1") {
      return;
    }

    btn.dataset.bound = "1";

    var captions = {
      closed: "Ici, on peut s'entraîner à toute heure.",
      open: "Ici, on peut s'entraîner à toute heure."
    };

    btn.addEventListener("click", function () {
      var ouvert = scene.classList.toggle("is-open");
      btn.setAttribute("aria-pressed", ouvert ? "true" : "false");
      if (label) {
        label.textContent = ouvert ? "Refermer le guichet" : "Et si on ouvrait ?";
      }
      if (caption) {
        caption.textContent = ouvert ? captions.open : captions.closed;
      }
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bind);
  } else {
    bind();
  }

  if (window.jQuery) {
    $(document).on("shiny:idle", bind);
  }
})();
