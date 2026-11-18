// Thème clair / sombre, persisté localement.

(function () {
  var CLE = "cq-theme";

  function prefereSombre() {
    return window.matchMedia &&
      window.matchMedia("(prefers-color-scheme: dark)").matches;
  }

  function normalise(theme) {
    if (theme === "nuit") return "dark";
    if (theme === "jour") return "light";
    return theme === "dark" ? "dark" : "light";
  }

  function lu() {
    try {
      return localStorage.getItem(CLE);
    } catch (e) {
      return null;
    }
  }

  function actuel() {
    return normalise(lu() || (prefereSombre() ? "dark" : "light"));
  }

  function bouton() {
    return document.getElementById("cq-theme-toggle");
  }

  function applique(theme) {
    theme = normalise(theme);
    document.documentElement.setAttribute("data-theme", theme);
    document.documentElement.style.colorScheme = theme === "dark" ? "dark" : "light";

    var btn = bouton();
    if (!btn) {
      return;
    }

    btn.setAttribute("data-theme-actuel", theme);
    btn.setAttribute(
      "aria-label",
      theme === "dark" ? "Activer le thème clair" : "Activer le thème sombre"
    );
  }

  function bascule() {
    var suivant = document.documentElement.getAttribute("data-theme") === "dark"
      ? "light"
      : "dark";

    try {
      localStorage.setItem(CLE, suivant);
    } catch (e) {
      /* stockage indisponible */
    }

    applique(suivant);
  }

  applique(actuel());

  document.addEventListener("click", function (event) {
    if (event.target.closest("#cq-theme-toggle")) {
      bascule();
    }
  });
})();
