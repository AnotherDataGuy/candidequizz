/* Fenêtre « Un problème ? Une suggestion ? » : compteur de caractères.
 * Délégation : la fenêtre est créée par le serveur à chaque ouverture. */
(function () {
  document.addEventListener("input", function (event) {
    var zone = event.target;
    if (!zone || zone.tagName !== "TEXTAREA" || !zone.id) return;
    var compteur = document.querySelector('.cq-fb-compteur[data-pour="' + zone.id + '"]');
    if (!compteur) return;
    var max = Number(compteur.getAttribute("data-max")) || 2000;
    var n = zone.value.length;
    compteur.textContent = n + " / " + max;
    compteur.classList.toggle("is-presque", n > max * 0.9);
  });
})();
