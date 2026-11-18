/* The browser acknowledges a painted frame before R starts synchronous work.
 * This handshake keeps the loader visible even when the generation blocks R.
 * The dialog owns focus; closing it always restores access to the application. */
(function () {
  'use strict';
  let active = null;
  let timer = null;
  let origin = null;
  const reduced = window.matchMedia('(prefers-reduced-motion: reduce)');

  function stopCycle() {
    window.clearInterval(timer);
    timer = null;
  }
  function cycle() {
    stopCycle();
    if (!active || active.classList.contains('is-paused') || reduced.matches) return;
    timer = window.setInterval(function () {
      const questions = Array.from(active.querySelectorAll('.cq-loading-question'));
      if (questions.length < 2) return;
      const index = questions.findIndex(q => q.classList.contains('is-active'));
      questions[index].classList.remove('is-active');
      questions[(index + 1) % questions.length].classList.add('is-active');
    }, 6500);
  }
  function close(focusId) {
    stopCycle();
    if (!active) return;
    active.close();
    active = null;
    document.body.classList.remove('cq-is-loading');
    const target = (focusId && document.getElementById(focusId)) || origin;
    if (target && target.isConnected) target.focus({ preventScroll: true });
  }
  function register() {
    Shiny.addCustomMessageHandler('cq-quiz-loading', function (message) {
      if (message.action === 'hide') { close(message.focusId); return; }
      const dialog = document.getElementById(message.id);
      if (!dialog) return;
      origin = document.activeElement;
      active = dialog;
      dialog.classList.toggle('is-paused', reduced.matches);
      const pause = dialog.querySelector('.cq-loading-pause');
      function updatePause() {
        const paused = dialog.classList.contains('is-paused');
        pause.setAttribute('aria-pressed', String(paused));
        pause.textContent = paused ? 'Reprendre l’animation' : 'Mettre l’animation en pause';
      }
      pause.onclick = function () {
        dialog.classList.toggle('is-paused');
        updatePause();
        cycle();
      };
      dialog.oncancel = function (event) { event.preventDefault(); };
      updatePause();
      if (!dialog.open) dialog.showModal();
      document.body.classList.add('cq-is-loading');
      cycle();
      window.requestAnimationFrame(function () {
        window.requestAnimationFrame(function () {
          Shiny.setInputValue(message.readyId, message.token, { priority: 'event' });
        });
      });
    });
  }
  reduced.addEventListener('change', function () {
    if (active && reduced.matches) active.classList.add('is-paused');
    cycle();
  });
  $(document).on('shiny:connected', register);
  $(document).on('shiny:disconnected', function () { close(); });
})();
