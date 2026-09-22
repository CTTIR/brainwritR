#' Local application styles
#' @keywords internal
#' @noRd
app_css <- function() {
  "
:root { --ink:#22303c; --mut:#5b6b7a; --bg:#eceff2; --card:#ffffff;
        --accent:#0e6e78; --accent-soft:#eef6f6; --warn:#b3372b;
        --line:#dce3e7; --field-line:#9cabb7; --shadow:rgba(34,48,60,.10); }
:root[data-bs-theme='dark'] { --ink:#e3eaf0; --mut:#9fb0bd; --bg:#10171d; --card:#1a232b;
        --accent:#5cc0c9; --accent-soft:#18323a; --warn:#ff8a7a;
        --line:#2d3b46; --field-line:#51626f; --shadow:rgba(0,0,0,.45); }
body { font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
       background: var(--bg); color: var(--ink); }
.bw-wrap { max-width: 720px; margin: 0 auto; padding: 20px 16px 40px; }
.bw-card { background: var(--card); border: 1px solid var(--line); border-radius: 14px;
           padding: 20px; margin-bottom: 12px; box-shadow: 0 1px 2px var(--shadow); }
.bw-timer { font-variant-numeric: tabular-nums; font-size: 2.6rem;
            font-weight: 700; line-height: 1; color: var(--accent); }
.bw-timer.low { color: var(--warn); }
.bw-q { font-weight: 600; margin: 12px 0 6px; }
.bw-prev { border-left: 3px solid var(--accent); background: var(--accent-soft);
           padding: 6px 10px; margin: 6px 0; border-radius: 0 8px 8px 0; }
.bw-prev .who { font-size: .8rem; color: var(--mut); }
.bw-status { color: var(--mut); font-size: .9rem; }
textarea.form-control { font-size: 1rem; min-height: 92px; }
h1, h2, h3, h4 { letter-spacing: -.01em; overflow-wrap: anywhere; }
.bw-header { display: flex; flex-wrap: wrap; gap: 4px 16px; align-items: baseline;
             justify-content: space-between; padding: 4px 2px 18px; }
.bw-brand { display: flex; align-items: center; gap: 10px;
            font-size: 1.25rem; font-weight: 750; color: var(--accent); }
.bw-footer { text-align: center; font-size: .8rem; color: var(--mut); padding: 12px 0; }
.bw-footer-link { display: inline-flex; align-items: center; gap: 6px; color: var(--mut);
                  text-decoration: none; }
.bw-footer-link:hover { color: var(--accent); text-decoration: underline; }
.bw-modbar { display: flex; flex-wrap: wrap; gap: 8px 12px; align-items: center;
             justify-content: space-between; padding: 12px 16px; }
.bw-modbar-name { overflow-wrap: anywhere; }
.bw-weight-item { border-top: 1px solid var(--line); padding: 10px 0 4px; }
.bw-weight-text { white-space: pre-wrap; overflow-wrap: anywhere; margin: 2px 0 4px; }
.bw-question-heading { font-size: .9rem; color: var(--ink); margin: 4px 0 6px;
                       overflow-wrap: anywhere; white-space: normal; }
.bw-question-heading span { white-space: pre-wrap; }
.bw-weight-value { min-width: 3.5em; text-align: right; font-variant-numeric: tabular-nums;
                   font-weight: 600; color: var(--accent); }
.bw-weight { flex: 1; min-height: 32px; }
.bw-remaining { font-weight: 600; margin-bottom: 4px; }
.bw-rank { min-width: 3.5em; font-variant-numeric: tabular-nums; color: var(--accent); }
.bw-rank-row { display: flex; gap: 14px; align-items: flex-start; margin: 6px 0;
               border-left: 3px solid var(--accent); background: var(--accent-soft);
               padding: 8px 12px; border-radius: 0 8px 8px 0; }
.bw-rank-row .who { font-size: .8rem; color: var(--mut); }
.bw-rank-text { white-space: pre-wrap; overflow-wrap: anywhere; }
.btn { min-height: 44px; border-radius: 8px; font-weight: 600; }
a.btn { display: inline-flex; align-items: center; justify-content: center; gap: .4em; }
.form-control { min-height: 44px; border-color: var(--field-line); }
.bw-controls { display: flex; align-items: center; justify-content: flex-end; gap: 12px; }
.bw-theme-toggle { width: 44px; padding: 0; display: inline-flex; align-items: center;
                   justify-content: center; }
[data-bs-theme='dark'] .bw-icon-moon, :root:not([data-bs-theme='dark']) .bw-icon-sun {
  display: none; }
[data-bs-theme='dark'] .shiny-plot-output img { border-radius: 8px; }
[data-bs-theme='dark'] .form-range::-webkit-slider-thumb { background-color: var(--accent); }
[data-bs-theme='dark'] .form-range::-moz-range-thumb { background-color: var(--accent); }
[data-bs-theme='dark'] .form-check-input:checked {
  background-color: var(--accent); border-color: var(--accent); }
[data-bs-theme='dark'] .btn-outline-secondary, [data-bs-theme='dark'] .shiny-download-link {
  --bs-btn-color: var(--ink); --bs-btn-border-color: var(--field-line);
  --bs-btn-hover-color: var(--ink); --bs-btn-hover-bg: var(--line);
  --bs-btn-hover-border-color: var(--field-line); --bs-btn-active-bg: var(--line); }
[data-bs-theme='dark'] .btn-outline-primary {
  --bs-btn-color: var(--accent); --bs-btn-border-color: var(--accent);
  --bs-btn-hover-color: var(--bg); --bs-btn-hover-bg: var(--accent);
  --bs-btn-hover-border-color: var(--accent); }
[data-bs-theme='dark'] .btn-outline-danger {
  --bs-btn-color: var(--warn); --bs-btn-border-color: var(--warn);
  --bs-btn-hover-color: var(--bg); --bs-btn-hover-bg: var(--warn);
  --bs-btn-hover-border-color: var(--warn); }
:focus-visible { outline: 3px solid var(--accent); outline-offset: 3px; }
.bw-prev, .bw-status, code { overflow-wrap: anywhere; }
.bw-prev > div:last-child { white-space: pre-wrap; }
.bw-timer { white-space: nowrap; margin-left: 12px; }
.bw-card.text-center .bw-timer { font-size: 4.5rem; margin: 16px 0; }
@media (max-width: 420px) {
  .bw-wrap { padding: 12px 10px 28px; }
  .bw-card { padding: 16px; }
  .bw-timer { font-size: 2.2rem; }
}
"
}

#' Reconnect and resume handlers
#' @keywords internal
#' @noRd
app_js <- function() {
  "
// Colour theme: the stored choice, else the device preference. Applied before
// the page renders so it never flashes in the wrong theme.
(function() {
  // A toggle button keeps its label ('Dunkles Design'); only aria-pressed changes.
  function apply(theme) {
    document.documentElement.setAttribute('data-bs-theme', theme);
    var button = document.getElementById('bw-theme-toggle');
    if (button) button.setAttribute('aria-pressed', String(theme === 'dark'));
  }
  var saved = null;
  try { saved = localStorage.getItem('bw_theme'); } catch (e) {}
  var prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
  apply(saved === 'dark' || saved === 'light' ? saved : (prefersDark ? 'dark' : 'light'));
  document.addEventListener('DOMContentLoaded', function() {
    apply(document.documentElement.getAttribute('data-bs-theme'));
  });
  $(document).on('click', '#bw-theme-toggle', function() {
    var next = document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'light' : 'dark';
    try { localStorage.setItem('bw_theme', next); } catch (e) {}
    apply(next);
  });
})();
// Reload after disconnect to recover from mobile network interruptions.
$(document).on('shiny:disconnected', function() {
  setTimeout(function() { location.reload(); }, 1500);
});
// Each session keeps its own participant identity on this device.
var bwPidKey = (function() {
  var code = null;
  try { code = new URLSearchParams(location.search).get('s'); } catch (e) {}
  return code ? 'bw_pid:' + code.trim().toLowerCase() : 'bw_pid';
})();
// Send the stored identities on every connection; the moderator token lives per tab.
$(document).on('shiny:connected', function() {
  var pid = null, token = null;
  try { pid = localStorage.getItem(bwPidKey); } catch (e) {}
  try { token = sessionStorage.getItem('bw_mod_token'); } catch (e) {}
  Shiny.setInputValue('stored_pid', pid, {priority: 'event'});
  if (token) Shiny.setInputValue('mod_token', token, {priority: 'event'});
});
Shiny.addCustomMessageHandler('bw_store_pid', function(pid) {
  try { localStorage.setItem(bwPidKey, pid); } catch (e) {}
});
Shiny.addCustomMessageHandler('bw_clear_pid', function(x) {
  try { localStorage.removeItem(bwPidKey); } catch (e) {}
});
Shiny.addCustomMessageHandler('bw_store_mod_token', function(token) {
  try { sessionStorage.setItem('bw_mod_token', token); } catch (e) {}
});
Shiny.addCustomMessageHandler('bw_clear_mod_token', function(x) {
  try { sessionStorage.removeItem('bw_mod_token'); } catch (e) {}
});
// Plenum weighting: each topic card holds 100 percent. Sliders stop at the
// remaining budget while moving and report their value when released.
function bwWeightLabel(slider) {
  slider.parentElement.querySelector('.bw-weight-value').textContent = slider.value + ' %';
  slider.setAttribute('aria-valuetext', slider.value + ' %');
}
function bwRemaining(card) {
  var used = 0;
  card.querySelectorAll('.bw-weight').forEach(function(s) { used += Number(s.value); });
  card.querySelector('.bw-remaining').textContent = 'Noch ' + (100 - used) + ' % zu vergeben';
}
$(document).on('input', '.bw-weight', function() {
  var card = this.closest('[data-topic]'), self = this, others = 0;
  this.dataset.moving = '1';
  card.querySelectorAll('.bw-weight').forEach(function(s) {
    if (s !== self) others += Number(s.value);
  });
  if (Number(this.value) > 100 - others) this.value = 100 - others;
  bwWeightLabel(this);
  bwRemaining(card);
});
// A gesture ends on release even when the value is unchanged and no change fires.
$(document).on('pointerup pointercancel touchend blur', '.bw-weight', function() {
  delete this.dataset.moving;
});
$(document).on('change', '.bw-weight', function() {
  delete this.dataset.moving;
  Shiny.setInputValue('weight', {entry: Number(this.dataset.entry), points: Number(this.value),
    nonce: Date.now()}, {priority: 'event'});
});
Shiny.addCustomMessageHandler('bw_weight', function(x) {
  var slider = document.querySelector('.bw-weight[data-entry=\"' + x.entry + '\"]');
  if (!slider) return;
  slider.value = x.points;
  bwWeightLabel(slider);
  bwRemaining(slider.closest('[data-topic]'));
});
// The voter's complete stored allocation, e.g. after a change in another tab;
// a slider that is being moved right now keeps its local value.
Shiny.addCustomMessageHandler('bw_weights', function(x) {
  var stored = {};
  (x.entries || []).forEach(function(id, i) { stored[id] = x.points[i]; });
  var cards = [];
  document.querySelectorAll('.bw-weight').forEach(function(slider) {
    if (slider.dataset.moving) return;
    var value = stored[slider.dataset.entry] || 0;
    if (Number(slider.value) !== value) {
      slider.value = value;
      bwWeightLabel(slider);
    }
    var card = slider.closest('[data-topic]');
    if (cards.indexOf(card) < 0) cards.push(card);
  });
  cards.forEach(bwRemaining);
});
// A deleted session reloads into its notice; created sessions open directly.
Shiny.addCustomMessageHandler('bw_reload', function(x) { location.reload(); });
Shiny.addCustomMessageHandler('bw_navigate', function(url) { location.href = url; });
"
}
