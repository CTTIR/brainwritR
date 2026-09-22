#' Local application styles
#' @keywords internal
#' @noRd
app_css <- function() {
  "
:root { --ink:#22303c; --mut:#5b6b7a; --bg:#eceff2; --card:#ffffff;
        --accent:#0e6e78; --accent-soft:#eef6f6; --warn:#b3372b; }
body { font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
       background: var(--bg); color: var(--ink); }
.bw-wrap { max-width: 720px; margin: 0 auto; padding: 20px 16px 40px; }
.bw-card { background: var(--card); border: 1px solid #dce3e7; border-radius: 14px; padding: 20px;
           margin-bottom: 12px; box-shadow: 0 1px 2px rgba(34,48,60,.10); }
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
.bw-modbar { display: flex; flex-wrap: wrap; gap: 8px 12px; align-items: center;
             justify-content: space-between; padding: 12px 16px; }
.bw-modbar-name { overflow-wrap: anywhere; }
.btn { min-height: 44px; border-radius: 8px; font-weight: 600; }
a.btn { display: inline-flex; align-items: center; justify-content: center; }
.form-control { min-height: 44px; border-color: #9cabb7; }
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
// A deleted session reloads into its notice; created sessions open directly.
Shiny.addCustomMessageHandler('bw_reload', function(x) { location.reload(); });
Shiny.addCustomMessageHandler('bw_navigate', function(url) { location.href = url; });
"
}
