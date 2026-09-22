# brainwritR 0.4.0

* Run several sessions side by side, each with its own address (`?s=<code>`) and QR code.
  The bare address keeps serving the standard session; existing databases need no migration.
* Add a moderator session overview to create, open, restart, archive, restore and delete
  sessions, with large downloadable QR codes.
* Restarting a session offers its settings for review in a new session and keeps the original
  results. Archived sessions refuse participants and stay available for review and export.
* Keep one participant identity per session on each device.
* Keep moderators signed in per browser tab and pause new logins after repeated wrong PINs.
* Rewrite the example question sets after the 6-3-5 principle: one open problem per topic,
  up to three new ideas, then develop an idea already on the sheet.
* Draw participant IDs from the operating system's secure generator. Drawing a wordcloud no
  longer resets R's random seed, which had made later IDs and group shuffles predictable
  and could make a join fail on a duplicate ID.
* Never recreate a deleted session file from a lingering page; open pages reload to a notice.

# brainwritR 0.3.0

* Add an in-place German/English/French language slider without interrupting drafts.
* Offer reversible, editable example questions in the selected interface language.
* Provide an English screenshot gallery and an entirely English classroom vignette.

* Add a multilingual descriptive analytics view with five shared ggplot figures.
* Count Unicode German terms with transparent stopword and prompt-vocabulary filters.
* Summarize adjacent-round lexical overlap without treating it as an idea-quality score.
* Compose deterministic two-page A4 reports directly with Cairo and patchwork.
* Add lossless RDS and styled four-sheet XLSX exports alongside CSV and Markdown.
* Apply optional stable author pseudonyms consistently across all five downloads.

# brainwritR 0.2.0

* Preserve the default individual-device workflow and add optional roster buttons.
* Add canonical group-device identities and sequential hot-seat handovers.
* Prepare sessions using the setup form or validated, safely parsed YAML files.
* Migrate existing databases in place, retaining sessions and contributions.
* Persist settings in SQLite and support sparse hot-seat groups with an empty sheet.
* Extend lifecycle, browser, migration and configuration regression coverage.

# brainwritR 0.1.0

* Initial classroom brainwriting application with German participant and moderator views.
* SQLite persistence, frozen sheet counts, balanced groups and late joining.
* Server-authoritative rounds, reconnect/resume and debounced draft saving.
* Server-side moderator authorization and transactional lifecycle operations.
* CSV and Markdown export, container deployment and classroom guide.
