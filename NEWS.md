# brainwritR 0.6.0

- Configure any positive number of questions separately for each topic. Answer
  fields, drafts, submissions, YAML, prompt exclusion and exports follow that
  count; existing two-question settings and databases remain supported.
- Show the authored question above each answer in results and plenum views, and
  list the questions in the analysis tab.
- Apply the pseudonymization checkbox immediately to the result view as well as
  downloads, without changing stored participant identities.
- Fix weighted plots and PDF generation on ggplot2 3.5.x, where `facet_wrap()`
  does not accept `space`. Keep download handlers active and send explicit MIME
  types for PDF, XLSX, RDS, CSV, Markdown and YAML files.
- Add server and real-browser regressions for variable questions, immediate
  pseudonyms, weighted plots and downloaded binary file contents.

# brainwritR 0.5.0

* Add **Analyse & Plenum** to finished sessions: participants see all contributions
  anonymously and distribute 100 % per topic with sliders, their own contributions
  included. On a shared hot-seat device the weighting passes from person to person.
* The moderator starts, ends and reopens the weighting, follows progress per topic and
  then sees a ranked overview with a chart; results stay hidden while voting is open.
* Weights appear in a new CSV download, an extra XLSX sheet, the RDS snapshot, the
  Markdown protocol and a third PDF page. For sessions without weights, CSV, XLSX,
  Markdown and PDF are unchanged; the RDS snapshot always carries the `votes` table and
  the new `plenum` and `plenum_turn` session columns.
* Ask for the planned number of participants in setup and describe the resulting format
  live, for example "Format 16-2-5 … 5–6 people per group"; the lobby counts joins
  against it. Names in setup are marked optional; pseudonyms are welcome.
* Replace the footer line with a CTTIR link and an inline GitHub icon.
* Add a light/dark toggle next to the language control. It switches in place without
  touching drafts, is remembered per browser and follows the device setting by default.
* Migrate session files created by 0.4.0 at startup.

Follow-up to an external audit of this release:

* Ending a round only ends the round on screen: repeated clicks or a second moderator tab
  no longer skip a round in individual and group-device mode.
* Weighting sliders are named by their question and contribution and announce the
  remaining budget; their values are spoken as percentages.
* Charts adapt to phones: rank-numbered weighting chart sized to its bars, single-column
  terms, smaller network and wordcloud, horizontal continuity bars, larger text.
* One rule decides what counts as a contribution: whitespace-only text (tabs, line
  breaks, no-break, ideographic and zero-width characters included) is ignored by
  analytics, weighting, protocols and the previous-contribution view.
* Open voting pages of the same person follow weights stored from another tab or device.
* Moderator logins also expire in open tabs after 12 hours.
* Optional spreadsheet-safe CSV downloads that defuse formula-like cells, also after
  embedded separators and line breaks.
* Publish the website through GitHub Pages actions, matching the repository setting.
* Open SQLite files with `synchronous = NORMAL` (SQLite's durable setting for WAL) after
  the busy timeout, instead of RSQLite's default `OFF`, so recently saved contributions
  survive an operating-system crash or power loss.

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
