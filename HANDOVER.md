# brainwritR implementation and validation handover

Checkpoint: 2026-09-22T07:30:21Z (UTC)

## Current change: 0.4.0 multi-session administration

Implementation commit `744be57` on `main`, built on `f2929de`. Hosted CI results
are recorded in CURRENT_STATE.md once available.

- **Storage (approved option A).** `DB_PATH` stays the standard session behind the
  bare URL and gains a lazily created `catalog` table (code, label, created_at,
  archived_at, prefill_yaml). Every further session is `sessions/<code>.sqlite`
  next to it, created by the unchanged `init_db()`. This keeps the session schema
  and the frozen iteration-1 tests untouched (singleton `CHECK`, exact `entries`
  columns). Existing databases need no migration.
- **Routing.** Each page binds to one session at connect (`resolve_route()`):
  `?s=<code>` participants, `?mod=1[&s=<code>]` moderators, `?mod=1&view=sessions`
  overview. Unknown codes show a notice; archived sessions refuse participants.
  Codes: six characters from `23456789abcdefghjkmnpqrstuvwxyz`.
- **Overview actions.** New, open, QR (modal + 1200 px PNG), restart, archive/restore
  (finished only), delete. Restart = new session in setup, prefilled for review;
  the source stays untouched (user decision). Archive = read-only, reversible (user
  decision). The standard session is never deleted or archived in place: delete
  means forced reset; archive copies it with `VACUUM INTO`, drops the copied catalog
  and then resets it. The in-session reset is relabelled **Zurücksetzen …**.
- **Deletion safety.** `db()` opens existing files only (`SQLITE_RW`); only
  `init_db()` creates. Open pages of a deleted session receive `bw_reload` from
  their polls and show the notice; draft and submit writes are guarded.
- **Moderator login.** Tokens (12 h, server memory only) stored in the tab's
  `sessionStorage` avoid re-entering the PIN when navigating. Global throttle:
  from the fifth consecutive failure, 15 s doubling to 300 s; failures older than
  15 min are forgotten; locked attempts are not compared. Signed-in tabs continue.
- **Participant identity** is stored per session (`bw_pid` for the standard session,
  `bw_pid:<code>` otherwise), so one device can join several sessions.
- **Randomness fix.** `ggwordcloud` called `set.seed(635)` on every draw, making all
  later `sample()` IDs and group shuffles predictable and letting two joins after
  the same reseed collide on the participant ID (UNIQUE failure). IDs, codes and
  tokens now use `openssl::rand_bytes()` (unbiased rejection sampling); wordcloud
  drawing in app and report runs under `withr::with_preserve_seed()`.
- **6-3-5 templates (user decision: reword only).** Each example topic is one open
  "How might we …?" problem; field 1 asks for up to three new ideas, field 2 to
  develop an idea from above (or one's own). Same in `settings-example.yml`.

New files: `R/ids.R`, `R/catalog.R`, `R/auth.R`, `R/pages.R`, `R/dashboard.R`;
tests `helper-sessions.R`, `test-ids.R`, `test-catalog.R`, `test-auth.R`,
`test-sessions-server.R`, `test-dashboard.R`, `test-sessions-browser.R`.
Non-frozen tests updated for the new templates: `test-templates.R`,
`test-settings.R`, `test-ui-language.R`, `test-i18n.R`.

Validation of 0.4.0 (local, R 4.6.1, Linux):

- Complete test run with browser tests: **1167 passing assertions**, 0 failures,
  0 warnings, 0 skips. The five iteration-1 files are byte-identical to `19e921d`.
- `devtools::check(args = "--as-cran")` with browser tests: **0 errors, 0 warnings,
  0 notes**. `lintr::lint_package()`: 0 lints. `R/` remains ASCII-only.
- Browser checks: per-session identities, token navigation without PIN, deletion
  notice; QR modal renders a 324×320 plot. Screenshots `setup.png`, `sessions.png`
  (new) and `analytics.png` recaptured and inspected.
- Container build `brainwritr:0.4.0` (`sha256:2221b6017a75…`, 620,507,532 bytes, +3.3 MB
  for openssl): HTTP 200, UID 10001, catalog and `sessions/` file persisted across
  restart in a named volume; coded address served. Test container/volume removed.

## Published package

- `brainwritR` 0.3.0, public repository <https://github.com/CTTIR/brainwritR>, MIT.
- Website: <https://cttir.github.io/brainwritR/>; CTTIR `themakR` theme.
- Default branch: `main`. Implementation milestones: `11116d2`, `a967aba`, `8feca97`;
  selector-content protection and lint-context correction: `7a64fb1`;
  delayed setup acknowledgement gate and platform checks: `b7f275d`;
  dynamic-input browser synchronization: `b1148e6`.
- The original archive remains untouched and Git-ignored. The single-file Mode A
  reference remains local at `dev/app.R`, excluded from package builds.
- Supplied hex artwork is integrated in the README, website, app header and favicon;
  original copies remain in `/data/GitHub/CTTIR/public/rh_hex_stickers`.

## File map

| Files | Responsibility |
|:--|:--|
| `R/db.R`, `R/session-ops.R` | SQLite, migration, atomic setup, shared rotation scheduler and identity claims |
| `R/catalog.R`, `R/dashboard.R`, `R/pages.R` | Session catalog and files, routing, overview, notices, moderator bar |
| `R/auth.R`, `R/ids.R` | Moderator tokens and PIN throttle; secure identifiers |
| `R/rotation.R`, `R/entries.R` | Exported pure rotation helpers and unchanged entry upsert semantics |
| `R/settings.R`, `R/templates.R` | Safe YAML validation/roundtrip and multilingual example question sets |
| `R/server.R`, `R/ui.R`, `R/assets.R`, `R/i18n.R` | Mobile Shiny lifecycle, gated textareas, language slider and controls |
| `R/analytics.R`, `R/plots.R` | Pure German text analysis and five shared ggplot figures |
| `R/exports.R`, `R/report.R` | Consistent snapshots, pseudonymization, XLSX/RDS and two-page Cairo PDF |
| `inst/extdata/settings-example.yml` | Valid three-topic reusable settings example |
| `tests/testthat/` | Original unmodified regression suite plus mode, settings, reporting, UI and privacy tests |
| `README.Rmd`, `README.md`, `vignettes/classroom-guide.Rmd` | Comprehensive guide, English gallery and entirely English vignette |
| `man/figures/` | Supplied logos and five real English browser screenshots |
| `docker/`, `.github/workflows/` | Single-process deployment, five-platform/version check matrix, lint and website |

## Mode and persistence decisions

`individual` remains the default and retains the existing run_app signature,
environment variable names and database path. It gains optional roster buttons.
`group_device` treats each group as one author with a stable canonical ID and one
sheet per topic. Taking over a claimed group requires confirmation. Old and new
devices can still overwrite each other's edits; last-write-wins is documented.
`hot_seat` uses the same topic/sheet formulas, sequential arrival-order turns,
explicit handovers, an armed per-person timer and PIN-gated moderator controls.

Two specification conflicts were resolved with explicit user approval:

1. A hot-seat session can start with one person even when K >= 2. Empty virtual
   starting groups receive one empty sheet, keeping modulo assignment valid.
2. SQLite remains authoritative for custom group names, prepared rosters and turn
   duration. Migration adds `settings_yaml` as well as `mode` and `current_turn`.
   Existing sessions default to individual mode; other tables are unchanged.

Settings uploads validate all fields, reject files over 100 KiB, disable YAML
expression evaluation, warn on unknown keys, and prefill without starting.
The compatibility format identifier remains `brainwriting635-settings/1` despite
the corrected package name. Runtime secrets never enter the settings file.
The opt-in **Use example questions** checkbox fills three editable topics in the
current UI language;
clearing it restores prior topics. Mode, timing and roster are preserved.

The initial v1 UI's minimum of 60 seconds was widened to 30 seconds as required
by the settings specification. Earlier reference corrections remain intentional:
server-side moderator authorization, atomic lifecycle writes, stale-ID handling,
correct capture of autosave context, and rejection of orphan writes after reset.
The claim that every sheet is filled every round is mathematically impossible
for unequal groups under the required modulo mapping and is not made in the docs.

## Languages and usability

The DE/EN/FR slider changes interface text in place and remembers the language in
browser storage. German remains the default. It preserves the textarea DOM node,
value and cursor selection. Authored questions, names, answers and selector labels
are protected from translation. In-app chart labels follow the selected language.

The vignette and screenshot gallery are English. The setup screenshot shows the
example question set enabled, with multiline question fields for mobile review.
Export column/sheet labels and the PDF remain German, preserving the specified
export contracts. The text-analysis method remains German Snowball tokenization;
interface language does not silently change the analysis method.

## Figure and report inventory

- Contributions by topic and round: grouped horizontal petrol bars; distinct topic
  IDs prevent duplicate titles from collapsing categories.
- Top terms: eight per topic, faceted, with prompt-vocabulary exclusion by default.
- Co-occurrence network: within-entry pairs, minimum two entries, maximum 40 terms,
  seeded Fruchterman-Reingold layout, frequencies as node size, co-occurrence as width.
- Wordcloud: maximum 60 terms, fixed seed, size budget adjusted to frequency and word
  length so dense report panels render without dropped-word warnings.
- Lexical continuity: mean Jaccard overlap between adjacent rounds per sheet/question,
  pooled across authors sharing a sheet. Missing/empty comparisons are NA.

All figures return styled placeholders when their required data are unavailable.
They are shared between app and report rather than implemented twice. The report
always contains two A4 portrait pages, including empty sessions, composed directly
with Cairo/patchwork. There is no runtime LaTeX/Pandoc report dependency.

KPIs count nonempty saved entries, average unfiltered word count, submitted share
of nonempty entries, and distinct filtered terms. These are descriptive measures,
not attendance, idea quality, or evidence of conceptual development.

## Exports and privacy

CSV/Markdown retain their default output contracts. RDS stores the four raw tables,
settings, generation time and version; XLSX has four styled German worksheets.
Optional pseudonymization applies stable arrival-order codes to every download,
including persistent IDs and stored roster echoes. In-app views keep names.
Authored free text is not automatically redacted; pseudonymization is not complete
anonymization. Snapshots are internally consistent, and timestamps match SQLite's
local-time conversion, including fractional-second rounding boundaries.

## Validation evidence

- Original iteration-1 test files remain byte-for-byte unchanged from `19e921d`.
- Complete installed-package test run: **948 passing assertions**, no failures,
  warnings or skips, including browser tests.
- Standard installed-package check with browsers enabled: **0 errors, 0 warnings,
  0 notes** (`/tmp/bw-installed-final2.log`).
- Direct `R CMD check --as-cran`, including PDF manual: 0 errors, 0 warnings;
  only the expected **New submission** NOTE (`/tmp/bw-cran-final2.log`). The
  845 non-browser assertions pass; nine browser tests skip intentionally on CRAN.
  This is not a CRAN submission.
- Headless test coverage: **96.75% overall**, **92.86% server**; analytics, exports,
  templates, rotation, launcher, UI and several other modules are fully covered.
- Lint uses unchanged default linters and 100-character limit; package/test helpers
  are loaded first so cross-file test helpers resolve correctly.
- Hosted Linux release/oldrel/devel, Windows and macOS jobs all pass, with
  948 assertions per job. Release platforms report zero test warnings. R-devel
  emits 13 upstream `qrcode` warnings from `qr_mode.R:45`, where `structure()`
  uses the deprecated `.Names` argument. Its package check still reports
  **Status: OK**. No warnings or assertions were suppressed; monitor an upstream
  qrcode update before the next R release.
- Hosted lint and pkgdown deployment pass on `b1148e6`.
- README and pkgdown build successfully. Five actual English screenshots and both
  PDF pages were visually inspected; mobile layouts were exercised at 390 px.
- Browser tests verify joins, reconnect/resume, input stability, all modes, YAML
  topic-count changes, group takeover, multilingual cursor preservation, and the
  reversible template. One initial combined run had a transient immediate-reload
  read failure; isolated and complete reruns passed without changing assertions.
  Chromote can print asynchronous Browser.close shutdown diagnostics; the final
  installed-package suite still reports zero test warnings or failures.
- Hosted browser checks exposed delayed topic-count acknowledgements overwriting
  immediate setup edits. A revision/count gate now absorbs acknowledgements without
  replacing edited fields; a deterministic regression covers this sequence.
- macOS CI installs XQuartz so R can load its Cairo graphics libraries.
- Race audit found rapid repeated hot-seat actions could skip the next author;
  expected round/turn guards now reject stale actions, with regression tests.
- Container builds on `rocker/r-ver:4.4.2`; Cairo is available. All three modes
  generated valid workbooks and exactly two-page A4 PDFs in the container.
- Container HTTP startup returned 200, ran as UID 10001, and retained a test record
  across restart with a named volume. Temporary test container/volume were removed.
- Final image size: 617,198,403 bytes (~589 MiB), versus
  446,137,133 bytes for 0.1.0; about 163 MiB additional for analytics/report libraries.

Validation source: `b1148e6`. The final source archive SHA-256 is
`5662a89953c68dae0de117a978a5e9a2028358c511c11cffb0d6728d84c22b12`.
The tested Docker image ID is
`sha256:2737cd4fa309e9ad07012c2804dc86dfc1aedef64bac62f7bf92b849508d2e04`.

Detailed local logs, screenshots and coverage objects are in ignored
`dev/validation/` and `/tmp/bw-*.log`. The public package does not include temporary
databases, participant records, development scratch files or archived drafts.

## Remaining operational boundaries

Several sessions, but exactly one R process per data folder; no replicas or horizontal
scaling. Moderator logins are lost on restart. Timers are driven by connected clients, so an empty room resumes on the
next connection. Late boundary keystrokes can be overtaken by a round/turn change.
Use a private moderator PIN, a reachable HTTPS URL and writable persistent storage.
Classroom network/device rehearsal and retention policy remain operator tasks.
No live classroom deployment or CRAN submission has been performed.

Next steps: rehearse with the intended classroom devices and prepare a
maintainer-controlled CRAN submission if wanted. Current CI run links/status are
recorded in CURRENT_STATE.md.
