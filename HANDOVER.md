# brainwritR implementation and validation handover

Checkpoint: 2026-09-21T13:55:47Z (UTC)

## Scope and naming

The package is **brainwritR 0.1.0**, hosted publicly at
<https://github.com/CTTIR/brainwritR>. The requested package name supersedes
`brainwriting635` in the original specification. Runtime environment variables,
SQLite filename default, German application title, and Traefik router `bw635`
retain their specified names for compatibility.

The preliminary ZIP remains untouched in `archive/`, which is Git-ignored and
excluded from source packages and Docker contexts. Its single-file application
was copied into ignored `dev/app.R` as the Mode A refactoring reference. The ZIP
did not contain a separate test suite; the specified headless cases and additional
behavioral tests were implemented in testthat.

## File map

| Location | Responsibility |
|:--|:--|
| `R/db.R` | Fresh SQLite connections, WAL, timeout, schema, transactions |
| `R/rotation.R` | Exported, validated topic/sheet arithmetic |
| `R/session-ops.R` | Setup, balanced start, joining, guarded clock, reset |
| `R/entries.R` | Submission-preserving upsert and CSV/Markdown data |
| `R/assets.R`, `R/ui.R` | Local styles, reconnect script, German accessible UI |
| `R/server.R` | Two polls, transition gates, role checks, views and controls |
| `R/run_app.R` | Invocation-time configuration and exported launcher |
| `tests/testthat/` | Core contracts, server lifecycle, browser and export tests |
| `README.Rmd`, `README.md` | Installation, method, screenshot, deployment and limits |
| `vignettes/classroom-guide.Rmd` | Facilitation, persistence, operations and privacy |
| `_pkgdown.yml`, `pkgdown/` | CTTIR `themakR` site and favicon assets |
| `man/figures/`, `inst/app/www/` | Named hex sticker, mobile screenshot, compact app icon |
| `docker/` | R 4.4.2 container and Traefik Compose configuration |
| `.github/workflows/` | Five-platform/version check matrix, lint, pkgdown deployment |

## Deliberate decisions and reference conflicts

- Preserved all table definitions, topic/sheet formulas, entry conflict key and
  submission-preserving upsert, plus the guarded round-advance SQL. A direct
  comparison verified identical normalized SQLite table definitions and rotation
  results across K = 2 through 6 and rounds 1 through 12.
- Corrected the reference's unsupported claim that every sheet gains a new group's
  contribution in every round. Under the prescribed modulo mapping, an arriving
  smaller group leaves some sheets untouched. Latecomers also cannot complete
  topics they missed before arrival. The formulas were not changed.
- Preserved transition-gated main views and separate live subregions. Captured
  edit context with each input before debounce to prevent a delayed old-round
  edit from being assigned to the next round.
- Added server-side authorization to moderator handlers, outputs and downloads;
  the original reference only gated the visible route. Reset requires a displayed
  confirmation and a finished session.
- Added transactional setup, start, joining and reset. Repeated starts or setup
  events cannot reshuffle or overwrite an already active session.
- Fixed stale local-storage IDs and a join race where a cached pre-join poll
  could incorrectly clear a newly created identity. Repeated join clicks do not
  insert duplicate participants. Delayed writes after reset cannot recreate
  orphaned entries.
- The finished participant route does not offer joining. CSV capitalization now
  follows the specified German column contract. Zoom, input labels, touch sizes,
  spacing, wrapping and supplied branding improve the retained slate/petrol design.
- The timer is driven by connected-session polls, not an independent scheduler.
  Moderator disconnects are tolerated. When everyone disconnects, an expired
  round advances on reconnection; missed rounds are not replayed.
- Exactly one R process/session/database per instance remains a deliberate limit.
  Last-second/offline keystrokes can be lost. Multiple tabs do not merge edits.

## Local validation

- `devtools::check(args = "--as-cran")`: **0 errors, 0 warnings, 0 notes**.
- Full testthat suite with actual Chromium: **145 passed, 0 failures, 0 warnings,
  0 skips**. Includes phone-width layout, stable textarea nodes, submit/edit,
  disconnect reload/resume, stale-ID cleanup, moderator PIN rejection, clock
  advancement without a moderator, prior contributions, escaped HTML, downloads
  and confirmation-gated reset.
- Headless coverage (browser subprocesses excluded): **93.13% overall**;
  DB, entries, rotation, assets and UI 100%; session operations 99.13%; server
  90.81%; launcher 63.64%. The actual launcher was additionally smoke-tested as
  a subprocess, including zero-config database creation.
- `lintr::lint_package()`: no lints. Workflow YAML parses.
- README renders; CTTIR-themed pkgdown reference, article and site build. Mobile
  screenshot visually inspected at 390 px width.
- Docker builds on the specified `rocker/r-ver:4.4.2`, runs as UID 10001, returns
  HTTP 200 and initializes SQLite. A participant persisted across container
  restart with a named volume. Compose validates; test containers/volume removed.
- Archive/dev/runtime data excluded from the source tarball and Docker context.
- All four supplied artwork originals copied unchanged to
  `/data/GitHub/CTTIR/public/rh_hex_stickers`; SHA-256 matches verified.
- Local Chromium requires an explicit `--no-sandbox` argument on this workstation
  because user-namespace sandboxing is disabled. This setting was confined to
  test orchestration; it is not embedded in application code.

## Hosted validation and remaining actions

Implementation commit: `19e921d35fddc0ff0a771820ef81ca652665adf4`.
Linux release (R 4.6.1), Linux old-release (R 4.5.3), macOS release and Windows
release checks each report Status: OK and 145 passed / 0 failed / 0 warnings /
0 skips. Lint, pkgdown and Pages deployment passed. R-devel is still compiling
dependencies in run 35607880529; its final result remains to be checked.

The website is live at <https://cttir.github.io/brainwritR/>. Reference, vignette,
logo and mobile screenshot URLs were verified. The direct `R CMD check --as-cran`,
including the PDF manual, reports 0 errors / 0 warnings and only the expected
incoming NOTE "New submission". No CRAN submission has been made.

Source tarball SHA-256:
`3d607e79609e6bf1d3b709cd107ef9d0c9668538822c17f67719e639e20beb69`.

CI uses runner Chrome and explicit Linux libraries (including libuv for fs source
builds), avoiding a broken Chromium PPA. Browser tests await asynchronous writes
without weakening assertions and own/clean their Chrome processes and temporary
files. The final full local suite also passes all 145 assertions.

For a real classroom deployment, supply the hostname, private moderator PIN,
Traefik certificate resolver, writable persistent data volume and operational
retention policy. Rehearse with the institution's phones/network before use.

Local validation logs and the reference comparison/capture scripts are retained
under ignored `dev/`, with logs in `dev/validation/`.
