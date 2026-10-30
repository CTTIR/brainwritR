# Maintenance audit — 2026-10-30

Baseline: `840840d3ec033d220794cd4189757972ac0580ec`, development release 0.6.0.
The original clean checkout is preserved; implementation uses an isolated branch.

## Purpose and release criteria

Run dependable small classroom brainwriting sessions from phones or shared
devices, preserve drafts and identities across reconnects, keep a consistent
database clock, and deliver understandable descriptive exports. Preserve all
three device modes, multilingual UI, vote budgets and existing database content.
Existing limits remain explicit: one R process per local data folder, collaborative
access, last-write-wins on duplicate devices, and no inference about idea quality.

Acceptance: local startup binds only to loopback; network deployment is explicit
and refuses the known demonstration PIN. Invitation URLs must reach the intended
session. Failed archive attempts close database connections without losing source
data. Published source archives contain no local Git metadata. Unit/server tests,
real browser journeys, clean installation, package/lint/docs checks, exports and
backup/restore are exercised with actual outcomes recorded.

## Findings and strategy

| Priority | Finding and evidence | Milestone and validation |
| --- | --- | --- |
| Essential | `run_app()` defaults to `0.0.0.0` and PIN `635`, despite describing this as a local demonstration. All reachable network interfaces receive the same moderator access. | Default to loopback. Require a private PIN for an explicit network bind or public URL; update Docker and classroom examples together. Verify configuration rejection before storage creation and actual startup. |
| Essential | `app_config()` accepts fragments, missing hosts and reserved route queries. `https://example.org/#section` becomes `https://example.org/#section?s=abc234`; a pre-existing `s` or `mod` parameter misroutes invitations. | Validate base URL authority and reserve session/moderator routing parameters while retaining harmless query strings. Test rejected and valid URLs. |
| Essential | A baseline `R CMD build --no-build-vignettes` includes `brainwritR/.git`, an absolute worktree pointer. | Exclude Git metadata and the maintenance record; inspect the final source tarball. |
| Everyday reliability | `archive_standard()` has explicit disconnects but no error cleanup around VACUUM and post-copy work. Failure can leave connections open until garbage collection. | Reproduce a failed copy, add scoped connection cleanup, and assert source preservation plus disconnected handles. |
| Qualification | Hosted checks at the baseline pass; seven dependencies are initially absent locally. Docker CLI is unavailable. | Install dependencies in an isolated library, reproduce the baseline and final suites, validate exports and stopped-app restoration. Keep Docker execution and new hosted runs explicitly unverified. |
| Deferred | Horizontal scaling, confidential-survey semantics, new statistical measures and major UI redesign are outside the stated classroom purpose. | Preserve the coherent current product and document measured operational limits. |

Changing the listening default is intentional: existing network launchers must
pass `host = "0.0.0.0"` and set a private PIN. Docker will do the former explicitly.
This follows [Shiny's documented local listening default](https://shiny.posit.co/r/reference/shiny/latest/runapp.html).
URL fragments are client-side identifiers, not server routing parameters
([RFC 3986](https://www.rfc-editor.org/rfc/rfc3986#section-3.5)).
Backups follow the existing stopped-app/full-folder procedure and
[SQLite's documented snapshot constraints](https://sqlite.org/backup.html).

## Implemented changes

Local startup now binds to `127.0.0.1`. The demonstration PIN is refused for a
non-loopback host or invitation URL, before creating storage. Network classroom
examples and the Docker command explicitly select `0.0.0.0`. URL validation
rejects credentials, fragments, absent hosts, invalid ports and reserved routing
keys, including percent-encoded keys, while retaining other query parameters.
No database schema or existing course data was changed.

Both standard-archive connections now have error cleanup. Regression tests keep
their handles alive deliberately and exercise failure during VACUUM and after
the copy, asserting disconnected handles, unchanged original tables and no
new catalog entry. The baseline VACUUM probe left one connection open.

Source packages exclude the worktree `.git` pointer. README, function help,
classroom guide, Docker examples and news describe the same launch contract.

The baseline suite had 185 tests, 1,541 passing assertions and one browser error.
The dark-mode test changed the emulated preference after initial page load; it
now reloads and waits for a confirmed light starting state before clicking.
The application already applied the device preference correctly on load.
The apparent browser package-loader inconsistency was investigated: shinytest2
intercepts `library()` during local tests to load this checkout, so no loader
change was needed.

## Verification evidence

Evidence is retained outside the package in
`../../evidence/brainwritR/` relative to this isolated checkout. Runtime: macOS
27.0.1 arm64, R 4.6.1, Shiny 1.14.0, RSQLite 3.53.3, bslib 0.12.0, ggplot2
4.0.3, Chromote/shinytest2 0.5.1 and isolated Chromium 156.0.8078.4. Dependencies
were installed in a dedicated audit library; package installation used a fresh
package library with those dependencies available.

- Final local suite with `NOT_CRAN=true` and `CHROMOTE_CHROME` set to the isolated
  Chromium: 188 tests, 1,594 passing assertions, zero failures/errors/warnings or
  skips. All 19 browser tests were exercised. The package check separately skips
  those browser tests under its normal CRAN convention.
- Full `R CMD check --as-cran`, including vignettes, PDF/HTML manual and examples:
  zero errors, zero warnings and one NOTE for a new submission. The final source
  archive has 107 code/test/help files byte-identical to the checkout and matching
  DESCRIPTION fields (apart from normal build metadata). It installs successfully.
- `lintr::lint_package()` with the repository configuration: zero lints.
  `actionlint -shellcheck= -pyflakes=` and `git diff --check` pass; optional
  shellcheck/pyflakes executables are absent and were not claimed as run.
- pkgdown: 15 HTML pages, 411 local references, zero missing local targets.
  README rendering reports upstream Pandoc math-option deprecation and inability
  to fetch remote badge images; generated Markdown retains the remote links.
- Fresh installed-package startup returned HTTP 200 and `lsof` confirmed only
  the intended loopback listening socket. The owned app process was stopped.
- A stopped-app copy and restore preserved the standard session plus two courses,
  all session/topic/participant/entry/vote tables and the catalog archive state.
- RDS round trip was identical, the XLSX had five worksheets and preserved
  formula-like input as text, and the three-page PDF was rendered and visually
  inspected. Author pseudonyms do not redact names embedded in contribution text;
  the documented distinction was checked explicitly.
- Mobile group selection and hot-seat answer screenshots were inspected. The
  full browser suite also exercises reconnects, retained drafts, separate session
  identities, PIN guards, language changes, vote budgets and binary downloads.

Chromote reports `Browser.close` promise timeouts and WebSocket closing messages
with this Chromium. A separate probe without brainwritR reproduces both while
confirming the owned browser process is already stopped before and after the
promise deadline. These diagnostics remain in the logs; they were not suppressed
or counted as application assertions.

## First use and remaining gates

Prerequisites: R >= 4.4 and the DESCRIPTION dependencies. Source compilation may
need the system libraries listed in `docker/Dockerfile`; PDF reports require an
R build with Cairo support. From this checkout:

```sh
Rscript -e 'install.packages("remotes", repos="https://cloud.r-project.org")'
Rscript -e 'remotes::install_deps(dependencies = TRUE)'
R CMD INSTALL .
Rscript -e 'brainwritR::run_app()'
```

Open `http://localhost:3838` and use `http://localhost:3838/?mod=1` with PIN `635`
for the local demonstration. For network use follow the README's explicit host,
private PIN and reachable URL example. Stop the app and copy the entire data
folder, including `sessions/` and any SQLite sidecar files, before upgrades.

No GitHub releases or open issues were returned by the authenticated read.
The baseline hosted matrix passed; new hosted runs are not observed. Docker
execution is unverified because the CLI is unavailable. Real classroom network,
TLS/reverse-proxy operation and simultaneous-device load beyond the browser
fixtures remain deployment qualification tasks. The project remains a development
release, not a CRAN-accepted package. No push, deployment or submission occurred.
Next release action: review the local commit, authorize publication, then run the
hosted platform matrix and container/TLS smoke test before distributing it.
