## Submission

This is a new submission of brainwritR 0.7.0.

## Test environments

* Local: macOS (aarch64-apple-darwin), R 4.6.1, 14 November 2026:
  full `R CMD check --as-cran` on the built source tarball, including
  PDF manual compilation with TinyTeX (TeX Live 2026) and HTML manual
  validation. This supersedes the earlier local check environment.
* GitHub Actions (r-lib/actions v2, `--as-cran`):
  macos-latest (release), windows-latest (release),
  ubuntu-latest (devel, release, oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'R. Heller <raban.heller@uni-ulm.de>'
  New submission

  This is the first submission of the package.

## Notes for the CRAN team

* `run_app()` starts a 'shiny' application and is only run in examples when
  `interactive()` is `TRUE`. Unless a path is given, it stores its SQLite
  database in `tools::R_user_dir("brainwritR", "data")`; nothing is written
  when the package is loaded.
* Tests write only to temporary directories. Browser tests that need
  'shinytest2' and a Chrome installation are skipped on CRAN.
