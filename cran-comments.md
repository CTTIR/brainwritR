## Submission

This is a new submission of brainwritR 0.7.0.

## Test environments

* local: macOS (aarch64-apple-darwin), R 4.6.1, `R CMD check --as-cran
  --no-manual` on the built tarball (no LaTeX installation is available
  locally, so the PDF manual was not built; it should be checked on
  win-builder before submission).
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
