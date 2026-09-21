# brainwritR current state

Checkpoint: 2026-09-21T15:08:53Z (UTC)

- Public MIT package **brainwritR 0.3.0**, published on `main` at
  <https://github.com/CTTIR/brainwritR>. Validated implementation: `b1148e6`.
- Three device modes, persisted YAML settings, descriptive analytics, two-page PDF,
  five export formats and consistent author pseudonymization are implemented.
- German/English/French language slider preserves drafts/cursors; English vignette
  and five real English gallery screenshots. The **Use example questions** checkbox
  supplies three editable topics and restores previous questions when cleared.
- Approved extensions: `settings_yaml` column and one sheet for empty hot-seat groups.
  Original iteration-1 test files remain unchanged from `19e921d`.
- Installed-package suite: **948 passes**, no failures, warnings or skips.
  Browser-enabled R CMD check: **0 errors / 0 warnings / 0 notes**.
- Final CRAN-style check with PDF/HTML manuals: **0 errors / 0 warnings**;
  expected **New submission** NOTE only. No CRAN submission performed.
- Coverage: **96.75% overall**, **92.86% server**. Lint and documentation builds pass.
- Docker R 4.4.2: build, HTTP 200, nonroot UID 10001, persistent-volume restart,
  and all-mode two-page Cairo reports/workbooks verified. Image size: 617,198,403 bytes.
- Hosted check matrix: **all five jobs pass**, each with 948 assertions. R-devel
  reports 13 upstream `qrcode` deprecation warnings about `.Names`; R CMD check
  itself reports Status: OK. Other platforms have zero test warnings.
  <https://github.com/CTTIR/brainwritR/actions/runs/35614628239>
- Hosted lint and website build/deployment passed:
  <https://github.com/CTTIR/brainwritR/actions/runs/35614628233>
  <https://github.com/CTTIR/brainwritR/actions/runs/35614628434>
- Website: <https://cttir.github.io/brainwritR/>.
- Detailed decisions, file map, limits, archive hash and validation receipts:
  [HANDOVER.md](HANDOVER.md). Local logs are preserved under ignored `dev/validation/`.
- Next operational step: classroom device rehearsal; maintainer-controlled CRAN
  submission only if desired. One R process and one session per instance remain required.
