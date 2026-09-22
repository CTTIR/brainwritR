# brainwritR current state

Checkpoint: 2026-09-22T07:30:21Z (UTC)

- **brainwritR 0.4.0** on `main`, implementation commit `744be57` (based on `f2929de`).
  Hosted CI: pending.
- 0.4.0 adds multi-session administration: the bare URL keeps the standard session;
  further sessions live in `sessions/<code>.sqlite` with their own `?s=<code>` address
  and QR code. The moderator overview (`?mod=1&view=sessions`) creates, opens,
  restarts (new prefilled session), archives/restores (finished only, read-only) and
  deletes sessions. Moderator tokens per tab; PIN throttle after five failures.
- Example question sets follow the 6-3-5 principle within the two answer fields.
- Fixed: wordcloud drawing reset R's seed (predictable, colliding participant IDs);
  identifiers now come from `openssl::rand_bytes()`.
- Local validation (R 4.6.1): **1167 passing assertions**, 0 failures/warnings/skips,
  browser tests included; `devtools::check(--as-cran)` **0 errors / 0 warnings / 0 notes**;
  lint clean. Iteration-1 test files unchanged from `19e921d`.
- Container (`rocker/r-ver:4.4.2`, image `sha256:2221b601…`, 620,507,532 bytes):
  HTTP 200, UID 10001, a created session file persisted in the volume across restart
  and is served at `?s=<code>`; Cairo and openssl 2.3.2 available. Test container and
  volume removed.
- Details, decisions and file map: [HANDOVER.md](HANDOVER.md).
- Next steps: confirm hosted CI and website, rehearse with classroom devices.
  One R process per data folder remains required.
