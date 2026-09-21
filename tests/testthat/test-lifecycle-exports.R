test_that("setup is validated and cannot overwrite an active session", {
  path <- withr::local_tempfile()
  init_db(path)
  topics <- rep(list(list(t = "Topic", a = "Question 1", b = "Question 2")), 3)
  expect_match(configure_session(path, 3, 0, 300, topics), "Bitte")
  expect_identical(read_table(path, "session")$status, "setup")
  expect_null(configure_session(path, 3, 3, 300, topics))
  before <- read_table(path, "topics")
  expect_match(configure_session(path, 3, 3, 300, topics), "bereits")
  expect_identical(read_table(path, "topics"), before)
})

test_that("exports retain all contributors and German column names", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  p <- read_table(path, "participants")$pid
  save_entry(path, p[1], 1, 1, 1, 1, "Grüße\nNeue Zeile", 1)
  save_entry(path, p[2], 1, 1, 1, 1, "Zweiter Beitrag")
  d <- export_df(path)
  expect_named(d, c(
    "Thema", "Bogen", "Runde", "Frage", "Teilnehmer",
    "Beitrag", "abgegeben", "Zeit"
  ))
  expect_equal(nrow(d), 2)
  expect_true("Grüße\nNeue Zeile" %in% d$Beitrag)
  expect_false(anyNA(d$Zeit))
  md <- build_md(path)
  expect_match(md, "## Thema 1")
  expect_match(md, "### Bogen 1")
  expect_match(md, "Zweiter Beitrag")
  expect_false(reset_session(path))
  for (i in 1:3) maybe_advance(path, TRUE)
  expect_true(reset_session(path))
  for (table in c("participants", "topics", "entries")) {
    expect_equal(nrow(read_table(path, table)), 0)
  }
  expect_identical(read_table(path, "session")$status, "setup")
  save_entry(path, p[1], 1, 1, 1, 1, "Delayed after reset")
  expect_equal(nrow(read_table(path, "entries")), 0)
})

test_that("configuration rejects invalid persistence and clocks", {
  expect_error(app_config(":memory:", "x", "https://example.org"), "persistent")
  expect_error(app_config("x", "", "https://example.org"), "non-empty")
  expect_error(app_config("x", "x", "bad"), "HTTP")
  expect_error(app_config("x", "x", "https://example.org", port = "oops"), "port")
  expect_error(app_config("x", "x", "https://example.org", poll_ms = 0), "poll_ms")
})
