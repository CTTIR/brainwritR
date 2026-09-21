test_that("run_app resolves environment at invocation and arguments take precedence", {
  root <- withr::local_tempdir()
  env_path <- file.path(root, "environment", "session.sqlite")
  explicit_path <- file.path(root, "explicit", "session.sqlite")
  withr::local_envvar(c(
    DB_PATH = env_path, MOD_PIN = "environment-pin",
    BASE_URL = "https://class.example.org", PORT = "4387"
  ))
  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    runApp = function(app_dir, host, port, ...) {
      calls$app <- app_dir
      calls$host <- host
      calls$port <- port
      "stopped"
    },
    .package = "shiny"
  )
  result <- withVisible(run_app())
  expect_false(result$visible)
  expect_identical(result$value, "stopped")
  expect_s3_class(calls$app, "shiny.appobj")
  expect_identical(calls$port, 4387L)
  expect_identical(calls$host, "0.0.0.0")
  expect_true(file.exists(env_path))
  expect_identical(read_table(env_path, "session")$status, "setup")

  run_app(explicit_path, "explicit-pin", "http://127.0.0.1:4567",
          port = 4567, host = "127.0.0.1", poll_ms = 100)
  expect_true(file.exists(explicit_path))
  expect_identical(calls$port, 4567L)
  expect_identical(calls$host, "127.0.0.1")
  withr::local_envvar(c(DB_PATH = file.path(root, "later.sqlite"), PORT = "4568"))
  run_app()
  expect_true(file.exists(file.path(root, "later.sqlite")))
  expect_identical(calls$port, 4568L)
})

test_that("invalid runtime inputs fail before creating a database", {
  path <- file.path(withr::local_tempdir(), "uncreated", "session.sqlite")
  for (bad in list(NULL, character(), NA_character_, c("a", "b"), " ", 42)) {
    expect_error(app_config(path, bad, "https://example.org"), "non-empty")
  }
  for (bad in list(numeric(), c(3838, 3839), NA_real_, Inf, 0, 65536, 1.5)) {
    expect_error(app_config(path, "pin", "https://example.org", port = bad), "port")
  }
  for (bad in list(numeric(), c(1, 2), NA_real_, Inf, -1, 1.5, "2500")) {
    expect_error(app_config(path, "pin", "https://example.org", poll_ms = bad), "poll_ms")
  }
  expect_error(run_app(path, mod_pin = "", base_url = "https://example.org"), "non-empty")
  expect_false(dir.exists(dirname(path)))
})

test_that("rotation rejects undefined assignments and repeats after a complete cycle", {
  for (bad in list(numeric(), NA_real_, NaN, Inf, -1, 0, 1.5, "1", TRUE)) {
    expect_error(topic_for(bad, 1, 3), "grp")
    expect_error(topic_for(1, bad, 3), "round")
    expect_error(sheet_for(bad, 3), "idx")
    expect_error(sheet_for(1, bad), "n_sheets")
  }
  for (k in 2:6) {
    expect_equal(topic_for(seq_len(k), k + 1, k), seq_len(k))
    expect_equal(topic_for(seq_len(k), 2 * k, k), topic_for(seq_len(k), k, k))
    expect_error(topic_for(k + 1, 1, k), "grp")
  }
  expect_equal(sheet_for(1:8, c(2, 4)), c(1, 2, 1, 4, 1, 2, 1, 4))
})

test_that("failed transactions roll back every write and release the connection", {
  path <- new_db(withr::local_tempfile(), n = 3)
  before <- lapply(c("session", "topics", "participants"), function(x) read_table(path, x))
  expect_error(transaction_db(path, function(con) {
    DBI::dbExecute(con, "UPDATE session SET current_round = 9")
    DBI::dbExecute(con, "DELETE FROM topics")
    DBI::dbExecute(con, "DELETE FROM participants")
    stop("deliberate transaction failure")
  }), "deliberate transaction failure")
  after <- lapply(c("session", "topics", "participants"), function(x) read_table(path, x))
  expect_identical(after, before)
  expect_null(start_session(path))
  expect_identical(read_table(path, "session")$status, "running")
})

test_that("reinitializing storage preserves an existing session and submissions", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  pid <- read_table(path, "participants")$pid[1]
  save_entry(path, pid, 1, 1, 1, 1, "Persistent contribution", 1)
  tables <- c("session", "topics", "participants", "entries")
  before <- lapply(tables, function(x) read_table(path, x))
  init_db(path)
  init_db(path)
  expect_identical(lapply(tables, function(x) read_table(path, x)), before)
})

test_that("SQL metacharacters remain literal names and contribution text", {
  path <- new_db(withr::local_tempfile(), n = 3)
  name <- "O'Brien'); DROP TABLE participants; --"
  pid <- add_participant(path, name)
  start_session(path)
  content <- "Grüße; '); DELETE FROM entries; --\nZweite Zeile, mit Komma"
  save_entry(path, pid, 1, 1, 1, 1, content, 1)
  exported <- export_df(path)
  expect_identical(exported$Teilnehmer, name)
  expect_identical(exported$Beitrag, content)
  expect_equal(nrow(read_table(path, "participants")), 4)
  expect_equal(nrow(read_table(path, "entries")), 1)
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(exported, csv, row.names = FALSE, fileEncoding = "UTF-8")
  restored <- utils::read.csv(csv, fileEncoding = "UTF-8", check.names = FALSE)
  expect_identical(restored$Teilnehmer, name)
  expect_identical(restored$Beitrag, content)
  expect_true(grepl(content, build_md(path), fixed = TRUE))
})

test_that("empty sessions export usable headers without inventing contributions", {
  path <- new_db(withr::local_tempfile())
  expect_equal(nrow(export_df(path)), 0)
  md <- build_md(path)
  expect_match(md, "## Thema 1")
  expect_match(md, "## Thema 3")
  expect_false(grepl("### Bogen", md, fixed = TRUE))
  before <- read_table(path, "session")
  maybe_advance(path, force = TRUE)
  expect_identical(read_table(path, "session"), before)
})
