test_that("rotation has both permutation invariants", {
  for (k in c(2, 3, 4, 6)) {
    m <- outer(seq_len(k), seq_len(k), function(g, r) topic_for(g, r, k))
    for (i in seq_len(k)) {
      expect_equal(sort(m[i, ]), seq_len(k))
      expect_equal(sort(m[, i]), seq_len(k))
    }
  }
  expect_equal(sheet_for(1:7, 5), c(1:5, 1, 2))
  expect_error(topic_for(0, 1, 3), "grp")
  expect_error(sheet_for(1, 0), "n_sheets")
})

test_that("16 participants start with frozen balanced sheets", {
  path <- new_db(withr::local_tempfile(), n = 16)
  expect_null(start_session(path))
  p <- read_table(path, "participants")
  expect_equal(as.integer(table(p$grp)), c(6L, 5L, 5L))
  expect_equal(sum(read_table(path, "topics")$n_sheets), 16)
  s <- read_table(path, "session")
  expect_identical(s$status, "running")
  expect_equal(s$current_round, 1)
  expect_gt(s$round_ends_at, now())
})

test_that("upsert retains submitted state and independent contributors", {
  path <- new_db(withr::local_tempfile(), n = 16)
  start_session(path)
  pid <- read_table(path, "participants")$pid[1]
  save_entry(path, pid, 1, 1, 1, 1, "draft")
  save_entry(path, pid, 1, 1, 1, 1, "edit")
  save_entry(path, pid, 1, 1, 1, 1, "submitted", 1L)
  save_entry(path, pid, 1, 1, 1, 1, "latest")
  e <- read_table(path, "entries")
  expect_equal(nrow(e), 1)
  expect_identical(e$text, "latest")
  expect_identical(e$submitted, 1L)
  pid2 <- read_table(path, "participants")$pid[2]
  save_entry(path, pid2, 1, 1, 1, 1, "another")
  expect_equal(nrow(read_table(path, "entries")), 2)
})

test_that("round advance obeys expiry, force, and idempotence", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  before <- read_table(path, "session")
  maybe_advance(path)
  expect_identical(read_table(path, "session"), before)
  for (r in 2:3) {
    maybe_advance(path, force = TRUE)
    expect_equal(read_table(path, "session")$current_round, r)
  }
  maybe_advance(path, force = TRUE)
  expect_identical(read_table(path, "session")$status, "finished")
  expect_true(is.na(read_table(path, "session")$round_ends_at))
  exec_sql(path, "UPDATE session SET status='running', current_round=1, round_ends_at=0")
  maybe_advance(path)
  maybe_advance(path)
  expect_equal(read_table(path, "session")$current_round, 2)
  expect_gt(read_table(path, "session")$round_ends_at, now())
})

test_that("late join uses smallest group, fresh index, and frozen sheets", {
  path <- new_db(withr::local_tempfile(), n = 16)
  start_session(path)
  frozen <- read_table(path, "topics")
  pid <- add_participant(path, "Late")
  p <- read_table(path, "participants")
  me <- p[p$pid == pid, ]
  expect_equal(me$grp, 2)
  expect_equal(me$idx, 6)
  ns <- frozen$n_sheets[topic_for(me$grp, 1, 3)]
  expect_true(sheet_for(me$idx, ns) %in% seq_len(ns))
  expect_identical(read_table(path, "topics"), frozen)
})

test_that("lifecycle rejects joins and starts outside allowed states", {
  path <- withr::local_tempfile()
  init_db(path)
  expect_null(add_participant(path, "No"))
  expect_match(start_session(path), "Lobby")
  exec_sql(path, "UPDATE session SET status='finished'")
  expect_null(add_participant(path, "No"))
  exec_sql(path, "UPDATE session SET status='lobby'")
  expect_identical(start_session(path), "Mindestens 3 Teilnehmer noetig (aktuell 0).")
  pid <- add_participant(path, "Lobby")
  expect_type(pid, "character")
  expect_true(is.na(read_table(path, "participants")$grp))
})

test_that("schema and connection pragmas match the persistence contract", {
  path <- withr::local_tempfile()
  init_db(path)
  con <- db(path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "PRAGMA busy_timeout")[[1]], 5000)
  expect_equal(DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1]], "wal")
  expect_error(DBI::dbExecute(con, "INSERT INTO session (id) VALUES (2)"), "CHECK")
  expect_named(read_table(path, "entries"), c(
    "id", "topic_id", "sheet", "round",
    "question", "pid", "text", "submitted", "updated_at"
  ))
})
