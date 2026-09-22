test_that("storage gains plenum state and a weights table without touching old data", {
  path <- withr::local_tempfile()
  init_db(path)
  s <- read_table(path, "session")
  expect_identical(s$plenum, "none")
  expect_identical(s$plenum_turn, 0L)
  expect_named(read_table(path, "votes"),
               c("pid", "entry_id", "topic_id", "points", "updated_at"))
  expect_named(read_table(path, "entries"), c(
    "id", "topic_id", "sheet", "round", "question", "pid", "text", "submitted", "updated_at"
  ))
})

test_that("weighting opens only after writing and when there is something to weigh", {
  path <- new_db(withr::local_tempfile(), n = 3)
  expect_match(open_plenum(path), "Schreibphase")
  start_session(path)
  for (i in 1:3) maybe_advance(path, TRUE)
  expect_match(open_plenum(path), "keine Beitr")
  plenum_fixture_entries(path)
  expect_null(open_plenum(path))
  expect_identical(read_table(path, "session")$plenum, "open")
  expect_null(close_plenum(path))
  expect_identical(read_table(path, "session")$plenum, "closed")
  expect_match(close_plenum(path), "nicht")
  expect_null(open_plenum(path))
})

test_that("each voter distributes at most 100 percent per topic in steps of five", {
  path <- plenum_fixture(withr::local_tempfile())
  p <- read_table(path, "participants")$pid
  e <- read_table(path, "entries")
  e <- e[nzchar(e$text), ]
  t1 <- e$id[e$topic_id == 1]
  t2 <- e$id[e$topic_id == 2]
  expect_identical(set_weight(path, p[1], t1[1], 62), 60L)
  expect_identical(set_weight(path, p[1], t1[2], 70), 40L)
  expect_identical(set_weight(path, p[1], t2[1], 100), 100L)
  expect_identical(set_weight(path, p[1], t1[1], 20), 20L)
  expect_identical(set_weight(path, p[1], t1[2], 80), 80L)
  expect_identical(set_weight(path, p[1], t1[1], -5), 0L)
  votes <- read_table(path, "votes")
  expect_equal(sum(votes$points[votes$pid == p[1] & votes$topic_id == 1]), 80)
  expect_false(t1[1] %in% votes$entry_id)
  own <- e$id[e$pid == p[2]][1]
  expect_identical(set_weight(path, p[2], own, 30), 30L)
  expect_null(set_weight(path, "unknown", t1[1], 10))
  expect_null(set_weight(path, p[1], 99999, 10))
  expect_null(set_weight(path, p[1], t1[1], NA))
  close_plenum(path)
  expect_null(set_weight(path, p[1], t1[2], 5))
})

test_that("voters see all nonempty contributions anonymously in a stable personal order", {
  path <- plenum_fixture(withr::local_tempfile())
  p <- read_table(path, "participants")$pid
  items <- plenum_items(path, p[1])
  expect_named(items, c("id", "topic_id", "question", "text", "points"))
  expect_setequal(items$id, read_table(path, "entries")$id[
    nzchar(trimws(read_table(path, "entries")$text))
  ])
  expect_false(is.unsorted(items$topic_id))
  expect_identical(plenum_items(path, p[1]), items)
  other <- plenum_items(path, p[2])
  expect_false(identical(other$id, items$id))
  set_weight(path, p[1], items$id[1], 25)
  expect_equal(plenum_items(path, p[1])$points[1], 25)
})

test_that("results rank contributions by points with shares and supporters", {
  entries <- data.frame(id = 1:5, topic_id = c(1, 1, 1, 2, 2), sheet = 1, round = 1,
                        question = 1, pid = "a", submitted = 1, updated_at = 0,
                        text = c("Eins", "Zwei", "Drei", "Vier", " "))
  topics <- data.frame(id = 1:2, title = c("A", "B"), q1 = "?", q2 = "?", n_sheets = 1)
  votes <- data.frame(pid = c("x", "x", "y", "z"), entry_id = c(1, 2, 1, 4),
                      topic_id = c(1, 1, 1, 2), points = c(60, 40, 100, 50), updated_at = 0)
  r <- plenum_results(entries, topics, votes)
  expect_identical(r$entry_id, c(1L, 2L, 3L, 4L))
  expect_equal(r$points, c(160, 40, 0, 50))
  expect_equal(r$share, c(0.8, 0.2, 0, 1))
  expect_equal(r$mean, c(80, 20, 0, 50))
  expect_equal(r$supporters, c(2, 1, 0, 1))
  expect_equal(r$rank, c(1, 2, 3, 1))
  empty <- plenum_results(entries, topics, votes[FALSE, ])
  expect_true(all(is.na(empty$share)))
  expect_equal(nrow(plenum_results(entries, topics, NULL)), 4)
})

test_that("progress counts voters who used the whole budget per topic", {
  path <- plenum_fixture(withr::local_tempfile())
  p <- read_table(path, "participants")$pid
  e <- read_table(path, "entries")
  e <- e[nzchar(e$text), ]
  set_weight(path, p[1], e$id[e$topic_id == 1][1], 100)
  set_weight(path, p[2], e$id[e$topic_id == 1][1], 50)
  progress <- plenum_progress(path)
  expect_identical(progress$topic_id, 1:3)
  expect_equal(progress$complete, c(1, 0, 0))
  expect_equal(progress$voters, c(2, 0, 0))
  expect_equal(progress$eligible, c(3, 3, 3))
})

test_that("hot-seat weighting passes the device in arrival order and can skip", {
  path <- withr::local_tempfile()
  init_db(path)
  topics <- rep(list(list(t = "Thema", a = "Frage 1", b = "Frage 2")), 2)
  settings <- list(format = "brainwriting635-settings/1", mode = "hot_seat", rounds = 1,
                   round_secs = 300, turn_secs = 60,
                   topics = lapply(topics, function(x) list(title = x$t, q1 = x$a, q2 = x$b)),
                   participants = c("Ada", "Bo", "Cy"))
  expect_null(configure_session(path, 2, 1, 300, topics, settings = settings))
  save_entry(path, current_author(path)$pid, 1, 1, 1, 1, "Idee", 1)
  end_pass(path)
  expect_identical(read_table(path, "session")$status, "finished")
  expect_null(open_plenum(path))
  expect_identical(plenum_voter(path)$name, "Ada")
  advance_plenum(path, expected = 1L)
  advance_plenum(path, expected = 1L)
  expect_identical(plenum_voter(path)$name, "Bo")
  advance_plenum(path, expected = 2L)
  advance_plenum(path, expected = 3L)
  expect_equal(nrow(plenum_voter(path)), 0)
  expect_equal(read_table(path, "session")$plenum_turn, 4)
})

test_that("reset clears weights and archiving waits until weighting has ended", {
  main <- catalog_fixture()
  code <- create_session(main, "Kurs Plenum")
  path <- plenum_fixture(session_path(main, code))
  e <- read_table(path, "entries")
  e <- e[nzchar(e$text), ]
  set_weight(path, read_table(path, "participants")$pid[1], e$id[1], 50)
  expect_match(archive_session(main, code), "Gewichtung")
  close_plenum(path)
  expect_null(archive_session(main, code))
  restore_session(main, code)
  reset_session(path)
  expect_equal(nrow(read_table(path, "votes")), 0)
  expect_identical(read_table(path, "session")$plenum, "none")
})
