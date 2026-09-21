test_that("v1 databases migrate without disturbing existing rows", {
  path <- withr::local_tempfile()
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE session (id INTEGER PRIMARY KEY,
    status TEXT DEFAULT 'setup', n_groups INTEGER DEFAULT 3, n_rounds INTEGER DEFAULT 3,
    round_secs INTEGER DEFAULT 300, current_round INTEGER DEFAULT 0, round_ends_at REAL)")
  DBI::dbExecute(con, "INSERT INTO session(id, status) VALUES (1, 'lobby')")
  DBI::dbDisconnect(con)
  init_db(path)
  init_db(path)
  s <- read_table(path, "session")
  expect_identical(s$status, "lobby")
  expect_identical(s$mode, "individual")
  expect_equal(s$current_turn, 0)
})

test_that("hot seat traverses authors and uses identical rotation assignments", {
  path <- new_db(withr::local_tempfile(), n = 5)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  expect_null(start_session(path))
  p <- read_table(path, "participants")
  p <- p[order(p$joined_at), ]
  for (r in 1:3) {
    seen <- character()
    for (i in 1:5) {
      s <- read_table(path, "session")
      expect_equal(s$current_round, r)
      expect_equal(s$current_turn, i)
      expect_true(is.na(s$round_ends_at))
      a <- current_author(path)
      seen <- c(seen, a$pid)
      expect_equal(topic_for(a$grp, r, 3), ((a$grp - 1 + r - 1) %% 3) + 1)
      topic <- read_table(path, "topics")
      ns <- topic$n_sheets[topic$id == topic_for(a$grp, r, 3)]
      expect_equal(sheet_for(a$idx, ns), ((a$idx - 1) %% ns) + 1)
      maybe_advance(path, force = TRUE)
    }
    expect_identical(seen, p$pid)
  }
  expect_identical(read_table(path, "session")$status, "finished")
  expect_equal(nrow(read_table(path, "entries")), 0)
})

test_that("expired kiosk ticks advance only once into handover", {
  path <- new_db(withr::local_tempfile(), n = 5)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  exec_sql(path, "UPDATE session SET round_ends_at = 0")
  maybe_advance(path)
  maybe_advance(path)
  expect_equal(read_table(path, "session")$current_turn, 2)
  expect_true(is.na(read_table(path, "session")$round_ends_at))
  extend_clock(path, 30)
  expect_true(is.na(read_table(path, "session")$round_ends_at))
  end_pass(path)
  expect_equal(read_table(path, "session")$current_round, 2)
  expect_equal(read_table(path, "session")$current_turn, 1)
  abort_session(path)
  expect_identical(read_table(path, "session")$status, "finished")
})

test_that("hot seat late arrivals append to the remaining passes", {
  path <- new_db(withr::local_tempfile(), n = 5)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  maybe_advance(path, TRUE)
  pid <- add_participant(path, "Nachzuegler")
  p <- read_table(path, "participants")
  a <- p[p$pid == pid, ]
  expect_equal(a$idx, max(p$idx[p$grp == a$grp]))
  for (i in 2:5) maybe_advance(path, TRUE)
  expect_identical(current_author(path)$pid, pid)
  maybe_advance(path, TRUE)
  for (i in 1:5) maybe_advance(path, TRUE)
  expect_identical(current_author(path)$pid, pid)
})

test_that("group devices keep canonical identities and contribute through one engine", {
  path <- new_db(withr::local_tempfile())
  exec_sql(path, "UPDATE session SET mode = 'group_device'")
  local_mocked_bindings(get_settings = function(db_path) {
    list(groups = paste("Gruppe", 1:3))
  })
  pids <- vapply(1:3, function(g) claim_group(path, g), character(1))
  expect_identical(claim_group(path, 1), pids[1])
  expect_equal(nrow(read_table(path, "participants")), 3)
  expect_null(add_participant(path, "Unzulässig"))
  expect_null(start_session(path))
  expect_equal(read_table(path, "topics")$n_sheets, rep(1L, 3))
  for (r in 1:3) {
    for (g in 1:3) {
      for (q in 1:2) {
        save_entry(path, pids[g], topic_for(g, r, 3), 1, r, q, "Beitrag", 1)
      }
    }
    maybe_advance(path, TRUE)
  }
  e <- read_table(path, "entries")
  expect_equal(nrow(e), 18)
  for (topic in 1:3) {
    for (question in 1:2) {
      subset <- e[e$topic_id == topic & e$question == question, ]
      expect_setequal(subset$pid, pids)
      expect_equal(sort(subset$round), 1:3)
    }
  }
})

test_that("group devices require all claims unless moderator overrides", {
  path <- new_db(withr::local_tempfile())
  exec_sql(path, "UPDATE session SET mode = 'group_device'")
  expect_type(start_session(path), "character")
  expect_identical(read_table(path, "session")$status, "lobby")
  expect_null(start_session(path, force = TRUE))
  expect_identical(read_table(path, "session")$status, "running")
})

test_that("handover start arms once and preserves authoritative timer", {
  path <- new_db(withr::local_tempfile(), n = 5)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  expect_equal(arm_turn(path), 1)
  first <- read_table(path, "session")$round_ends_at
  expect_gt(first, now())
  expect_equal(arm_turn(path), 0)
  expect_identical(read_table(path, "session")$round_ends_at, first)
  extend_clock(path, 30)
  expect_equal(read_table(path, "session")$round_ends_at, first + 30)
  maybe_advance(path)
  expect_equal(read_table(path, "session")$current_turn, 1)
})

mode_settings <- function(mode, names = character()) {
  list(format = "brainwriting635-settings/1", mode = mode,
       rounds = 3L, round_secs = 30L, turn_secs = 20L,
       topics = lapply(1:3, function(i) {
         list(title = paste("Thema", i), q1 = "Warum?", q2 = "Wie?")
       }), groups = c("Nord", "Sued", "West"), participants = names)
}

test_that("mode settings persist, survive reconnect, and reset completely", {
  path <- withr::local_tempfile()
  init_db(path)
  settings <- mode_settings("group_device")
  expect_null(configure_session(path, 3, 3, 30, NULL, settings))
  expect_identical(get_settings(path), validate_settings(settings))
  init_db(path)
  expect_identical(get_settings(path), validate_settings(settings))
  expect_equal(read_table(path, "topics")$n_sheets, rep(1L, 3))
  pid <- claim_group(path, 1)
  expect_identical(read_table(path, "participants")$name, "Nord")
  expect_identical(claim_group(path, 1), pid)
  expect_match(configure_session(path, 3, 3, 30, NULL, settings), "bereits")
  start_session(path, TRUE)
  abort_session(path)
  expect_true(reset_session(path))
  s <- read_table(path, "session")
  expect_identical(s$mode, "individual")
  expect_true(is.na(s$settings_yaml))
  expect_equal(nrow(read_table(path, "participants")), 0)
})

test_that("hot seat starts directly with one participant and usable empty-group sheets", {
  path <- withr::local_tempfile()
  init_db(path)
  settings <- mode_settings("hot_seat", "Einzelperson")
  expect_null(configure_session(path, 3, 3, 30, NULL, settings))
  expect_identical(read_table(path, "session")$status, "running")
  expect_equal(read_table(path, "topics")$n_sheets, rep(1L, 3))
  expect_identical(current_author(path)$name, "Einzelperson")
  expect_equal(arm_turn(path), 1)
  expect_equal(read_table(path, "session")$round_ends_at - now(), 20, tolerance = 1)
  pid <- add_participant(path, "Spaeter")
  participant <- read_table(path, "participants")
  expect_equal(participant$grp[participant$pid == pid], 2)
  maybe_advance(path, TRUE)
  expect_identical(current_author(path)$pid, pid)
})

test_that("empty hot-seat roster leaves database untouched", {
  path <- withr::local_tempfile()
  init_db(path)
  expect_match(configure_session(path, 3, 3, 30, NULL, mode_settings("hot_seat")),
               "mindestens")
  expect_identical(read_table(path, "session")$status, "setup")
  expect_equal(nrow(read_table(path, "topics")), 0)
})

test_that("stale handover and completion actions cannot change the next turn", {
  path <- new_db(withr::local_tempfile(), n = 3)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  first <- read_table(path, "session")
  expect_equal(arm_turn(path, expected = first), 1)
  maybe_advance(path, force = TRUE, expected = first)
  expect_equal(read_table(path, "session")$current_turn, 2)
  maybe_advance(path, force = TRUE, expected = first)
  expect_equal(read_table(path, "session")$current_turn, 2)
  expect_equal(arm_turn(path, expected = first), 0)
  expect_true(is.na(read_table(path, "session")$round_ends_at))
  expect_false(end_pass(path, expected = first))
  expect_equal(read_table(path, "session")$current_round, 1)
  second <- read_table(path, "session")
  expect_equal(end_pass(path, expected = second), 1)
  expect_false(end_pass(path, expected = second))
  expect_equal(read_table(path, "session")$current_round, 2)
  expect_equal(read_table(path, "session")$current_turn, 1)
  expect_equal(arm_turn(path, expected = first), 0)
})
