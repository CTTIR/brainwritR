hot_guard_db <- function(path, armed = FALSE) {
  new_db(path, n = 3)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  if (armed) arm_turn(path)
  path
}

test_that("rapid repeated kiosk submission never skips the next author", {
  path <- hot_guard_db(withr::local_tempfile(), armed = TRUE)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  author <- current_author(path)$pid
  shiny::testServer(app_server(cfg), {
    session$flushReact()
    session$setInputs(a1 = "Erster Beitrag", a2 = "Zweiter Beitrag")
    session$setInputs(submit_btn = 1)
    expect_equal(read_table(path, "session")$current_turn, 2)
    session$setInputs(submit_btn = 2)
    expect_equal(read_table(path, "session")$current_turn, 2)
    expect_true(is.na(read_table(path, "session")$round_ends_at))
    entries <- read_table(path, "entries")
    expect_equal(nrow(entries), 2)
    expect_identical(unique(entries$pid), author)
    expect_equal(entries$submitted, c(1L, 1L))
  })
})

test_that("rapid repeated handover skip advances only the rendered author", {
  path <- hot_guard_db(withr::local_tempfile())
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    session$flushReact()
    session$setInputs(turn_skip = 1)
    expect_equal(read_table(path, "session")$current_turn, 2)
    session$setInputs(turn_skip = 2)
    expect_equal(read_table(path, "session")$current_turn, 2)
    expect_equal(nrow(read_table(path, "entries")), 0)
    session$setInputs(turn_start = 1)
    expect_true(is.na(read_table(path, "session")$round_ends_at))
  })
})

test_that("rapid repeated moderator pass completion advances only one pass", {
  path <- hot_guard_db(withr::local_tempfile(), armed = TRUE)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    session$setInputs(next_btn = 1)
    expect_equal(read_table(path, "session")$current_round, 2)
    session$setInputs(next_btn = 2)
    expect_equal(read_table(path, "session")$current_round, 2)
    expect_equal(read_table(path, "session")$current_turn, 1)
    expect_true(is.na(read_table(path, "session")$round_ends_at))
    expect_equal(nrow(read_table(path, "entries")), 0)
  })
})

test_that("forged kiosk moderator controls and downloads require authentication", {
  path <- hot_guard_db(withr::local_tempfile(), armed = TRUE)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  before <- read_table(path, "session")
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "wrong", pin_btn = 1)
    session$setInputs(abort_btn = 1, abort_confirm = 1, mod_skip = 1,
                      next_btn = 1, plus60_btn = 1, late_name = "Forged", late_add = 1)
    expect_false(is_mod())
    expect_identical(read_table(path, "session"), before)
    expect_equal(nrow(read_table(path, "participants")), 3)
    for (id in c("dl_rds", "dl_xlsx", "dl_pdf")) {
      expect_error(output[[id]], class = "shiny.silent.error")
    }
    session$setInputs(pin = "secret", pin_btn = 2)
    session$setInputs(abort_btn = 2, abort_confirm = 2)
    expect_identical(read_table(path, "session")$status, "finished")
  })
})

test_that("repeated round-end events from one rendered round advance only once", {
  for (mode in c("individual", "group_device")) {
    path <- new_db(withr::local_tempfile(), n = 3)
    exec_sql(path, sprintf("UPDATE session SET mode = '%s'", mode))
    start_session(path, force = TRUE)
    cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
    shiny::testServer(app_server(cfg), {
      session$setInputs(pin = "secret", pin_btn = 1)
      session$setInputs(next_btn = 1)
      session$setInputs(next_btn = 2)
      expect_equal(read_table(path, "session")$current_round, 2, label = mode)
      # Once the new round has been rendered, the next action advances normally.
      session$elapse(3000)
      session$setInputs(next_btn = 3)
      expect_equal(read_table(path, "session")$current_round, 3, label = mode)
    })
  }
})
