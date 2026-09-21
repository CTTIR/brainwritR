test_that("forged kiosk moderator controls cannot bypass PIN authorization", {
  path <- new_db(withr::local_tempfile(), n = 3)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  before <- read_table(path, "session")
  shiny::testServer(app_server(cfg), {
    session$setInputs(
      mod_skip = 1, abort_btn = 1, abort_confirm = 1,
      plus60_btn = 1, next_btn = 1, late_name = "Forged", late_add = 1
    )
    expect_identical(read_table(path, "session"), before)
    expect_equal(nrow(read_table(path, "participants")), 3)
    session$setInputs(pin = "secret", pin_btn = 1)
    session$setInputs(late_name = "Nachzuegler", late_add = 2)
    expect_equal(nrow(read_table(path, "participants")), 4)
    session$setInputs(mod_skip = 2)
    expect_equal(read_table(path, "session")$current_turn, 2)
    session$elapse(100)
    session$setInputs(next_btn = 2)
    expect_equal(read_table(path, "session")$current_round, 2)
    expect_equal(read_table(path, "session")$current_turn, 1)
    session$setInputs(abort_btn = 2, abort_confirm = 2)
    expect_identical(read_table(path, "session")$status, "finished")
  })
})

test_that("mode-specific identity inputs cannot create incompatible authors", {
  path <- new_db(withr::local_tempfile(), n = 3)
  exec_sql(path, "UPDATE session SET mode = 'hot_seat'")
  start_session(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  first <- current_author(path)$pid
  shiny::testServer(app_server(cfg), {
    session$setInputs(join_name = "Forged", join_btn = 1, stored_pid = "unknown",
                      group_choice = 1, group_confirm = 1, roster_choice = 1)
    expect_equal(nrow(read_table(path, "participants")), 3)
    expect_identical(active_pid(), first)
    expect_null(my_pid())
    session$setInputs(turn_start = 1)
    expect_false(is.na(read_table(path, "session")$round_ends_at))
    session$elapse(100)
    session$setInputs(a1 = "Kiosk answer", a2 = "Kiosk second answer", submit_btn = 1)
    expect_equal(nrow(read_table(path, "entries")), 2)
    expect_true(all(read_table(path, "entries")$pid == first))
    expect_equal(read_table(path, "session")$current_turn, 2)
    session$elapse(100)
    expect_false(identical(active_pid(), first))
  })
})

test_that("group claims validate identifiers and confirmed takeover reuses membership", {
  path <- new_db(withr::local_tempfile())
  exec_sql(path, "UPDATE session SET mode = 'group_device'")
  canonical <- claim_group(path, 2)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    for (invalid in list(0, 4, 1.5, "1", NA_real_)) {
      session$setInputs(group_choice = invalid)
      expect_equal(nrow(read_table(path, "participants")), 1)
      expect_null(my_pid())
    }
    session$setInputs(group_choice = 2)
    expect_null(my_pid())
    session$setInputs(group_confirm = 1)
    expect_identical(my_pid(), canonical)
    expect_equal(nrow(read_table(path, "participants")), 1)
    session$setInputs(join_name = "Wrong author", join_btn = 1)
    expect_equal(nrow(read_table(path, "participants")), 1)
  })
})
