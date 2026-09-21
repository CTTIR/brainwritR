test_that("app builds without changing persistence", {
  path <- new_db(withr::local_tempfile())
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  before <- read_table(path, "session")
  app <- shiny::shinyApp(app_ui(cfg), app_server(cfg))
  expect_s3_class(app, "shiny.appobj")
  expect_identical(read_table(path, "session"), before)
  html <- as.character(app_ui(cfg))
  expect_identical(attr(app_ui(cfg), "lang"), "de")
  expect_false(grepl("maximum-scale|fonts.googleapis", html))
})

test_that("forged moderator inputs cannot change a participant session", {
  path <- new_db(withr::local_tempfile(), n = 3)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "wrong", pin_btn = 1)
    expect_false(is_mod())
    session$setInputs(
      start_btn = 1, next_btn = 1, plus60_btn = 1,
      reset_btn = 1, reset_confirm = 1, setup_save = 1
    )
    expect_identical(read_table(path, "session")$status, "lobby")
    expect_equal(nrow(read_table(path, "participants")), 3)
    session$setInputs(pin = "secret", pin_btn = 2)
    expect_true(is_mod())
    session$setInputs(start_btn = 2)
    expect_identical(read_table(path, "session")$status, "running")
    session$setInputs(reset_confirm = 2)
    expect_identical(read_table(path, "session")$status, "running")
  })
})

test_that("joining twice in one connection does not duplicate membership", {
  path <- new_db(withr::local_tempfile())
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(join_name = "Alex", join_btn = 1)
    pid <- my_pid()
    session$setInputs(join_btn = 2)
    expect_identical(my_pid(), pid)
    expect_equal(nrow(read_table(path, "participants")), 1)
    session$setInputs(stored_pid = "dangling")
    expect_null(my_pid())
    session$setInputs(stored_pid = pid)
    expect_identical(my_pid(), pid)
  })
})

test_that("draft debounce keeps the context of the edit across a round change", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  pid <- read_table(path, "participants")$pid[1]
  shiny::testServer(app_server(cfg), {
    session$setInputs(stored_pid = pid)
    session$setInputs(a1 = "")
    session$setInputs(a1 = "Previous round draft")
    maybe_advance(path, force = TRUE)
    session$elapse(100)
    session$elapse(1300)
    e <- read_table(path, "entries")
    expect_equal(nrow(e), 1)
    expect_equal(e$round, 1)
    expect_equal(e$text, "Previous round draft")
  })
})

test_that("moderator setup, live regions and guarded reset follow the lifecycle", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, n_groups = 3)
    expect_match(output$mod_view$html, "Session einrichten")
    expect_match(output$topic_form$html, "Titel des Themas")
    session$setInputs(setup_save = 1)
    expect_equal(read_table(path, "session")$status, "setup")
    session$setInputs(
      t_title_1 = "Topic A", t_q1_1 = "A1", t_q2_1 = "A2",
      t_title_2 = "Topic B", t_q1_2 = "B1", t_q2_2 = "B2",
      t_title_3 = "Topic C", t_q1_3 = "C1", t_q2_3 = "C2",
      n_rounds = 3, round_secs = 300, setup_save = 2
    )
    session$elapse(100)
    expect_equal(read_table(path, "session")$status, "lobby")
    expect_match(output$mod_view$html, "Lobby")
    expect_match(output$mod_lobby_list$html, "Noch niemand")
    expect_true(nzchar(output$qr$src))
    pids <- vapply(c("Alpha", "Beta", "Gamma"), function(nm) add_participant(path, nm), "")
    session$elapse(100)
    expect_match(output$mod_lobby_list$html, "Alpha")
    session$setInputs(start_btn = 1)
    session$elapse(100)
    expect_match(output$mod_view$html, "Runde beenden")
    expect_match(output$mod_progress$html, "0 / 1 abgegeben")
    before <- read_table(path, "session")$round_ends_at
    session$setInputs(plus60_btn = 1)
    expect_equal(read_table(path, "session")$round_ends_at, before + 60)
    save_entry(path, pids[1], 1, 1, 1, 1, "Useful contribution", 1)
    for (i in 1:3) {
      session$setInputs(next_btn = i)
      session$elapse(100)
    }
    expect_match(output$mod_view$html, "Ergebnisse")
    expect_match(output$mod_results$html, "Useful contribution")
    session$setInputs(reset_confirm = 1)
    expect_equal(nrow(read_table(path, "participants")), 3)
    session$setInputs(reset_btn = 1, reset_confirm = 2)
    session$elapse(100)
    expect_match(output$mod_view$html, "Session einrichten")
    expect_equal(nrow(read_table(path, "participants")), 0)
  })
})
