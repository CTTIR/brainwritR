setup_inputs <- function(session) {
  session$setInputs(
    t_title_1 = "Topic A", t_q1_1 = "A1", t_q2_1 = "A2",
    t_title_2 = "Topic B", t_q1_2 = "B1", t_q2_2 = "B2",
    t_title_3 = "Topic C", t_q1_3 = "C1", t_q2_3 = "C2",
    n_groups = 3, n_rounds = 3, round_secs = 300, setup_save = 1
  )
}

test_that("a coded moderator page configures only its own session file", {
  x <- multi_fixture()
  code <- create_session(x$main, "Kurs A")
  route_to(mod = "1", s = code)
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_match(output$page$html, "Kurs A", fixed = TRUE)
    expect_match(output$page$html, "Alle Sessions", fixed = TRUE)
    expect_match(output$mod_view$html, "Session einrichten")
    setup_inputs(session)
    session$elapse(100)
    expect_match(output$mod_view$html, "Lobby")
    expect_match(output$mod_view$html, paste0("https://bw.example.org?s=", code), fixed = TRUE)
  })
  expect_identical(read_table(session_path(x$main, code), "session")$status, "lobby")
  expect_identical(read_table(x$main, "session")$status, "setup")
})

test_that("participants join the session named by the address", {
  x <- multi_fixture()
  code <- create_session(x$main, "Kurs B")
  path <- session_path(x$main, code)
  new_db(path)
  route_to(s = code)
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(join_name = "Alex", join_btn = 1)
    expect_false(is.null(my_pid()))
  })
  expect_identical(read_table(path, "participants")$name, "Alex")
  expect_equal(nrow(read_table(x$main, "participants")), 0)
})

test_that("unknown and archived addresses show a notice instead of a session", {
  x <- multi_fixture()
  route_to(s = "zzzzzz")
  shiny::testServer(app_server(x$cfg), {
    expect_match(output$page$html, "Session nicht gefunden")
  })
  code <- create_session(x$main, "Kurs C")
  new_db(session_path(x$main, code))
  exec_sql(session_path(x$main, code), "UPDATE session SET status = 'finished'")
  archive_session(x$main, code)
  route_to(s = code)
  shiny::testServer(app_server(x$cfg), {
    expect_match(output$page$html, "Session archiviert")
  })
})

test_that("archived sessions stay reviewable but cannot be reset", {
  x <- multi_fixture()
  code <- create_session(x$main, "Kurs D")
  path <- session_path(x$main, code)
  new_db(path, n = 3)
  start_session(path)
  for (i in 1:3) maybe_advance(path, TRUE)
  archive_session(x$main, code)
  route_to(mod = "1", s = code)
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    html <- output$mod_view$html
    expect_match(html, "Archiviert")
    expect_match(html, "dl_csv", fixed = TRUE)
    expect_false(grepl("reset_btn", html, fixed = TRUE))
    session$setInputs(reset_btn = 1, reset_confirm = 1)
  })
  expect_equal(nrow(read_table(path, "participants")), 3)
})

test_that("a restarted session opens its setup prefilled with the earlier settings", {
  x <- multi_fixture()
  topics <- lapply(c("Erstes", "Zweites"), function(t) list(t = t, a = "Frage 1", b = "Frage 2"))
  settings <- list(format = "brainwriting635-settings/1", mode = "hot_seat", rounds = 4,
                   round_secs = 300, turn_secs = 45,
                   topics = lapply(topics, function(x) list(title = x$t, q1 = x$a, q2 = x$b)),
                   participants = c("Ada", "Bo"))
  expect_null(configure_session(x$main, 2, 4, 300, topics, settings = settings))
  code <- rerun_session(x$main, NA)
  route_to(mod = "1", s = code)
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    html <- output$mod_view$html
    expect_match(html, 'value="hot_seat" checked', fixed = TRUE)
    expect_match(html, "Ada\nBo", fixed = TRUE)
    expect_match(output$topic_form$html, "Zweites", fixed = TRUE)
    expect_equal(topic_form_gate()$k, 2)
  })
})

test_that("deleting a session reloads its open pages without recreating the file", {
  x <- multi_fixture()
  code <- create_session(x$main, "Kurs E")
  path <- session_path(x$main, code)
  new_db(path, n = 3)
  start_session(path)
  pid <- read_table(path, "participants")$pid[1]
  route_to(s = code)
  session <- capturing_session()
  shiny::testServer(app_server(x$cfg), session = session, {
    session$setInputs(stored_pid = pid)
    session$elapse(100)
    expect_null(delete_session(x$main, code))
    session$setInputs(a1 = "Nach dem Loeschen")
    session$elapse(1500)
  })
  expect_gt(length(sent(session, "bw_reload")), 0L)
  expect_false(file.exists(path))
})

test_that("request queries are read once from the connecting page", {
  session <- shiny::MockShinySession$new()
  expect_identical(request_query(session), list(mocksearch = "1"))
})
