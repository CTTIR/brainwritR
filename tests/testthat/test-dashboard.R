act <- function(session, action, code = "") {
  session$setInputs(session_action = list(action = action, code = code, nonce = runif(1)))
}

navigated <- function(session) {
  vapply(sent(session, "bw_navigate"), `[[`, "", "message")
}

test_that("the session overview requires the moderator PIN", {
  x <- multi_fixture()
  route_to(mod = "1", view = "sessions")
  shiny::testServer(app_server(x$cfg), {
    expect_match(output$page$html, 'id="pin"', fixed = TRUE)
    session$setInputs(session_new = 1, session_label = "Heimlich", session_create = 1)
    act(session, "delete")
  })
  expect_equal(nrow(catalog_rows(x$main)), 0)
})

test_that("the overview lists every session with state, address and actions", {
  x <- multi_fixture()
  finish_with_entry(x$main)
  code <- create_session(x$main, "Kurs Liste")
  archived <- create_session(x$main, "Kurs Archiv")
  finish_with_entry(session_path(x$main, archived))
  archive_session(x$main, archived)
  route_to(mod = "1", view = "sessions")
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_match(output$page$html, "Neue Session", fixed = TRUE)
    html <- output$sessions_list$html
    expect_match(html, "Standard-Session", fixed = TRUE)
    expect_match(html, "Kurs Liste", fixed = TRUE)
    expect_match(html, paste0("https://bw.example.org?s=", code), fixed = TRUE)
    expect_match(html, "Beendet", fixed = TRUE)
    expect_match(html, "Einrichtung", fixed = TRUE)
    expect_match(html, paste0('href="?mod=1&amp;s=', code, '"'), fixed = TRUE)
    expect_false(grepl("Kurs Archiv", html, fixed = TRUE))
    session$setInputs(sessions_filter = "archived")
    html <- output$sessions_list$html
    expect_match(html, "Kurs Archiv", fixed = TRUE)
    expect_match(html, "Wiederherstellen", fixed = TRUE)
    expect_false(grepl("Kurs Liste", html, fixed = TRUE))
  })
})

test_that("creating and restarting sessions opens them for review", {
  x <- multi_fixture()
  finish_with_entry(x$main)
  route_to(mod = "1", view = "sessions")
  session <- capturing_session()
  shiny::testServer(app_server(x$cfg), session = session, {
    session$setInputs(pin = "secret", pin_btn = 1, session_new = 1)
    session$setInputs(session_label = "Kurs Neu", session_create = 1)
    act(session, "rerun")
  })
  rows <- catalog_rows(x$main)
  expect_identical(rows$label, c("Kurs Neu", "Standard-Session (Wiederholung)"))
  expect_identical(navigated(session), paste0("?mod=1&s=", rows$code))
})

test_that("QR codes are shown and downloadable for every session", {
  x <- multi_fixture()
  code <- create_session(x$main, "Kurs QR")
  route_to(mod = "1", view = "sessions")
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    act(session, "qr", code)
    expect_true(nzchar(output$session_qr$src))
    file <- output$session_qr_png
    png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
    expect_identical(readBin(file, "raw", 8L), png_signature)
  })
})

test_that("archiving, restoring and deleting follow the lifecycle and confirmations", {
  x <- multi_fixture()
  code <- create_session(x$main, "Kurs Pflege")
  route_to(mod = "1", view = "sessions")
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    act(session, "archive", code)
    expect_false(any(!is.na(catalog_rows(x$main)$archived_at)))
    finish_with_entry(session_path(x$main, code))
    act(session, "archive", code)
    expect_false(is.na(catalog_rows(x$main)$archived_at))
    act(session, "restore", code)
    expect_true(is.na(catalog_rows(x$main)$archived_at))
    act(session, "delete", code)
    expect_true(file.exists(session_path(x$main, code)))
    session$setInputs(session_confirm = 1)
    expect_false(file.exists(session_path(x$main, code)))
    expect_equal(nrow(catalog_rows(x$main)), 0)
  })
})

test_that("the standard session is archived or reset only after confirmation", {
  x <- multi_fixture()
  finish_with_entry(x$main, "Standardidee")
  route_to(mod = "1", view = "sessions")
  shiny::testServer(app_server(x$cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    act(session, "archive")
    expect_identical(read_table(x$main, "session")$status, "finished")
    session$setInputs(session_confirm = 1)
    expect_identical(read_table(x$main, "session")$status, "setup")
    expect_equal(nrow(catalog_rows(x$main)), 1)
    new_db(x$main, n = 3)
    start_session(x$main)
    act(session, "delete")
    session$setInputs(session_confirm = 2)
    expect_identical(read_table(x$main, "session")$status, "setup")
    expect_equal(nrow(read_table(x$main, "participants")), 0)
  })
})
