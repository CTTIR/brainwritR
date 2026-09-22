capturing_session <- function() {
  session <- shiny::MockShinySession$new()
  messages <- new.env(parent = emptyenv())
  messages$log <- list()
  session$sendCustomMessage <- function(type, message) {
    messages$log[[length(messages$log) + 1L]] <- list(type = type, message = message)
  }
  attr(session, "messages") <- messages
  session
}

sent <- function(session, type) {
  log <- attr(session, "messages")$log
  Filter(function(x) identical(x$type, type), log)
}

route_to <- function(..., envir = parent.frame()) {
  query <- list(...)
  testthat::local_mocked_bindings(request_query = function(session) query, .env = envir)
}

multi_fixture <- function(envir = parent.frame()) {
  main <- file.path(withr::local_tempdir(.local_envir = envir), "brainwriting.sqlite")
  init_db(main)
  list(main = main, cfg = app_config(main, "secret", "https://bw.example.org", poll_ms = 50))
}

catalog_fixture <- function(envir = parent.frame()) {
  main <- file.path(withr::local_tempdir(.local_envir = envir), "brainwriting.sqlite")
  init_db(main)
  main
}

finish_with_entry <- function(path, text = "Eine Idee") {
  new_db(path, n = 3)
  start_session(path)
  pid <- read_table(path, "participants")$pid[1]
  save_entry(path, pid, 1, 1, 1, 1, text, 1)
  for (i in 1:3) maybe_advance(path, TRUE)
  invisible(path)
}

plenum_fixture_entries <- function(path) {
  p <- read_table(path, "participants")
  for (r in 1:3) {
    for (i in seq_len(nrow(p))) {
      topic <- topic_for(p$grp[i], r, 3)
      save_entry(path, p$pid[i], topic, 1, r, 1, paste("Idee", i, "aus Runde", r), 1)
      save_entry(path, p$pid[i], topic, 1, r, 2, if (r == 3) "" else paste("Weiter", i, r))
    }
  }
  invisible(path)
}

plenum_fixture <- function(path) {
  new_db(path, n = 3)
  start_session(path)
  for (i in 1:3) maybe_advance(path, TRUE)
  plenum_fixture_entries(path)
  open_plenum(path)
  invisible(path)
}

hot_plenum_fixture <- function(path) {
  init_db(path)
  topics <- rep(list(list(t = "Thema", a = "Frage 1", b = "Frage 2")), 2)
  settings <- list(format = "brainwriting635-settings/1", mode = "hot_seat", rounds = 1,
                   round_secs = 300, turn_secs = 60,
                   topics = lapply(topics, function(x) list(title = x$t, q1 = x$a, q2 = x$b)),
                   participants = c("Ada", "Bo", "Cy"))
  configure_session(path, 2, 1, 300, topics, settings = settings)
  save_entry(path, current_author(path)$pid, 1, 1, 1, 1, "Gemeinsame Idee", 1)
  end_pass(path)
  open_plenum(path)
  invisible(path)
}
