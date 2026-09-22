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
