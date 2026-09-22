variable_settings <- function() {
  list(format = "brainwriting635-settings/1", mode = "individual", rounds = 2,
       round_secs = 300, turn_secs = 90,
       topics = list(list(title = "A", questions = "One prompt"),
                     list(title = "B", questions = c("First", "Second", "Third"))))
}

variable_db <- function(path) {
  init_db(path)
  settings <- variable_settings()
  stopifnot(is.null(configure_session(path, 2, 2, 300, list(), settings)))
  add_participant(path, "Alice")
  add_participant(path, "Bob")
  start_session(path)
  path
}
