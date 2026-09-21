#' Run the classroom brainwriting application
#'
#' Starts one multilingual Shiny application backed by SQLite. Open
#' `base_url` for participants and append `/?mod=1` for the moderator.
#' Configuration is resolved at invocation, never at package load time.
#' @param db_path SQLite file path. Defaults to `DB_PATH`, then
#'   `"data/brainwriting.sqlite"`.
#' @param mod_pin Moderator PIN. Defaults to `MOD_PIN`, then `"635"`.
#'   Set a private PIN before deployment.
#' @param base_url Public participant URL used in the QR code. Defaults to
#'   `BASE_URL`, then `"http://localhost:3838"`.
#' @param port Listening port. Defaults to `PORT`, then `3838`.
#' @param host Listening address, default `"0.0.0.0"`.
#' @param poll_ms Database polling interval in milliseconds, default `2500`.
#' @return Invisibly, the value returned by [shiny::runApp()] when the app stops.
#' @details Use exactly one R process per database. No replicas or horizontal
#'   scaling are supported. A round advances on the next connected client's poll;
#'   with no clients connected it advances when someone reconnects.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(db_path = Sys.getenv("DB_PATH", "data/brainwriting.sqlite"),
                    mod_pin = Sys.getenv("MOD_PIN", "635"),
                    base_url = Sys.getenv("BASE_URL", "http://localhost:3838"),
                    port = Sys.getenv("PORT", "3838"), host = "0.0.0.0",
                    poll_ms = 2500) {
  cfg <- app_config(db_path, mod_pin, base_url, port, host, poll_ms)
  dir.create(dirname(cfg$db_path), showWarnings = FALSE, recursive = TRUE)
  init_db(cfg$db_path)
  invisible(shiny::runApp(shiny::shinyApp(app_ui(cfg), app_server(cfg)),
    host = cfg$host, port = cfg$port
  ))
}

#' Validate runtime configuration
#' @keywords internal
#' @noRd
app_config <- function(db_path, mod_pin, base_url, port = 3838,
                       host = "0.0.0.0", poll_ms = 2500) {
  for (x in list(db_path = db_path, mod_pin = mod_pin, base_url = base_url, host = host)) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop("Paths, PIN, URL and host must be non-empty strings.", call. = FALSE)
    }
  }
  if (identical(db_path, ":memory:")) {
    stop("A persistent SQLite file is required for per-operation connections.", call. = FALSE)
  }
  if (!grepl("^https?://[^[:space:]]+$", base_url)) {
    stop("base_url must be an HTTP or HTTPS URL.", call. = FALSE)
  }
  port <- suppressWarnings(as.numeric(port))
  if (length(port) != 1L) stop("port must be a single integer.", call. = FALSE)
  check_index(port, "port", 1, 65535)
  if (length(poll_ms) != 1L) stop("poll_ms must be a single integer.", call. = FALSE)
  check_index(poll_ms, "poll_ms", 1)
  list(
    db_path = db_path, mod_pin = mod_pin, base_url = base_url,
    port = as.integer(port), host = host, poll_ms = poll_ms
  )
}
