#' Run the classroom brainwriting application
#'
#' Starts one multilingual Shiny application backed by SQLite. Open
#' `base_url` for participants and append `/?mod=1` for the moderator.
#' Configuration is resolved at invocation, never at package load time.
#'
#' The main database holds the standard session behind `base_url` and the
#' catalog of all further sessions. Each further session is a separate SQLite
#' file in a `sessions` folder next to the main database, with its own address
#' (`?s=<code>`) and QR code. Moderators create, open, restart, archive and
#' delete sessions in the overview at `?mod=1&view=sessions`.
#' @param db_path Main SQLite file path. Defaults to `DB_PATH`, then
#'   `"data/brainwriting.sqlite"`.
#' @param mod_pin Moderator PIN. Defaults to `MOD_PIN`, then `"635"`.
#'   The demonstration PIN is accepted only with a loopback host and URL.
#' @param base_url Public participant URL used in the QR code. Defaults to
#'   `BASE_URL`, then `"http://localhost:3838"`. Must not contain credentials,
#'   a fragment, or the reserved query parameters `s`, `mod`, and `view`.
#' @param port Listening port. Defaults to `PORT`, then `3838`.
#' @param host Listening address, default `"127.0.0.1"`. Set `"0.0.0.0"`
#'   explicitly to accept network connections, with a private moderator PIN.
#' @param poll_ms Database polling interval in milliseconds, default `2500`.
#' @return Invisibly, the value returned by [shiny::runApp()] when the app stops.
#' @details Use exactly one R process per data folder. No replicas or horizontal
#'   scaling are supported. A round advances on the next connected client's poll;
#'   with no clients connected it advances when someone reconnects. Moderator
#'   logins are kept in server memory, so a restart asks for the PIN again.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(db_path = Sys.getenv("DB_PATH", "data/brainwriting.sqlite"),
                    mod_pin = Sys.getenv("MOD_PIN", "635"),
                    base_url = Sys.getenv("BASE_URL", "http://localhost:3838"),
                    port = Sys.getenv("PORT", "3838"), host = "127.0.0.1",
                    poll_ms = 2500) {
  cfg <- app_config(db_path, mod_pin, base_url, port, host, poll_ms)
  dir.create(dirname(cfg$db_path), showWarnings = FALSE, recursive = TRUE)
  init_db(cfg$db_path)
  migrate_sessions(cfg$db_path)
  invisible(shiny::runApp(shiny::shinyApp(app_ui(cfg), app_server(cfg)),
    host = cfg$host, port = cfg$port
  ))
}

#' Validate runtime configuration
#' @keywords internal
#' @noRd
app_config <- function(db_path, mod_pin, base_url, port = 3838,
                       host = "127.0.0.1", poll_ms = 2500) {
  for (x in list(db_path = db_path, mod_pin = mod_pin, base_url = base_url, host = host)) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop("Paths, PIN, URL and host must be non-empty strings.", call. = FALSE)
    }
  }
  if (identical(db_path, ":memory:")) {
    stop("A persistent SQLite file is required for per-operation connections.", call. = FALSE)
  }
  url_host <- validate_base_url(base_url)
  if (identical(trimws(mod_pin), "635") &&
        !(tolower(host) %in% c("localhost", "127.0.0.1", "::1") &&
            url_host %in% c("localhost", "127.0.0.1", "[::1]"))) {
    stop("Set a private mod_pin before accepting network connections or using a public URL.",
         call. = FALSE)
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

#' Validate the invitation base URL and return its host
#' @keywords internal
#' @noRd
validate_base_url <- function(url) {
  if (!grepl("^https?://[^/?#]+(?:[/?][^#]*)?$", url, perl = TRUE) ||
        grepl("[[:space:]\\\\]", url) || grepl("%(?![[:xdigit:]]{2})", url, perl = TRUE)) {
    stop("base_url must be an HTTP or HTTPS URL with a host and no fragment.", call. = FALSE)
  }
  authority <- sub("^https?://([^/?]+).*$", "\\1", url)
  pattern <- "^(\\[[0-9A-Fa-f.]*:[0-9A-Fa-f:.]*\\]|[A-Za-z0-9][A-Za-z0-9.-]*)(?::([0-9]+))?$"
  pieces <- regmatches(authority, regexec(pattern, authority, perl = TRUE))[[1]]
  if (!length(pieces) || grepl("\\.\\.", pieces[2])) {
    stop("base_url must have a valid host and must not contain credentials.", call. = FALSE)
  }
  if (nzchar(pieces[3])) {
    url_port <- suppressWarnings(as.numeric(pieces[3]))
    if (!is.finite(url_port) || url_port < 1 || url_port > 65535) {
      stop("base_url port must be between 1 and 65535.", call. = FALSE)
    }
  }
  if (grepl("?", url, fixed = TRUE)) {
    query <- sub("^[^?]*\\?", "", url)
    keys <- utils::URLdecode(sub("=.*$", "", strsplit(query, "&", fixed = TRUE)[[1]]))
    if (any(keys %in% c("s", "mod", "view"))) {
      stop("base_url must not contain the reserved query parameters s, mod or view.", call. = FALSE)
    }
  }
  tolower(pieces[2])
}
