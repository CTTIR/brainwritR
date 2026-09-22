#' Directory holding the additional session databases
#'
#' The main database (`DB_PATH`) keeps the standard session behind the bare
#' address and the catalog of all further sessions. Each further session is a
#' separate SQLite file with the unchanged session schema.
#' @param main_db Main database path.
#' @keywords internal
#' @noRd
sessions_dir <- function(main_db) file.path(dirname(main_db), "sessions")

#' Database file of a catalogued session
#' @keywords internal
#' @noRd
session_path <- function(main_db, code) {
  file.path(sessions_dir(main_db), sprintf("%s.sqlite", code))
}

#' Whether a session reference addresses the standard session
#' @keywords internal
#' @noRd
is_standard <- function(code) {
  length(code) == 1L && (is.na(code) || identical(code, ""))
}

#' Whether a value is a syntactically valid session code
#' @keywords internal
#' @noRd
is_session_code <- function(code) {
  is.character(code) && length(code) == 1L && !is.na(code) && grepl("^[2-9a-z]{6}$", code)
}

#' Participant address of a session
#' @param base_url Public participant URL of the standard session.
#' @param code Session codes; missing or empty values address the standard session.
#' @return Character vector of URLs.
#' @keywords internal
#' @noRd
session_url <- function(base_url, code) {
  separator <- if (grepl("?", base_url, fixed = TRUE)) "&" else "?"
  vapply(code, function(x) {
    if (is_standard(x)) base_url else paste0(base_url, separator, "s=", x)
  }, character(1), USE.NAMES = FALSE)
}

#' Create the catalog table on first use
#' @keywords internal
#' @noRd
catalog_ensure <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS catalog (
      code TEXT PRIMARY KEY,
      label TEXT NOT NULL,
      created_at REAL NOT NULL,
      archived_at REAL,
      prefill_yaml TEXT
    )")
}

#' Read the catalog in creation order without creating it
#' @keywords internal
#' @noRd
catalog_rows <- function(main_db) {
  con <- db(main_db)
  on.exit(dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "catalog")) {
    return(data.frame(code = character(), label = character(), created_at = numeric(),
                      archived_at = numeric(), prefill_yaml = character()))
  }
  dbGetQuery(con, "SELECT * FROM catalog ORDER BY created_at, rowid")
}

#' One catalog row, or NULL for unknown or malformed codes
#' @keywords internal
#' @noRd
catalog_session <- function(main_db, code) {
  if (!is_session_code(code)) return(NULL)
  rows <- catalog_rows(main_db)
  row <- rows[rows$code == code, , drop = FALSE]
  if (nrow(row)) row else NULL
}

#' Resolve a session reference to its file and catalog metadata
#' @keywords internal
#' @noRd
session_target <- function(main_db, code) {
  if (is_standard(code)) {
    return(list(path = main_db, label = "Standard-Session", archived = FALSE))
  }
  row <- catalog_session(main_db, code)
  if (is.null(row)) return(NULL)
  list(path = session_path(main_db, code), label = row$label, archived = !is.na(row$archived_at))
}

#' Draw a code that is neither catalogued nor present on disk
#' @keywords internal
#' @noRd
unused_code <- function(main_db) {
  taken <- catalog_rows(main_db)$code
  repeat {
    code <- bw_session_code()
    if (!code %in% taken && !file.exists(session_path(main_db, code))) return(code)
  }
}

#' Register a session file in the catalog
#' @keywords internal
#' @noRd
catalog_insert <- function(main_db, code, label, archived_at = NA_real_, prefill = NULL) {
  transaction_db(main_db, function(con) {
    catalog_ensure(con)
    dbExecute(con, "
      INSERT INTO catalog (code, label, created_at, archived_at, prefill_yaml)
      VALUES (:c, :l, :t, :a, :y)",
      params = list(
        c = code, l = label, t = now(), a = archived_at,
        y = if (is.null(prefill)) NA_character_ else yaml::as.yaml(unclass(prefill))
      )
    )
  })
}

#' Create a further session in setup
#' @param main_db Main database path.
#' @param label Optional display name; defaults to the creation time.
#' @param prefill Optional validated settings offered for review in setup.
#' @return The new session code.
#' @keywords internal
#' @noRd
create_session <- function(main_db, label = NULL, prefill = NULL) {
  label <- substr(trimws(label %||% ""), 1L, 80L)
  if (!nzchar(label)) label <- paste("Session", format(Sys.time(), "%d.%m.%Y %H:%M"))
  dir.create(sessions_dir(main_db), showWarnings = FALSE, recursive = TRUE)
  code <- unused_code(main_db)
  init_db(session_path(main_db, code))
  catalog_insert(main_db, code, label, prefill = prefill)
  code
}

#' Session row stored in a session file
#' @keywords internal
#' @noRd
session_state <- function(path) {
  con <- db(path)
  on.exit(dbDisconnect(con), add = TRUE)
  get_session(con)
}

#' Lifecycle status stored in a session file
#' @keywords internal
#' @noRd
session_status <- function(path) session_state(path)$status

#' Offer the settings of a configured session as a new session
#' @return The new code, or NULL when the source is unknown or still in setup.
#' @keywords internal
#' @noRd
rerun_session <- function(main_db, code) {
  source <- session_target(main_db, code)
  if (is.null(source) || !file.exists(source$path) || session_status(source$path) == "setup") {
    return(NULL)
  }
  label <- paste(sub(" \\(Wiederholung\\)$", "", source$label), "(Wiederholung)")
  create_session(main_db, label, prefill = get_settings(source$path))
}

#' Archive a finished session
#'
#' Archived sessions refuse participants and keep results for review and export.
#' The standard session is copied into a new archived session and then reset,
#' so the bare address is free for the next activity.
#' @return NULL on success, otherwise a German message.
#' @keywords internal
#' @noRd
archive_session <- function(main_db, code) {
  target <- session_target(main_db, code)
  if (is.null(target) || !file.exists(target$path)) return("Session nicht gefunden.")
  state <- session_state(target$path)
  if (state$status != "finished") {
    return("Nur beendete Sessions k\u00f6nnen archiviert werden.")
  }
  if (identical(state$plenum, "open")) return("Bitte zuerst die Gewichtung beenden.")
  if (is_standard(code)) return(archive_standard(main_db))
  transaction_db(main_db, function(con) {
    dbExecute(con, "UPDATE catalog SET archived_at = COALESCE(archived_at, :t) WHERE code = :c",
              params = list(t = now(), c = code))
  })
  NULL
}

#' Move a finished standard session into the archive
#' @keywords internal
#' @noRd
archive_standard <- function(main_db) {
  dir.create(sessions_dir(main_db), showWarnings = FALSE, recursive = TRUE)
  code <- unused_code(main_db)
  path <- session_path(main_db, code)
  con <- db(main_db)
  dbExecute(con, "VACUUM INTO :p", params = list(p = path))
  dbDisconnect(con)
  copy <- db(path)
  dbExecute(copy, "DROP TABLE IF EXISTS catalog")
  dbDisconnect(copy)
  init_db(path)
  label <- paste("Standard-Session vom", format(Sys.time(), "%d.%m.%Y %H:%M"))
  catalog_insert(main_db, code, label, archived_at = now())
  reset_session(main_db)
  NULL
}

#' Return an archived session to the active list
#' @return NULL on success, otherwise a German message.
#' @keywords internal
#' @noRd
restore_session <- function(main_db, code) {
  if (is.null(catalog_session(main_db, code))) return("Session nicht gefunden.")
  transaction_db(main_db, function(con) {
    dbExecute(con, "UPDATE catalog SET archived_at = NULL WHERE code = :c", params = list(c = code))
  })
  NULL
}

#' Delete a session permanently; the standard session is reset instead
#' @return NULL on success, otherwise a German message.
#' @keywords internal
#' @noRd
delete_session <- function(main_db, code) {
  if (is_standard(code)) {
    reset_session(main_db, force = TRUE)
    return(NULL)
  }
  if (is.null(catalog_session(main_db, code))) return("Session nicht gefunden.")
  files <- paste0(session_path(main_db, code), c("", "-wal", "-shm"))
  unlink(files)
  if (any(file.exists(files))) return("Die Session-Datei konnte nicht gel\u00f6scht werden.")
  transaction_db(main_db, function(con) {
    dbExecute(con, "DELETE FROM catalog WHERE code = :c", params = list(c = code))
  })
  NULL
}

#' Live state of one session file for the overview
#' @keywords internal
#' @noRd
session_summary <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(status = "missing", mode = NA_character_, current_round = NA_integer_,
                      n_rounds = NA_integer_, participants = 0L, contributions = 0L,
                      topics = "", plenum = "none"))
  }
  con <- db(path)
  on.exit(dbDisconnect(con), add = TRUE)
  s <- get_session(con)
  data.frame(
    status = s$status, mode = s$mode, current_round = s$current_round, n_rounds = s$n_rounds,
    participants = dbGetQuery(con, "SELECT COUNT(*) AS n FROM participants")$n,
    contributions = sum(bw_has_text(dbGetQuery(con, "SELECT text FROM entries")$text)),
    topics = paste(dbGetQuery(con, "SELECT title FROM topics ORDER BY id")$title,
                   collapse = " \u00b7 "),
    plenum = s$plenum %||% "none"
  )
}

#' Overview of the standard session and all catalogued sessions
#' @param main_db Main database path.
#' @param base_url Public participant URL of the standard session.
#' @return Data frame with one row per session, standard session first.
#' @keywords internal
#' @noRd
list_sessions <- function(main_db, base_url) {
  rows <- catalog_rows(main_db)
  code <- c(NA_character_, rows$code)
  paths <- c(main_db, session_path(main_db, rows$code))
  state <- do.call(rbind, lapply(paths, session_summary))
  cbind(
    data.frame(code = code, label = c("Standard-Session", rows$label), standard = is.na(code)),
    state,
    data.frame(created_at = c(NA_real_, rows$created_at),
               archived = !is.na(c(NA_real_, rows$archived_at)),
               url = session_url(base_url, code))
  )
}

#' Bind one browser page to the session its address names
#'
#' @param cfg Validated runtime configuration; `db_path` is the main database.
#' @param query Parsed query string.
#' @return List with kind (session, sessions, missing or archived), mod, cfg
#'   for the addressed session and, for sessions in review, prefill settings.
#' @keywords internal
#' @noRd
resolve_route <- function(cfg, query) {
  main <- cfg$main_db %||% cfg$db_path
  mod <- identical(query$mod, "1")
  base <- utils::modifyList(cfg, list(
    main_db = main, code = NA_character_, label = "Standard-Session", archived = FALSE
  ))
  if (mod && identical(query$view, "sessions")) {
    return(list(kind = "sessions", mod = TRUE, cfg = base))
  }
  code <- trimws(query$s %||% "")
  if (!nzchar(code)) return(list(kind = "session", mod = mod, cfg = base, prefill = NULL))
  code <- tolower(code)
  row <- catalog_session(main, code)
  if (is.null(row) || !file.exists(session_path(main, code))) {
    return(list(kind = "missing", mod = mod, cfg = base))
  }
  archived <- !is.na(row$archived_at)
  if (archived && !mod) return(list(kind = "archived", mod = FALSE, cfg = base))
  prefill <- NULL
  if (!is.na(row$prefill_yaml)) {
    parsed <- validate_settings(yaml::yaml.load(row$prefill_yaml, eval.expr = FALSE))
    if (inherits(parsed, "bw_settings")) prefill <- parsed
  }
  list(kind = "session", mod = mod, prefill = prefill, cfg = utils::modifyList(base, list(
    db_path = session_path(main, code), base_url = session_url(cfg$base_url, code),
    code = code, label = row$label, archived = archived
  )))
}

#' Bring every catalogued session file to the current schema
#'
#' Missing files are skipped and never recreated; the overview reports them.
#' @param main_db Main database path.
#' @keywords internal
#' @noRd
migrate_sessions <- function(main_db) {
  for (code in catalog_rows(main_db)$code) {
    path <- session_path(main_db, code)
    if (file.exists(path)) init_db(path)
  }
  invisible(NULL)
}
