#' Open a configured database connection
#'
#' Only `init_db()` may create a file. A deleted session therefore fails loudly
#' instead of being recreated empty by a lingering poll or delayed draft.
#' @keywords internal
#' @noRd
db <- function(db_path, create = FALSE) {
  flags <- if (create) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RW
  con <- dbConnect(SQLite(), db_path, flags = flags)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

#' Initialize the persistent schema
#' @keywords internal
#' @noRd
init_db <- function(db_path) {
  con <- db(db_path, create = TRUE)
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "PRAGMA journal_mode = WAL")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS session (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      status TEXT NOT NULL DEFAULT 'setup',      -- setup|lobby|running|finished
      n_groups INTEGER NOT NULL DEFAULT 3,
      n_rounds INTEGER NOT NULL DEFAULT 3,
      round_secs INTEGER NOT NULL DEFAULT 300,
      current_round INTEGER NOT NULL DEFAULT 0,
      round_ends_at REAL
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS topics (
      id INTEGER PRIMARY KEY,
      title TEXT NOT NULL,
      q1 TEXT NOT NULL,
      q2 TEXT NOT NULL,
      n_sheets INTEGER
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS participants (
      pid TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      grp INTEGER,
      idx INTEGER,
      joined_at REAL NOT NULL
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS entries (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      topic_id INTEGER NOT NULL,
      sheet INTEGER NOT NULL,
      round INTEGER NOT NULL,
      question INTEGER NOT NULL,
      pid TEXT NOT NULL,
      text TEXT NOT NULL DEFAULT '',
      submitted INTEGER NOT NULL DEFAULT 0,
      updated_at REAL,
      UNIQUE (topic_id, sheet, round, question, pid)
    )")
  columns <- dbGetQuery(con, "PRAGMA table_info(session)")$name
  if (!"mode" %in% columns) {
    dbExecute(con, "ALTER TABLE session ADD COLUMN mode TEXT NOT NULL DEFAULT 'individual'")
  }
  if (!"current_turn" %in% columns) {
    dbExecute(con, "ALTER TABLE session ADD COLUMN current_turn INTEGER NOT NULL DEFAULT 0")
  }
  if (!"settings_yaml" %in% columns) {
    dbExecute(con, "ALTER TABLE session ADD COLUMN settings_yaml TEXT")
  }
  if (nrow(dbGetQuery(con, "SELECT id FROM session")) == 0) {
    dbExecute(con, "INSERT INTO session (id) VALUES (1)")
  }
}

#' Current epoch seconds
#' @keywords internal
#' @noRd
now <- function() as.numeric(Sys.time())
#' Read the singleton session
#' @keywords internal
#' @noRd
get_session <- function(con) dbGetQuery(con, "SELECT * FROM session WHERE id = 1")

#' Execute a write transaction with one connection
#' @keywords internal
#' @noRd
transaction_db <- function(db_path, operation) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  dbWithTransaction(con, operation(con))
}

#' Reconstruct configuration from the persistent session
#' @keywords internal
#' @noRd
get_settings <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  settings_from_tables(get_session(con),
                       dbGetQuery(con, "SELECT title, q1, q2 FROM topics ORDER BY id"))
}

#' Reconstruct settings from a single consistent database snapshot
#' @keywords internal
#' @noRd
settings_from_tables <- function(s, topics) {
  topics <- topics[, c("title", "q1", "q2"), drop = FALSE]
  if (length(s$settings_yaml) == 1L && !is.na(s$settings_yaml) && nzchar(s$settings_yaml)) {
    parsed <- yaml::yaml.load(s$settings_yaml, eval.expr = FALSE)
    validated <- validate_settings(parsed)
    if (inherits(validated, "bw_settings")) return(validated)
    stop(paste(validated, collapse = "\n"), call. = FALSE)
  }
  structure(list(
    format = "brainwriting635-settings/1", mode = s$mode %||% "individual",
    rounds = s$n_rounds,
    round_secs = s$round_secs, turn_secs = 90L,
    topics = lapply(seq_len(nrow(topics)), function(i) as.list(topics[i, ])),
    groups = paste("Gruppe", seq_len(s$n_groups)), participants = character()
  ), class = "bw_settings")
}
