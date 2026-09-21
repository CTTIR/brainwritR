#' Open a configured database connection
#' @keywords internal
#' @noRd
db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

#' Initialize the persistent schema
#' @keywords internal
#' @noRd
init_db <- function(db_path) {
  con <- db(db_path)
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
