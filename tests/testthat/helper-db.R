new_db <- function(path, k = 3L, n = 0L) {
  init_db(path)
  con <- db(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "UPDATE session SET status = 'lobby', n_groups = ?, n_rounds = ?",
    params = list(k, k)
  )
  for (i in seq_len(k)) {
    DBI::dbExecute(con, "INSERT INTO topics (id, title, q1, q2) VALUES (?, ?, ?, ?)",
      params = list(i, paste("Thema", i), "Warum?", "Wie?")
    )
  }
  for (i in seq_len(n)) add_participant(path, paste("Person", i))
  path
}
read_table <- function(path, table) {
  con <- db(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, table)
}
exec_sql <- function(path, sql) {
  con <- db(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sql)
}
