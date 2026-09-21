#' Upsert a draft without clearing submission
#' @keywords internal
#' @noRd
save_entry <- function(db_path, pid, topic_id, sheet, round, question, text, submitted = NULL) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  participant <- dbGetQuery(con, "SELECT pid FROM participants WHERE pid = :p",
                            params = list(p = pid))
  if (!nrow(participant)) return(invisible(NULL))
  dbExecute(con, "
    INSERT INTO entries (topic_id, sheet, round, question, pid, text, submitted, updated_at)
    VALUES (:t, :s, :r, :q, :p, :x, COALESCE(:sub, 0), :u)
    ON CONFLICT (topic_id, sheet, round, question, pid)
    DO UPDATE SET text = :x, updated_at = :u, submitted = COALESCE(:sub, submitted)",
    params = list(
      t = topic_id, s = sheet, r = round, q = question, p = pid,
      x = text %||% "", u = now(),
      sub = if (is.null(submitted)) NA_integer_ else as.integer(submitted)
    )
  )
}
#' Export df
#' @keywords internal
#' @noRd
export_df <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, "
    SELECT t.title AS Thema, e.sheet AS Bogen, e.round AS Runde,
           e.question AS Frage, p.name AS Teilnehmer, e.text AS Beitrag,
           e.submitted AS abgegeben,
           datetime(e.updated_at, 'unixepoch', 'localtime') AS Zeit
    FROM entries e
    JOIN topics t ON t.id = e.topic_id
    JOIN participants p ON p.pid = e.pid
    ORDER BY t.id, e.sheet, e.round, e.question")
}

#' Build md
#' @keywords internal
#' @noRd
build_md <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  tp <- dbGetQuery(con, "SELECT * FROM topics ORDER BY id")
  en <- dbGetQuery(con, "
    SELECT e.*, p.name FROM entries e
    JOIN participants p ON p.pid = e.pid
    ORDER BY e.topic_id, e.sheet, e.round, e.question")
  out <- c(
    "# Brainwriting 6-3-5 \u2014 Ergebnisse",
    paste0("_", format(Sys.time(), "%d.%m.%Y %H:%M"), "_"), ""
  )
  for (i in seq_len(nrow(tp))) {
    out <- c(
      out,
      paste0("## Thema ", tp$id[i], ": ", tp$title[i]),
      paste0("- **F1:** ", tp$q1[i]),
      paste0("- **F2:** ", tp$q2[i]), ""
    )
    sub <- en[en$topic_id == tp$id[i] & nzchar(en$text), ]
    for (sh in sort(unique(sub$sheet))) {
      out <- c(out, paste0("### Bogen ", sh), "")
      ss <- sub[sub$sheet == sh, ]
      for (j in seq_len(nrow(ss))) {
        out <- c(out, paste0(
          "- **R", ss$round[j], " \u00b7 F", ss$question[j],
          " \u00b7 ", ss$name[j], ":** ", ss$text[j]
        ))
      }
      out <- c(out, "")
    }
  }
  paste(out, collapse = "\n")
}
