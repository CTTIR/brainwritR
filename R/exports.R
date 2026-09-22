#' Capture a consistent database snapshot for portable exports
#' @param db_path SQLite database path.
#' @return Named list containing raw tables and provenance metadata.
#' @keywords internal
#' @noRd
collect_data <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  data <- DBI::dbWithTransaction(con, {
    tables <- c("session", "topics", "participants", "entries")
    if (DBI::dbExistsTable(con, "votes")) tables <- c(tables, "votes")
    stats::setNames(lapply(tables, function(table) DBI::dbReadTable(con, table)), tables)
  })
  data$generated_at <- Sys.time()
  data$package_version <- as.character(utils::packageVersion("brainwritR"))
  data$settings <- settings_from_tables(data$session, data$topics)
  data
}

#' Replace author identities consistently throughout an export snapshot
#'
#' Codes follow arrival order, with existing row order breaking ties. Free text
#' is retained verbatim; names voluntarily written inside contributions are not
#' detected or redacted by this author-field pseudonymization.
#' @param data Snapshot returned by collect_data().
#' @return Snapshot with author names and persistent identifiers replaced.
#' @keywords internal
#' @noRd
pseudonymize_data <- function(data) {
  p <- data$participants
  group <- identical(as.character(data$session$mode), "group_device")
  prefix <- if (group) "Gruppe-" else "TN-"
  order <- order(p$joined_at, seq_len(nrow(p)))
  codes <- character(nrow(p))
  codes[order] <- sprintf("%s%02d", prefix, seq_len(nrow(p)))
  mapped <- match(data$entries$pid, p$pid)
  data$entries$pid <- codes[mapped]
  if (!is.null(data$votes)) data$votes$pid <- codes[match(data$votes$pid, p$pid)]
  data$participants$name <- codes
  data$participants$pid <- codes
  if (!is.null(data$settings)) {
    # Only preparatory author fields are transformed; topics remain intact.
    for (field in c("participants", "groups")) {
      names <- data$settings[[field]]
      if (!length(names)) next
      matches <- match(names, p$name)
      replacement <- codes[matches]
      missing <- which(is.na(matches))
      replacement[missing] <- sprintf("%s%02d", prefix, nrow(p) + seq_along(missing))
      data$settings[[field]] <- replacement
    }
  }
  # Stored configuration mirrors the settings echo and must not retain names.
  if ("settings_yaml" %in% names(data$session)) {
    data$session$settings_yaml <- yaml::as.yaml(unclass(data$settings))
  }
  data
}

#' Produce the existing German long-table export from a snapshot
#' @param data Snapshot returned by collect_data().
#' @return Eight-column data frame in CSV column order.
#' @keywords internal
#' @noRd
export_snapshot_df <- function(data) {
  e <- data$entries
  t <- data$topics
  p <- data$participants
  e <- e[e$topic_id %in% t$id & e$pid %in% p$pid, , drop = FALSE]
  e <- e[order(e$topic_id, e$sheet, e$round, e$question, e$id), , drop = FALSE]
  topic <- match(e$topic_id, t$id)
  participant <- match(e$pid, p$pid)
  data.frame(
    Thema = t$title[topic], Bogen = e$sheet, Runde = e$round, Frage = e$question,
    Teilnehmer = p$name[participant], Beitrag = e$text, abgegeben = e$submitted,
    Zeit = export_localtime(e$updated_at), check.names = FALSE
  )
}

#' Produce the established Markdown protocol from a snapshot
#' @param data Snapshot returned by collect_data().
#' @return Markdown protocol as one string.
#' @keywords internal
#' @noRd
build_snapshot_md <- function(data) {
  t <- data$topics[order(data$topics$id), , drop = FALSE]
  e <- data$entries
  e <- e[order(e$topic_id, e$sheet, e$round, e$question, e$id), , drop = FALSE]
  names <- data$participants$name[match(e$pid, data$participants$pid)]
  out <- c("# Brainwriting 6-3-5 \u2014 Ergebnisse",
           paste0("_", format(data$generated_at, "%d.%m.%Y %H:%M"), "_"), "")
  for (i in seq_len(nrow(t))) {
    out <- c(out, paste0("## Thema ", t$id[i], ": ", t$title[i]),
             paste0("- **F1:** ", t$q1[i]), paste0("- **F2:** ", t$q2[i]), "")
    rows <- which(e$topic_id == t$id[i] & bw_has_text(e$text))
    for (sheet in sort(unique(e$sheet[rows]))) {
      out <- c(out, paste0("### Bogen ", sheet), "")
      for (j in rows[e$sheet[rows] == sheet]) {
        out <- c(out, paste0("- **R", e$round[j], " \u00b7 F", e$question[j],
                             " \u00b7 ", names[j], ":** ", e$text[j]))
      }
      out <- c(out, "")
    }
  }
  paste(c(out, weights_md(data$entries, data$topics, data$votes)), collapse = "\n")
}

#' Whether a snapshot holds any plenum weights
#' @keywords internal
#' @noRd
has_votes <- function(votes) {
  !is.null(votes) && nrow(votes) > 0L && any(votes$points > 0)
}

#' Ranked plenum weights for the Markdown protocol
#' @return Markdown lines, or nothing when no weights exist.
#' @keywords internal
#' @noRd
weights_md <- function(entries, topics, votes) {
  if (!has_votes(votes)) return(character())
  r <- plenum_results(entries, topics, votes)
  r <- r[r$points > 0, , drop = FALSE]
  out <- c("## Gewichtung im Plenum", "",
           "_Anteil an allen im Thema vergebenen Punkten; je Person standen 100 % bereit._", "")
  for (id in unique(r$topic_id)) {
    x <- r[r$topic_id == id, , drop = FALSE]
    out <- c(out, paste0("### Thema ", id, ": ", x$title[1]), "",
             paste0(x$rank, ". **", round(100 * x$share), " %** ", x$text,
                    " _(R", x$round, " \u00b7 F", x$question, " \u00b7 Bogen ", x$sheet, ")_"),
             "")
  }
  out
}

#' Ranked plenum weights with German column names
#' @param data Snapshot returned by collect_data().
#' @return One row per nonempty contribution, ranked within each topic.
#' @keywords internal
#' @noRd
export_weights_df <- function(data) {
  r <- plenum_results(data$entries, data$topics, data$votes)
  data.frame(
    Thema = r$title, Rang = r$rank, Beitrag = r$text, Frage = r$question, Bogen = r$sheet,
    Runde = r$round, Punkte = r$points, Anteil = round(r$share, 4),
    "Durchschnitt je Person" = round(r$mean, 1), "Unterst\u00fctzende" = r$supporters,
    check.names = FALSE
  )
}

#' Write the complete lossless analysis snapshot
#' @param data Snapshot, optionally pseudonymized.
#' @param file Destination filename.
#' @return File path, invisibly.
#' @keywords internal
#' @noRd
write_rds <- function(data, file) {
  saveRDS(data, file)
  invisible(file)
}

#' Write four German worksheets with shared descriptive summaries
#' @param data Snapshot, optionally pseudonymized.
#' @param file Destination filename.
#' @return File path, invisibly.
#' @keywords internal
#' @noRd
write_xlsx <- function(data, file) {
  workbook <- openxlsx::createWorkbook()
  tables <- list("Beitr\u00e4ge" = export_snapshot_df(data),
                 "Teilnehmer" = data$participants, "Themen" = data$topics)
  header <- openxlsx::createStyle(textDecoration = "bold")
  for (sheet in names(tables)) {
    openxlsx::addWorksheet(workbook, sheet)
    openxlsx::writeData(workbook, sheet, tables[[sheet]], headerStyle = header)
    openxlsx::freezePane(workbook, sheet, firstRow = TRUE)
    openxlsx::setColWidths(workbook, sheet, seq_len(ncol(tables[[sheet]])), "auto")
  }
  kpis <- bw_kpis(data$entries, data$topics)
  names(kpis) <- c("Beitr\u00e4ge", "\u00d8 W\u00f6rter", "Abgabequote",
                   "Verschiedene Begriffe")
  summary <- bw_topic_summary(data$entries, data$topics)
  summary$topic_id <- data$topics$title[match(summary$topic_id, data$topics$id)]
  names(summary) <- c("Thema", names(kpis), "Ankn\u00fcpfungsgrad")
  openxlsx::addWorksheet(workbook, "Kennzahlen")
  openxlsx::writeData(workbook, "Kennzahlen", kpis, headerStyle = header)
  openxlsx::writeData(workbook, "Kennzahlen", summary, startRow = 5, headerStyle = header)
  openxlsx::freezePane(workbook, "Kennzahlen", firstRow = TRUE)
  openxlsx::setColWidths(workbook, "Kennzahlen", seq_len(ncol(summary)), "auto")
  if (has_votes(data$votes)) {
    weights <- export_weights_df(data)
    openxlsx::addWorksheet(workbook, "Gewichtung")
    openxlsx::writeData(workbook, "Gewichtung", weights, headerStyle = header)
    openxlsx::addStyle(workbook, "Gewichtung", openxlsx::createStyle(numFmt = "0%"),
                       rows = seq_len(nrow(weights)) + 1L, cols = 8L)
    openxlsx::freezePane(workbook, "Gewichtung", firstRow = TRUE)
    openxlsx::setColWidths(workbook, "Gewichtung", seq_len(ncol(weights)), "auto")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

#' Preserve SQLite localtime conversion including millisecond rounding
#' @param epoch Epoch seconds, including missing values.
#' @return Local date-time strings with SQLite's precision semantics.
#' @keywords internal
#' @noRd
export_localtime <- function(epoch) {
  if (!length(epoch)) return(character())
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  query <- DBI::dbSendQuery(con, "SELECT datetime(:epoch, 'unixepoch', 'localtime') AS time")
  on.exit(DBI::dbClearResult(query), add = TRUE, after = FALSE)
  DBI::dbBind(query, list(epoch = epoch))
  DBI::dbFetch(query)$time
}

#' Defuse spreadsheet formulas in text cells for an optional CSV mode
#'
#' Formula triggers (=, +, -, @, tab, carriage return) at the start of a text
#' cell, and after an embedded comma, semicolon, tab or line break, are prefixed
#' with an apostrophe, following the OWASP guidance on CSV injection. Spreadsheets
#' that split on another separator (German Excel and LibreOffice use ';') or start
#' a new record at a line break therefore see no live formula either. The default
#' CSV stays raw for statistical use; RDS remains lossless.
#' @param data Data frame about to be written as CSV.
#' @return The data frame with defused character columns.
#' @keywords internal
#' @noRd
spreadsheet_safe <- function(data) {
  for (column in names(data)) {
    x <- data[[column]]
    if (!is.character(x)) next
    ok <- !is.na(x)
    x[ok] <- gsub("(?:^|(?<=[,;\t\r\n]))([=+@\t\r-])", "'\\1", x[ok], perl = TRUE)
    data[[column]] <- x
  }
  data
}
