#' Open the plenum weighting of a finished session
#'
#' Every author distributes 100 percent per topic across all nonempty
#' contributions of that topic, including their own. Reopening keeps earlier
#' weights; in hot-seat mode the shared device starts again with the first person.
#' @return NULL on success, otherwise a German message.
#' @keywords internal
#' @noRd
open_plenum <- function(db_path) {
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (s$status != "finished") {
      return("Die Gewichtung ist erst nach der Schreibphase m\u00f6glich.")
    }
    if (s$plenum == "open") return(NULL)
    if (!any(bw_has_text(dbGetQuery(con, "SELECT text FROM entries")$text))) {
      return("Es gibt noch keine Beitr\u00e4ge zum Gewichten.")
    }
    dbExecute(con, "UPDATE session SET plenum = 'open', plenum_turn = :t WHERE id = 1",
              params = list(t = if (s$mode == "hot_seat") 1L else 0L))
    NULL
  })
}

#' End the plenum weighting; weights are kept
#' @return NULL on success, otherwise a German message.
#' @keywords internal
#' @noRd
close_plenum <- function(db_path) {
  transaction_db(db_path, function(con) {
    if (get_session(con)$plenum != "open") return("Die Gewichtung l\u00e4uft nicht.")
    dbExecute(con, "UPDATE session SET plenum = 'closed' WHERE id = 1")
    NULL
  })
}

#' Contributions offered to one voter, anonymous and in a stable personal order
#'
#' The order is a hash of voter and contribution, so it differs between people,
#' stays the same across reloads and never uses R's random number generator.
#' @return Data frame with id, topic_id, question, text and the voter's points.
#' @keywords internal
#' @noRd
plenum_items <- function(db_path, pid) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  e <- dbGetQuery(con, "SELECT id, topic_id, question, text FROM entries")
  e <- e[bw_has_text(e$text), , drop = FALSE]
  v <- dbGetQuery(con, "SELECT entry_id, points FROM votes WHERE pid = :p",
                  params = list(p = pid %||% ""))
  e$points <- v$points[match(e$id, v$entry_id)]
  e$points[is.na(e$points)] <- 0L
  key <- as.character(openssl::md5(paste(pid %||% "", e$id, sep = ":")))
  e <- e[order(e$topic_id, key), c("id", "topic_id", "question", "text", "points"), drop = FALSE]
  rownames(e) <- NULL
  e
}

#' Store one weight within the voter's remaining budget for that topic
#'
#' Values are rounded to steps of five and limited to what is left of the
#' voter's 100 percent for the contribution's topic.
#' @return The stored points as integer, or NULL when the request is refused.
#' @keywords internal
#' @noRd
set_weight <- function(db_path, pid, entry_id, points) {
  if (!is.character(pid) || length(pid) != 1L || is.na(pid) ||
        !is.numeric(entry_id) || length(entry_id) != 1L || is.na(entry_id) ||
        !is.numeric(points) || length(points) != 1L || !is.finite(points)) {
    return(NULL)
  }
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (s$status != "finished" || s$plenum != "open") return(NULL)
    voter <- dbGetQuery(con, "SELECT pid FROM participants WHERE pid = :p", params = list(p = pid))
    entry <- dbGetQuery(con, "SELECT id, topic_id, text FROM entries WHERE id = :e",
                        params = list(e = entry_id))
    if (!nrow(voter) || !nrow(entry) || !bw_has_text(entry$text)) return(NULL)
    others <- dbGetQuery(con, "
      SELECT COALESCE(SUM(points), 0) AS n FROM votes
      WHERE pid = :p AND topic_id = :t AND entry_id <> :e",
      params = list(p = pid, t = entry$topic_id, e = entry$id)
    )$n
    value <- as.integer(min(max(round(points / 5) * 5, 0), 100 - others))
    if (value > 0L) {
      dbExecute(con, "
        INSERT INTO votes (pid, entry_id, topic_id, points, updated_at)
        VALUES (:p, :e, :t, :v, :u)
        ON CONFLICT (pid, entry_id) DO UPDATE SET points = :v, updated_at = :u",
        params = list(p = pid, e = entry$id, t = entry$topic_id, v = value, u = now())
      )
    } else {
      dbExecute(con, "DELETE FROM votes WHERE pid = :p AND entry_id = :e",
                params = list(p = pid, e = entry$id))
    }
    value
  })
}

#' Person whose turn it is on the shared hot-seat device
#' @return One participant row, or zero rows when nobody is due.
#' @keywords internal
#' @noRd
plenum_voter <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  s <- get_session(con)
  p <- dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at, rowid")
  if (s$mode != "hot_seat" || s$plenum != "open" || s$plenum_turn < 1L ||
        s$plenum_turn > nrow(p)) {
    return(p[FALSE, , drop = FALSE])
  }
  p[s$plenum_turn, , drop = FALSE]
}

#' Pass the shared device on exactly once per rendered turn
#' @keywords internal
#' @noRd
advance_plenum <- function(db_path, expected) {
  transaction_db(db_path, function(con) {
    dbExecute(con, "
      UPDATE session SET plenum_turn = plenum_turn + 1
      WHERE id = 1 AND mode = 'hot_seat' AND plenum = 'open' AND plenum_turn = :t
        AND plenum_turn <= (SELECT COUNT(*) FROM participants)",
      params = list(t = expected)
    )
  })
  invisible(NULL)
}

#' Aggregate plenum weights per contribution
#'
#' Points are summed over voters. The share is a contribution's part of all
#' points given in its topic; the mean is points per person who weighted that
#' topic. Contributions are ranked within their topic, ties sharing a rank.
#' @param entries Raw entries table.
#' @param topics Raw topics table.
#' @param votes Raw votes table or NULL.
#' @return Data frame with one row per nonempty contribution.
#' @keywords internal
#' @noRd
plenum_results <- function(entries, topics, votes) {
  entries <- entries[bw_has_text(entries$text), , drop = FALSE]
  if (is.null(votes)) votes <- data.frame(pid = character(), entry_id = integer(),
                                          topic_id = integer(), points = integer())
  votes <- votes[votes$points > 0, , drop = FALSE]
  sum_by <- function(key, values) vapply(key, function(k) sum(values[[1]][values[[2]] == k]), 0)
  count_by <- function(key, ids, groups) {
    vapply(key, function(k) length(unique(ids[groups == k])), 0)
  }
  points <- sum_by(entries$id, list(votes$points, votes$entry_id))
  total <- sum_by(entries$topic_id, list(votes$points, votes$topic_id))
  voters <- count_by(entries$topic_id, votes$pid, votes$topic_id)
  out <- data.frame(
    topic_id = entries$topic_id, title = topics$title[match(entries$topic_id, topics$id)],
    entry_id = entries$id, sheet = entries$sheet, round = entries$round,
    question = entries$question, text = entries$text, points = points,
    share = ifelse(total > 0, points / total, NA_real_),
    mean = ifelse(voters > 0, points / voters, NA_real_),
    supporters = count_by(entries$id, votes$pid, votes$entry_id),
    rank = stats::ave(-points, entries$topic_id, FUN = function(x) rank(x, ties.method = "min"))
  )
  out <- out[order(out$topic_id, out$rank, out$entry_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Weighting progress per topic
#' @return Data frame with topic_id, title, complete (full budget used),
#'   voters (any weight) and eligible authors.
#' @keywords internal
#' @noRd
plenum_progress <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  topics <- dbGetQuery(con, "SELECT id, title FROM topics ORDER BY id")
  eligible <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM participants")$n
  sums <- dbGetQuery(con, "
    SELECT topic_id, pid, SUM(points) AS total FROM votes GROUP BY topic_id, pid")
  data.frame(
    topic_id = topics$id, title = topics$title,
    complete = vapply(topics$id, function(t) sum(sums$topic_id == t & sums$total >= 100), 0),
    voters = vapply(topics$id, function(t) sum(sums$topic_id == t & sums$total > 0), 0),
    eligible = rep(eligible, nrow(topics))
  )
}
