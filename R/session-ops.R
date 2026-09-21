#' Maybe advance
#' @keywords internal
#' @noRd
maybe_advance <- function(db_path, force = FALSE) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  s <- get_session(con)
  if (!nrow(s) || s$status != "running") {
    return(invisible(NULL))
  }
  if (!force && (is.na(s$round_ends_at) || now() < s$round_ends_at)) {
    return(invisible(NULL))
  }
  if (s$current_round >= s$n_rounds) {
    dbExecute(con, "
      UPDATE session SET status = 'finished', round_ends_at = NULL
      WHERE id = 1 AND status = 'running' AND current_round = :r",
      params = list(r = s$current_round)
    )
  } else {
    dbExecute(con, "
      UPDATE session SET current_round = current_round + 1, round_ends_at = :e
      WHERE id = 1 AND status = 'running' AND current_round = :r",
      params = list(e = now() + s$round_secs, r = s$current_round)
    )
  }
  invisible(NULL)
}

#' Start session
#' @keywords internal
#' @noRd
start_session <- function(db_path) {
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (s$status != "lobby") {
      return("Die Session ist nicht in der Lobby.")
    }
    p <- dbGetQuery(con, "SELECT pid FROM participants ORDER BY joined_at")
    if (nrow(p) < s$n_groups) {
      return(paste0("Mindestens ", s$n_groups, " Teilnehmer noetig (aktuell ", nrow(p), ")."))
    }
    ord <- sample(p$pid)
    k <- s$n_groups
    for (i in seq_along(ord)) {
      dbExecute(con, "UPDATE participants SET grp = :g, idx = :x WHERE pid = :p",
        params = list(g = ((i - 1) %% k) + 1, x = ((i - 1) %/% k) + 1, p = ord[i])
      )
    }
    for (g in seq_len(k)) {
      n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM participants WHERE grp = :g",
        params = list(g = g)
      )$n
      dbExecute(con, "UPDATE topics SET n_sheets = :n WHERE id = :g",
        params = list(n = n, g = g)
      )
    }
    dbExecute(con, "
    UPDATE session SET status = 'running', current_round = 1, round_ends_at = :e
    WHERE id = 1", params = list(e = now() + s$round_secs))
    NULL
  })
}

#' Add participant
#' @keywords internal
#' @noRd
add_participant <- function(db_path, name) {
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    pid <- paste(sample(c(letters, 0:9), 14, replace = TRUE), collapse = "")
    if (s$status == "lobby") {
      dbExecute(con, "INSERT INTO participants (pid, name, joined_at) VALUES (:p, :n, :t)",
        params = list(p = pid, n = name, t = now())
      )
    } else if (s$status == "running") {
      g <- dbGetQuery(con, "
      SELECT grp, COUNT(*) AS n FROM participants
      WHERE grp IS NOT NULL GROUP BY grp ORDER BY n, grp")
      grp <- if (nrow(g)) g$grp[1] else 1
      ix <- dbGetQuery(con, "
      SELECT COALESCE(MAX(idx), 0) + 1 AS x FROM participants WHERE grp = :g",
        params = list(g = grp)
      )$x
      dbExecute(con, "
      INSERT INTO participants (pid, name, grp, idx, joined_at)
      VALUES (:p, :n, :g, :x, :t)",
        params = list(p = pid, n = name, g = grp, x = ix, t = now())
      )
    } else {
      return(NULL)
    }
    pid
  })
}


#' Validate and persist moderator setup atomically
#' @keywords internal
#' @noRd
configure_session <- function(db_path, k, rounds, seconds, topics) {
  valid <- function(x, low, high) {
    is.numeric(x) && length(x) == 1L && !is.na(x) &&
      is.finite(x) && x == floor(x) && x >= low && x <= high
  }
  if (!valid(k, 2, 6) || !valid(rounds, 1, 12) || !valid(seconds, 60, 1800)) {
    return("Bitte 2\u20136 Gruppen, 1\u201312 Runden und 60\u20131800 Sekunden waehlen.")
  }
  if (length(topics) != k || any(!vapply(topics, function(x) {
    all(vapply(x[c("t", "a", "b")], function(y) {
      is.character(y) && length(y) == 1L && !is.na(y) && nzchar(trimws(y))
    }, logical(1)))
  }, logical(1)))) {
    return("Bitte alle Titel und Fragen ausfuellen.")
  }
  transaction_db(db_path, function(con) {
    if (get_session(con)$status != "setup") {
      return("Die Session wurde bereits eingerichtet.")
    }
    dbExecute(con, "DELETE FROM topics")
    for (i in seq_len(k)) {
      dbExecute(con, "INSERT INTO topics (id, title, q1, q2) VALUES (:i, :t, :a, :b)",
        params = list(i = i, t = topics[[i]]$t, a = topics[[i]]$a, b = topics[[i]]$b)
      )
    }
    dbExecute(con, "
      UPDATE session SET n_groups = :g, n_rounds = :r, round_secs = :s, status = 'lobby'
      WHERE id = 1", params = list(g = k, r = rounds, s = seconds))
    NULL
  })
}

#' Wipe a finished session atomically
#' @keywords internal
#' @noRd
reset_session <- function(db_path) {
  transaction_db(db_path, function(con) {
    if (get_session(con)$status != "finished") {
      return(invisible(FALSE))
    }
    dbExecute(con, "DELETE FROM entries")
    dbExecute(con, "DELETE FROM participants")
    dbExecute(con, "DELETE FROM topics")
    dbExecute(con, "DELETE FROM sqlite_sequence WHERE name = 'entries'")
    dbExecute(con, "
      UPDATE session SET status = 'setup', current_round = 0, round_ends_at = NULL,
                         n_groups = 3, n_rounds = 3, round_secs = 300
      WHERE id = 1")
    invisible(TRUE)
  })
}
