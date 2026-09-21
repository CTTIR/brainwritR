#' Maybe advance
#' @keywords internal
#' @noRd
maybe_advance <- function(db_path, force = FALSE, expected = NULL) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  s <- get_session(con)
  if (!nrow(s) || s$status != "running" || !matches_turn(s, expected)) {
    return(invisible(NULL))
  }
  if (!force && (is.na(s$round_ends_at) || now() < s$round_ends_at)) {
    return(invisible(NULL))
  }
  if (s$mode == "hot_seat") {
    n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM participants")$n
    turn <- s$current_turn + 1L
    round <- s$current_round
    if (turn > n) {
      turn <- 1L
      round <- round + 1L
    }
    dbExecute(con, "
      UPDATE session SET current_turn = :t, current_round = :next_round,
                         round_ends_at = NULL, status = :status
      WHERE id = 1 AND status = 'running' AND current_round = :r AND current_turn = :old_t",
      params = list(
        t = turn, next_round = min(round, s$n_rounds),
        status = if (round > s$n_rounds) "finished" else "running",
        r = s$current_round, old_t = s$current_turn
      )
    )
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
start_session <- function(db_path, force = FALSE) {
  transaction_db(db_path, function(con) start_on_connection(con, force))
}

#' Start using the setup transaction connection
#' @keywords internal
#' @noRd
start_on_connection <- function(con, force = FALSE) {
  s <- get_session(con)
  if (s$status != "lobby") {
    return("Die Session ist nicht in der Lobby.")
  }
  p <- dbGetQuery(con, "SELECT pid FROM participants ORDER BY joined_at")
  if (s$mode == "group_device") {
    if (nrow(p) < s$n_groups && !force) {
      return("Bitte alle Gruppen verbinden oder ohne alle Gruppen starten.")
    }
    dbExecute(con, "UPDATE topics SET n_sheets = 1")
    dbExecute(con, "UPDATE session SET status = 'running', current_round = 1,
                     round_ends_at = :e WHERE id = 1",
      params = list(e = now() + s$round_secs)
    )
    return(NULL)
  }
  if (s$mode == "hot_seat" && nrow(p) == 0L) {
    return("Bitte mindestens einen Namen eingeben.")
  }
  if (s$mode == "individual" && nrow(p) < s$n_groups) {
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
    if (s$mode == "hot_seat") n <- max(1L, n)
    dbExecute(con, "UPDATE topics SET n_sheets = :n WHERE id = :g",
      params = list(n = n, g = g)
    )
  }
  dbExecute(con, "
    UPDATE session SET status = 'running', current_round = 1, round_ends_at = :e
    WHERE id = 1", params = list(e = now() + s$round_secs))
  if (s$mode == "hot_seat") {
    dbExecute(con, "UPDATE session SET current_turn = 1, round_ends_at = NULL WHERE id = 1")
  }
  NULL
}

#' Add participant
#' @keywords internal
#' @noRd
add_participant <- function(db_path, name) {
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (s$mode == "group_device" || !is.character(name) || length(name) != 1L ||
          is.na(name) || !nzchar(trimws(name))) {
      return(NULL)
    }
    name <- trimws(name)
    pid <- paste(sample(c(letters, 0:9), 14, replace = TRUE), collapse = "")
    if (s$status == "lobby") {
      dbExecute(con, "INSERT INTO participants (pid, name, joined_at) VALUES (:p, :n, :t)",
        params = list(p = pid, n = name, t = now())
      )
    } else if (s$status == "running") {
      g <- dbGetQuery(con, "
      SELECT grp, COUNT(*) AS n FROM participants
      WHERE grp IS NOT NULL GROUP BY grp ORDER BY n, grp")
      sizes <- integer(s$n_groups)
      sizes[g$grp] <- g$n
      grp <- which.min(sizes)
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
configure_session <- function(db_path, k, rounds, seconds, topics, settings = NULL) {
  if (is.null(settings)) {
    settings <- list(
      format = "brainwriting635-settings/1", mode = "individual", rounds = rounds,
      round_secs = seconds, turn_secs = 90L,
      topics = lapply(topics, function(x) list(title = x$t, q1 = x$a, q2 = x$b))
    )
  }
  settings <- validate_settings(settings)
  if (is.character(settings)) {
    return(paste0("Bitte Einstellungen korrigieren:\n", paste(settings, collapse = "\n")))
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k != length(settings$topics)) {
    return("Die Anzahl der Gruppen muss der Themenanzahl entsprechen.")
  }
  if (settings$mode == "hot_seat" && !length(settings$participants)) {
    return("Bitte mindestens einen Namen eingeben.")
  }
  transaction_db(db_path, function(con) {
    if (get_session(con)$status != "setup") {
      return("Die Session wurde bereits eingerichtet.")
    }
    dbExecute(con, "DELETE FROM topics")
    for (i in seq_len(k)) {
      topic <- settings$topics[[i]]
      dbExecute(con, "INSERT INTO topics (id, title, q1, q2) VALUES (:i, :t, :a, :b)",
                params = list(i = i, t = topic$title, a = topic$q1, b = topic$q2))
    }
    dbExecute(con, "
      UPDATE session SET n_groups = :g, n_rounds = :r, round_secs = :s,
        status = 'lobby', mode = :m, settings_yaml = :y, current_turn = 0
      WHERE id = 1",
              params = list(g = k, r = settings$rounds, s = settings$round_secs,
                            m = settings$mode, y = yaml::as.yaml(unclass(settings))))
    if (settings$mode == "group_device") {
      dbExecute(con, "UPDATE topics SET n_sheets = 1")
    }
    if (settings$mode == "hot_seat") {
      timestamp <- now()
      for (i in seq_along(settings$participants)) {
        joined <- timestamp - (length(settings$participants) - i) / 1000
        pid <- paste(sample(c(letters, 0:9), 14, replace = TRUE), collapse = "")
        dbExecute(con, "INSERT INTO participants (pid, name, joined_at) VALUES (:p, :n, :t)",
                  params = list(p = pid, n = settings$participants[i], t = joined))
      }
      return(start_on_connection(con))
    }
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
                         n_groups = 3, n_rounds = 3, round_secs = 300,
                         mode = 'individual', current_turn = 0, settings_yaml = NULL
      WHERE id = 1")
    invisible(TRUE)
  })
}

#' Arm a handover exactly once
#' @keywords internal
#' @noRd
arm_turn <- function(db_path, expected = NULL) {
  seconds <- get_settings(db_path)$turn_secs
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (!matches_turn(s, expected)) return(invisible(0L))
    dbExecute(con, "UPDATE session SET round_ends_at = :e
      WHERE id = 1 AND status = 'running' AND mode = 'hot_seat'
      AND round_ends_at IS NULL AND current_round = :r AND current_turn = :t",
      params = list(e = now() + seconds, r = s$current_round, t = s$current_turn)
    )
  })
}

#' Read the current kiosk author in stable arrival order
#' @keywords internal
#' @noRd
current_author <- function(db_path) {
  con <- db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  s <- get_session(con)
  p <- dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at, rowid")
  if (s$status != "running" || s$mode != "hot_seat" || s$current_turn < 1L ||
        s$current_turn > nrow(p)) {
    return(p[FALSE, , drop = FALSE])
  }
  p[s$current_turn, , drop = FALSE]
}

#' Add time only to an active clock
#' @keywords internal
#' @noRd
extend_clock <- function(db_path, seconds = 60) {
  stopifnot(is.numeric(seconds), length(seconds) == 1L, is.finite(seconds), seconds > 0)
  transaction_db(db_path, function(con) {
    dbExecute(con, "UPDATE session SET round_ends_at = round_ends_at + :s
      WHERE id = 1 AND status = 'running' AND round_ends_at IS NOT NULL",
      params = list(s = seconds)
    )
  })
}

#' Finish the remainder of the current kiosk pass
#' @keywords internal
#' @noRd
end_pass <- function(db_path, expected = NULL) {
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (s$status != "running" || s$mode != "hot_seat" || !matches_turn(s, expected)) {
      return(invisible(FALSE))
    }
    dbExecute(con, "UPDATE session SET current_turn = 1, round_ends_at = NULL,
      current_round = :next_round, status = :status
      WHERE id = 1 AND status = 'running' AND current_round = :r AND current_turn = :t",
      params = list(
        next_round = min(s$current_round + 1L, s$n_rounds),
        status = if (s$current_round >= s$n_rounds) "finished" else "running",
        r = s$current_round, t = s$current_turn
      )
    )
  })
}

#' Finish early while retaining all contributions
#' @keywords internal
#' @noRd
abort_session <- function(db_path) {
  transaction_db(db_path, function(con) {
    dbExecute(con, "UPDATE session SET status = 'finished', round_ends_at = NULL
                   WHERE id = 1 AND status = 'running'")
  })
}

#' Claim a group's persistent author identity
#' @keywords internal
#' @noRd
claim_group <- function(db_path, grp) {
  settings <- get_settings(db_path)
  transaction_db(db_path, function(con) {
    s <- get_session(con)
    if (s$mode != "group_device" || !s$status %in% c("lobby", "running") ||
          !is.numeric(grp) || length(grp) != 1L || is.na(grp) ||
          grp != floor(grp) || grp < 1L || grp > s$n_groups) {
      return(NULL)
    }
    existing <- dbGetQuery(con, "SELECT pid FROM participants WHERE grp = :g",
      params = list(g = grp)
    )
    if (nrow(existing)) {
      return(existing$pid[1])
    }
    pid <- paste(sample(c(letters, 0:9), 14, replace = TRUE), collapse = "")
    dbExecute(con, "INSERT INTO participants (pid, name, grp, idx, joined_at)
      VALUES (:p, :n, :g, 1, :t)",
      params = list(p = pid, n = settings$groups[grp], g = grp, t = now())
    )
    pid
  })
}

#' Reject stale actions from a previously rendered round or turn
#' @keywords internal
#' @noRd
matches_turn <- function(current, expected) {
  if (is.null(expected)) return(TRUE)
  isTRUE(current$current_round == expected$current_round) &&
    isTRUE(current$current_turn == expected$current_turn)
}
