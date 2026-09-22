#' Process-wide moderator login state
#'
#' Tokens live only in server memory: a restart logs every moderator out.
#' Wrong PINs are counted across all connections because a reload opens a new
#' connection; logged-in moderators keep working while new logins wait.
#' @param ttl Token lifetime in seconds.
#' @return An environment shared by all connections of one app.
#' @keywords internal
#' @noRd
moderator_auth <- function(ttl = 12 * 3600) {
  auth <- new.env(parent = emptyenv())
  auth$ttl <- ttl
  auth$tokens <- numeric()
  auth$failures <- 0L
  auth$last_failure <- -Inf
  auth$locked_until <- -Inf
  auth
}

#' Check a PIN attempt against the login throttle
#'
#' From the fifth consecutive failure, logins pause for 15 seconds, doubling
#' with every further failure up to five minutes. Attempts during a pause are
#' refused without being compared; failures older than 15 minutes are forgotten.
#' @return List with ok, token (on success) and wait in seconds.
#' @keywords internal
#' @noRd
auth_try_pin <- function(auth, pin, expected, time = now()) {
  if (time < auth$locked_until) {
    return(list(ok = FALSE, wait = ceiling(auth$locked_until - time)))
  }
  if (is.character(pin) && length(pin) == 1L && identical(pin, expected)) {
    auth$failures <- 0L
    token <- bw_token()
    live <- auth$tokens[auth$tokens > time]
    auth$tokens <- c(live, stats::setNames(time + auth$ttl, token))
    return(list(ok = TRUE, token = token, wait = 0))
  }
  if (time - auth$last_failure > 900) auth$failures <- 0L
  auth$failures <- auth$failures + 1L
  auth$last_failure <- time
  wait <- 0
  if (auth$failures >= 5L) {
    wait <- min(300, 15 * 2^(auth$failures - 5L))
    auth$locked_until <- time + wait
  }
  list(ok = FALSE, wait = wait)
}

#' Whether a token belongs to a current moderator login
#' @keywords internal
#' @noRd
auth_check_token <- function(auth, token, time = now()) {
  is.character(token) && length(token) == 1L && !is.na(token) && nzchar(token) &&
    isTRUE(auth$tokens[token] > time)
}

#' End a moderator login
#' @keywords internal
#' @noRd
auth_revoke <- function(auth, token) {
  if (is.character(token) && length(token) == 1L) {
    auth$tokens <- auth$tokens[names(auth$tokens) != token]
  }
  invisible(NULL)
}

#' Moderator login for one connection
#'
#' Accepts the PIN or a token the tab stored after an earlier login, so
#' navigating between sessions and reconnecting do not ask for the PIN again.
#' The token lifetime is also the session lifetime: an open connection is
#' signed out within a minute of its login expiring or being revoked.
#' @return A reactive value that is TRUE while the connection is authorized.
#' @keywords internal
#' @noRd
moderator_login <- function(input, session, auth, pin) {
  is_mod <- reactiveVal(FALSE)
  current <- NULL
  observeEvent(input$pin_btn, {
    attempt <- auth_try_pin(auth, input$pin, pin)
    if (attempt$ok) {
      current <<- attempt$token
      is_mod(TRUE)
      session$sendCustomMessage("bw_store_mod_token", attempt$token)
    } else if (attempt$wait > 0) {
      showNotification(sprintf(
        "Zu viele Fehlversuche. Bitte in %d s erneut versuchen.", as.integer(attempt$wait)
      ), type = "error")
    } else {
      showNotification("Falsche PIN.", type = "error")
    }
  })
  observeEvent(input$mod_token, {
    if (auth_check_token(auth, input$mod_token)) {
      current <<- input$mod_token
      is_mod(TRUE)
    }
  })
  # The login lifetime also bounds an open connection: revalidate every minute.
  observe({
    invalidateLater(60000)
    if (isTRUE(isolate(is_mod())) && !auth_check_token(auth, current)) {
      current <<- NULL
      is_mod(FALSE)
      session$sendCustomMessage("bw_clear_mod_token", "")
      showNotification("Die Anmeldung ist abgelaufen. Bitte PIN erneut eingeben.",
                       type = "warning", duration = NULL)
    }
  })
  observeEvent(input$mod_logout, {
    auth_revoke(auth, current)
    current <<- NULL
    is_mod(FALSE)
    session$sendCustomMessage("bw_clear_mod_token", "")
  })
  is_mod
}
