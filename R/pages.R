#' Query string of the connecting page, read once at connection
#' @keywords internal
#' @noRd
request_query <- function(session) {
  shiny::parseQueryString(isolate(session$clientData$url_search) %||% "")
}

#' Moderator PIN prompt
#' @keywords internal
#' @noRd
mod_login_ui <- function() {
  div(
    class = "bw-card",
    h2("Moderator"),
    passwordInput("pin", "PIN", width = "100%"),
    actionButton("pin_btn", "Anmelden", class = "btn-primary w-100")
  )
}

#' Moderator bar naming the session and linking to the overview
#' @keywords internal
#' @noRd
mod_nav_ui <- function(cfg) {
  name <- if (is_standard(cfg$code)) {
    strong("Standard-Session")
  } else {
    tagList(strong(translate = "no", cfg$label), " ", tags$code(translate = "no", cfg$code))
  }
  div(
    class = "bw-card bw-modbar",
    div(class = "bw-modbar-name", name),
    div(
      class = "d-flex gap-2 flex-wrap",
      tags$a(class = "btn btn-outline-primary", href = "?mod=1&view=sessions", "Alle Sessions"),
      actionButton("mod_logout", "Abmelden", class = "btn-outline-secondary")
    )
  )
}

#' Notice for addresses that do not name an open session
#' @keywords internal
#' @noRd
notice_ui <- function(route) {
  archived <- identical(route$kind, "archived")
  div(
    class = "bw-card",
    h2(if (archived) "Session archiviert" else "Session nicht gefunden"),
    p(if (archived) {
      "Diese Session ist abgeschlossen. Ein Beitritt ist nicht mehr m\u00f6glich."
    } else {
      "Bitte QR-Code oder Link pr\u00fcfen."
    }),
    div(
      class = "d-flex gap-2 flex-wrap",
      tags$a(class = "btn btn-outline-primary", href = "?", "Zur Startseite"),
      if (isTRUE(route$mod)) {
        tags$a(class = "btn btn-outline-secondary", href = "?mod=1&view=sessions", "Alle Sessions")
      }
    )
  )
}

#' Serve a notice page
#' @keywords internal
#' @noRd
notice_server <- function(output, route) {
  output$page <- shiny::renderUI(notice_ui(route))
  invisible(NULL)
}

#' Setup form values, optionally taken from settings offered for review
#' @param prefill Validated settings or NULL.
#' @return List of initial form values.
#' @keywords internal
#' @noRd
setup_defaults <- function(prefill = NULL) {
  if (!inherits(prefill, "bw_settings")) {
    return(list(mode = "individual", k = 3L, rounds = 3L, round_secs = 300L,
                turn_secs = 90L, roster = "", groups = "", planned = 15L))
  }
  generic <- paste("Gruppe", seq_along(prefill$topics))
  list(
    mode = prefill$mode, k = length(prefill$topics), rounds = prefill$rounds,
    round_secs = prefill$round_secs, turn_secs = prefill$turn_secs,
    roster = paste(prefill$participants, collapse = "\n"),
    planned = prefill$expected_participants %||% 15L,
    # Generic names are regenerated; only custom names need to be carried over.
    groups = if (identical(prefill$groups, generic)) "" else paste(prefill$groups, collapse = "\n")
  )
}

#' Label of the names field: optional unless the names set the hot-seat order
#' @keywords internal
#' @noRd
roster_label <- function(mode) {
  if (identical(mode, "hot_seat")) {
    "Namen (ein Name pro Zeile)"
  } else {
    "Namen (optional, ein Name pro Zeile)"
  }
}
