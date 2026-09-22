#' Display label of a session lifecycle state
#' @keywords internal
#' @noRd
status_label <- function(status, archived = FALSE, plenum = "none") {
  if (isTRUE(archived)) return("Archiviert")
  if (identical(status, "finished") && identical(plenum, "open")) return("Gewichtung l\u00e4uft")
  labels <- c(setup = "Einrichtung", lobby = "Lobby", running = "L\u00e4uft",
              finished = "Beendet", missing = "Datei fehlt")
  unname(labels[status]) %||% status
}

#' Display label of a device mode
#' @keywords internal
#' @noRd
mode_label <- function(mode) {
  labels <- c(individual = "Alle am eigenen Ger\u00e4t", group_device = "Ein Ger\u00e4t pro Gruppe",
              hot_seat = "Reihum an einem Ger\u00e4t")
  if (is.na(mode) || !mode %in% names(labels)) return(NULL)
  labels[[mode]]
}

#' Button that reports one overview action for one session
#' @keywords internal
#' @noRd
session_action_button <- function(label, action, code, class = "btn-outline-secondary",
                                  disabled = FALSE) {
  tags$button(
    type = "button", class = paste("btn btn-sm", class),
    disabled = if (disabled) "disabled" else NULL,
    onclick = sprintf(paste0(
      "Shiny.setInputValue('session_action', ",
      "{action: '%s', code: '%s', nonce: Date.now()}, {priority: 'event'})"
    ), action, code),
    label
  )
}

#' One overview card
#' @param row One row of list_sessions().
#' @keywords internal
#' @noRd
session_card <- function(row) {
  code <- if (row$standard) "" else row$code
  open <- if (row$standard) "?mod=1" else paste0("?mod=1&s=", row$code)
  round <- if (row$status == "running") {
    sprintf(if (identical(row$mode, "hot_seat")) "Durchgang %s von %s" else "Runde %s von %s",
            row$current_round, row$n_rounds)
  }
  created <- if (!is.na(row$created_at)) {
    paste("Erstellt:", format(as.POSIXct(row$created_at, origin = "1970-01-01"), "%d.%m.%Y %H:%M"))
  }
  facts <- c(mode_label(row$mode), round, paste(row$participants, "Teilnehmer"),
             contributions_text(row$contributions))
  div(
    class = "bw-card bw-session",
    div(
      class = "d-flex justify-content-between align-items-start gap-2",
      div(
        if (row$standard) strong(row$label) else strong(translate = "no", row$label),
        " ",
        if (row$standard) {
          tags$span(class = "badge text-bg-light", "Basisadresse")
        } else {
          tags$code(translate = "no", row$code)
        }
      ),
      tags$span(class = "badge text-bg-secondary",
                status_label(row$status, row$archived, row$plenum))
    ),
    div(class = "bw-status", lapply(facts, function(x) tags$span(class = "me-2", x))),
    if (nzchar(row$topics)) div(class = "bw-status", translate = "no", row$topics),
    if (!is.null(created)) div(class = "bw-status", created),
    div(class = "my-2", tags$code(translate = "no", row$url)),
    div(
      class = "d-flex gap-2 flex-wrap",
      tags$a(class = "btn btn-sm btn-primary", href = open, "\u00d6ffnen"),
      session_action_button("QR-Code", "qr", code),
      session_action_button("Neu starten", "rerun", code,
                            disabled = row$status %in% c("setup", "missing")),
      if (row$archived) {
        session_action_button("Wiederherstellen", "restore", code)
      } else {
        session_action_button("Archivieren", "archive", code,
                              disabled = row$status != "finished")
      },
      session_action_button(if (row$standard) "Zur\u00fccksetzen" else "L\u00f6schen",
                            "delete", code, class = "btn-outline-danger ms-auto")
    )
  )
}

#' Moderator overview of all sessions
#'
#' Lists the standard session and every catalogued session with its live state,
#' address and QR code, and offers creating, restarting, archiving, restoring
#' and deleting. Every action is authorized on the server.
#' @keywords internal
#' @noRd
sessions_server <- function(input, output, session, cfg, is_mod) {
  main <- cfg$main_db
  refresh <- reactiveVal(0L)
  pending <- reactiveVal(NULL)
  qr_target <- reactiveVal(NULL)

  changes <- reactivePoll(cfg$poll_ms, session,
    checkFunc = function() {
      if (!isolate(is_mod())) return("")
      state <- list_sessions(main, cfg$base_url)
      paste(do.call(paste, c(state, sep = "\t")), collapse = "\n")
    },
    valueFunc = function() NULL
  )
  rows <- reactive({
    req(is_mod())
    changes()
    refresh()
    list_sessions(main, cfg$base_url)
  })
  updated <- function() refresh(refresh() + 1L)
  navigate <- function(code) session$sendCustomMessage("bw_navigate", paste0("?mod=1&s=", code))
  report <- function(result) {
    if (!is.null(result)) showNotification(result, type = "warning")
    updated()
  }

  output$page <- renderUI({
    if (!is_mod()) return(mod_login_ui())
    tagList(
      div(
        class = "bw-card",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          h3(class = "mb-0", "Sessions"),
          actionButton("mod_logout", "Abmelden", class = "btn-outline-secondary")
        ),
        p(class = "bw-status mt-2",
          "Jede Session hat eine eigene Adresse und einen eigenen QR-Code."),
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          actionButton("session_new", "Neue Session", class = "btn-primary"),
          shiny::radioButtons("sessions_filter", NULL, inline = TRUE,
                              choices = c("Aktiv" = "active", "Archiv" = "archived"))
        )
      ),
      uiOutput("sessions_list")
    )
  })

  output$sessions_list <- renderUI({
    state <- rows()
    archived <- identical(input$sessions_filter, "archived")
    state <- state[state$archived == archived, , drop = FALSE]
    # Standard session first, then the newest sessions.
    state <- state[order(!state$standard, -ifelse(is.na(state$created_at), 0, state$created_at)), ,
                   drop = FALSE]
    if (!nrow(state)) return(div(class = "bw-card bw-status", "Keine Sessions."))
    lapply(seq_len(nrow(state)), function(i) session_card(state[i, , drop = FALSE]))
  })

  observeEvent(input$session_new, {
    req(is_mod())
    showModal(modalDialog(
      title = "Neue Session",
      textInput("session_label", "Bezeichnung (optional)", width = "100%"),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("session_create", "Erstellen", class = "btn-primary")
      )
    ))
  })
  observeEvent(input$session_create, {
    req(is_mod())
    code <- create_session(main, input$session_label)
    removeModal()
    updated()
    navigate(code)
  })

  confirm <- function(action, code, title, body, button) {
    pending(list(action = action, code = code))
    showModal(modalDialog(
      title = title, body,
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("session_confirm", button, class = "btn-danger")
      )
    ))
  }

  observeEvent(input$session_action, {
    req(is_mod())
    request <- input$session_action
    action <- request$action
    code <- request$code %||% ""
    req(is.character(action), length(action) == 1L, is.character(code), length(code) == 1L)
    target <- session_target(main, code)
    if (is.null(target)) {
      showNotification("Session nicht gefunden.", type = "warning")
      return()
    }
    switch(action,
      qr = {
        qr_target(list(code = code, url = session_url(cfg$base_url, code)))
        showModal(modalDialog(
          title = "QR-Code", easyClose = TRUE,
          plotOutput("session_qr", height = "320px"),
          tags$code(translate = "no", session_url(cfg$base_url, code)),
          footer = tagList(
            downloadButton("session_qr_png", "QR-Code herunterladen (PNG)"),
            modalButton("Schlie\u00dfen")
          )
        ))
      },
      rerun = {
        again <- rerun_session(main, code)
        if (is.null(again)) {
          showNotification("Diese Session ist noch nicht eingerichtet.", type = "warning")
        } else {
          updated()
          navigate(again)
        }
      },
      archive = if (is_standard(code)) {
        confirm("archive", code, "Standard-Session archivieren?", paste(
          "Die Ergebnisse werden als archivierte Session gespeichert.",
          "Danach ist die Standard-Session f\u00fcr die n\u00e4chste Aktivit\u00e4t leer."
        ), "Archivieren")
      } else {
        report(archive_session(main, code))
      },
      restore = report(restore_session(main, code)),
      delete = if (is_standard(code)) {
        confirm("delete", code, "Standard-Session zur\u00fccksetzen?",
                "Alle Teilnehmer, Boegen und Beitraege werden geloescht. Vorher exportieren!",
                "Ja, alles loeschen")
      } else {
        confirm("delete", code, "Session l\u00f6schen?", tagList(
          p(translate = "no", strong(target$label)),
          p(paste("Die Session wird mit allen Teilnehmern und Beitr\u00e4gen",
                  "endg\u00fcltig gel\u00f6scht. Vorher exportieren!"))
        ), "Endg\u00fcltig l\u00f6schen")
      }
    )
  })

  observeEvent(input$session_confirm, {
    req(is_mod(), !is.null(pending()))
    request <- pending()
    pending(NULL)
    removeModal()
    report(switch(request$action,
      archive = archive_session(main, request$code),
      delete = delete_session(main, request$code)
    ))
  })

  draw_qr <- function(url) {
    op <- graphics::par(mar = c(0, 0, 0, 0))
    on.exit(graphics::par(op), add = TRUE)
    plot(qr_code(url))
  }
  output$session_qr <- renderPlot({
    req(is_mod(), qr_target())
    draw_qr(qr_target()$url)
  }, height = 320)
  output$session_qr_png <- downloadHandler(
    filename = function() {
      paste0("brainwritR-qr-", if (nzchar(qr_target()$code)) qr_target()$code else "standard",
             ".png")
    },
    content = function(file) {
      req(is_mod(), qr_target())
      grDevices::png(file, width = 1200, height = 1200)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_qr(qr_target()$url)
    }
  )
  invisible(NULL)
}
