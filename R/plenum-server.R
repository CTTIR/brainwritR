#' Plenum weighting for one connection
#'
#' Participant weighting on one's own device or a passed-on shared device,
#' synchronisation of open voting pages, and the moderator's Analyse & Plenum
#' tab with progress, chart, ranking and CSV download.
#' @param cfg Configuration of the connected session.
#' @param ctx The connection's shared reactives and helpers: is_mod, my_pid,
#'   tick_meta, tick_all, part_state, plenum_armed, session_alive,
#'   is_archived_now, analytics_data, compact and download_data.
#' @return List with `part_ui(m, pid)` building the participant view.
#' @keywords internal
#' @noRd
plenum_server <- function(input, output, session, cfg, ctx) {
  is_mod <- ctx$is_mod
  my_pid <- ctx$my_pid
  tick_meta <- ctx$tick_meta
  tick_all <- ctx$tick_all
  part_state <- ctx$part_state
  plenum_armed <- ctx$plenum_armed
  session_alive <- ctx$session_alive
  is_archived_now <- ctx$is_archived_now
  analytics_data <- ctx$analytics_data
  compact <- ctx$compact
  download_data <- ctx$download_data

  plenum_part_ui <- function(m, pid) {
    s <- m$s
    if (s$plenum == "closed") {
      return(div(
        class = "bw-card", h3("Danke!"),
        p(paste("Die Gewichtung ist abgeschlossen.",
                "Die Ergebnisse besprechen wir jetzt im Plenum."))
      ))
    }
    if (s$mode == "hot_seat") {
      turn <- s$plenum_turn
      if (turn > nrow(m$participants)) {
        return(div(
          class = "bw-card text-center", h3("Alle haben gewichtet."),
          p("Die Moderation beendet die Gewichtung.")
        ))
      }
      voter <- m$participants[turn, ]
      if (!identical(isolate(plenum_armed()), turn)) {
        return(div(
          class = "bw-card text-center",
          h2("Weitergeben an: ", tags$span(translate = "no", voter$name)),
          p(class = "bw-status", "Gewichtung im Plenum"),
          actionButton("plenum_start", "Los geht's", class = "btn-primary w-100 btn-lg"),
          actionButton("plenum_skip", "\u00dcberspringen", class = "btn-outline-secondary mt-3")
        ))
      }
      pid <- voter$pid
    }
    weights_ui(plenum_items(cfg$db_path, pid), m$topics, handover = s$mode == "hot_seat")
  }

  # Who may weigh from this page right now: the device's author, or on a
  # shared device the person whose turn has been started.
  plenum_voter_pid <- function() {
    s <- session_state(cfg$db_path)
    if (s$mode != "hot_seat") return(my_pid())
    if (!identical(plenum_armed(), s$plenum_turn)) return(NULL)
    plenum_voter(cfg$db_path)$pid[1]
  }
  observeEvent(input$weight, {
    w <- input$weight
    req(is.list(w), session_alive())
    voter <- plenum_voter_pid()
    req(!is.null(voter), !is.na(voter))
    points <- suppressWarnings(as.numeric(w$points))
    stored <- set_weight(cfg$db_path, voter, suppressWarnings(as.numeric(w$entry)), points)
    if (!is.null(stored) && !identical(as.numeric(stored), points)) {
      session$sendCustomMessage("bw_weight", list(entry = w$entry, points = stored))
    }
  })
  # Keep every open voting page of a voter in line with the stored weights,
  # e.g. a second tab or a group device that was taken over.
  last_weights <- NULL
  observe({
    st <- tick_all()
    req(st$s$status == "finished", st$s$plenum == "open")
    voter <- isolate(plenum_voter_pid())
    req(!is.null(voter), !is.na(voter))
    mine <- st$votes[st$votes$pid == voter, c("entry_id", "points"), drop = FALSE]
    key <- paste(mine$entry_id, mine$points, sep = ":", collapse = ",")
    if (identical(key, last_weights)) return()
    last_weights <<- key
    session$sendCustomMessage("bw_weights", list(
      entries = as.list(mine$entry_id), points = as.list(mine$points)
    ))
  })
  # A started turn belongs to one stored turn of an open weighting; a moderator
  # skip, closing or reopening hands the device back to the handover screen.
  observe({
    s <- tick_meta()$s
    armed <- isolate(plenum_armed())
    if (!is.null(armed) && (s$plenum != "open" || !identical(armed, s$plenum_turn))) {
      plenum_armed(NULL)
    }
  })
  # Only the person shown on the handover screen can be started.
  observeEvent(input$plenum_start, {
    shown <- isolate(part_state())$m$s
    s <- session_state(cfg$db_path)
    req(s$mode == "hot_seat", s$plenum == "open", identical(shown$plenum_turn, s$plenum_turn))
    plenum_armed(s$plenum_turn)
  })
  observeEvent(input$plenum_done, {
    turn <- plenum_armed()
    req(!is.null(turn))
    advance_plenum(cfg$db_path, turn)
    plenum_armed(NULL)
  })
  observeEvent(input$plenum_skip, {
    turn <- isolate(part_state())$m$s$plenum_turn
    req(is.null(plenum_armed()), !is.null(turn))
    advance_plenum(cfg$db_path, turn)
  })

  observeEvent(input$plenum_open, {
    req(is_mod(), !is_archived_now())
    err <- open_plenum(cfg$db_path)
    if (!is.null(err)) showNotification(err, type = "warning")
  })
  observeEvent(input$plenum_reopen, {
    req(is_mod(), !is_archived_now())
    err <- open_plenum(cfg$db_path)
    if (!is.null(err)) showNotification(err, type = "warning")
  })
  observeEvent(input$plenum_close, {
    req(is_mod())
    close_plenum(cfg$db_path)
  })
  observeEvent(input$plenum_skip_mod, {
    req(is_mod(), tick_meta()$s$mode == "hot_seat")
    advance_plenum(cfg$db_path, tick_meta()$s$plenum_turn)
  })

  output$plenum_view <- renderUI({
    req(is_mod())
    st <- tick_all()
    s <- st$s
    req(s$status == "finished")
    archived <- is_archived_now()
    controls <- switch(s$plenum,
      none = if (!any(bw_has_text(st$entries$text))) {
        p(class = "bw-status", "Es gibt noch keine Beitr\u00e4ge zum Gewichten.")
      } else if (!archived) {
        actionButton("plenum_open", "Gewichtung starten", class = "btn-primary")
      },
      open = tagList(
        p(class = "bw-status", "Gewichtung l\u00e4uft."),
        plenum_progress_ui(plenum_progress(cfg$db_path)),
        if (s$mode == "hot_seat") {
          voter <- st$participants$name[s$plenum_turn]
          div(
            class = "my-2",
            if (length(voter) && !is.na(voter)) p(paste0("Aktuell: ", voter)),
            actionButton("plenum_skip_mod", "\u00dcberspringen", class = "btn-outline-secondary")
          )
        },
        actionButton("plenum_close", "Gewichtung beenden", class = "btn-primary mt-2"),
        p(class = "bw-status mt-2", "Ergebnisse erscheinen nach dem Beenden.")
      ),
      closed = tagList(
        p(class = "bw-status", "Gewichtung beendet."),
        if (!archived) {
          actionButton("plenum_reopen", "Wieder \u00f6ffnen", class = "btn-outline-secondary")
        }
      )
    )
    voters <- length(unique(st$votes$pid[st$votes$points > 0]))
    tagList(
      div(
        class = "bw-card",
        h3("Gewichtung im Plenum"),
        p(class = "bw-status", paste(
          "Alle verteilen je Thema 100 % auf die anonym gezeigten Beitr\u00e4ge,",
          "auch auf eigene."
        )),
        controls
      ),
      if (s$plenum == "closed" && voters == 0L) {
        div(class = "bw-card bw-status", "Es wurden keine Gewichte vergeben.")
      } else if (s$plenum == "closed") {
        tagList(
          div(
            class = "bw-card",
            strong(plenum_count_text(voters)),
            plotOutput("fig_weights", height = "auto"),
            div(
              class = "d-flex gap-2 flex-wrap align-items-center mt-2",
              downloadButton("dl_weights", "Gewichtung (CSV)"),
              tags$span(class = "bw-status", paste(
                "Die Gewichtung ist auch in XLSX, RDS, Markdown",
                "und im PDF-Bericht enthalten."
              ))
            )
          ),
          plenum_results_ui(plenum_results(st$entries, st$topics, st$votes), st$topics)
        )
      }
    )
  })
  weights_results <- reactive({
    st <- analytics_data()
    plenum_results(st$entries, st$topics, st$votes)
  })
  output$fig_weights <- renderPlot({
    bw_plot_language(bw_plot_weights(weights_results(), analytics_data()$topics,
                                     compact = compact("fig_weights"), numbered = TRUE),
                     input$ui_language %||% "de")
  }, height = function() {
    bw_weights_height(weights_results(), compact = compact("fig_weights"))
  }, res = 96)
  output$dl_weights <- downloadHandler(
    contentType = "text/csv; charset=UTF-8",
    filename = function() {
      paste0("brainwriting_gewichtung_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      data <- export_weights_df(download_data())
      if (isTRUE(input$csv_safe)) data <- spreadsheet_safe(data)
      write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  shiny::outputOptions(output, "dl_weights", suspendWhenHidden = FALSE)

  list(part_ui = plenum_part_ui)
}
