#' Build a server bound to immutable configuration
#' @keywords internal
#' @noRd
app_server <- function(cfg) {
  force(cfg)
  function(input, output, session) {
    is_mod <- reactiveVal(FALSE)
    my_pid <- reactiveVal(NULL)
    part_state <- reactiveVal(NULL)
    mod_state <- reactiveVal(NULL)
    submitted_flag <- reactiveVal(NULL)
    reset_pending <- reactiveVal(FALSE)

    tick_meta <- reactivePoll(cfg$poll_ms, session,
      checkFunc = function() {
        maybe_advance(cfg$db_path)
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        dbGetQuery(con, "
        SELECT status || '|' || current_round || '|' || COALESCE(round_ends_at, 0) || '|' ||
               (SELECT COUNT(*) FROM participants) || '|' ||
               (SELECT COALESCE(MAX(joined_at), 0) FROM participants) AS v
        FROM session")$v
      },
      valueFunc = function() {
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        list(
          s            = get_session(con),
          topics       = dbGetQuery(con, "SELECT * FROM topics ORDER BY id"),
          participants = dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at")
        )
      }
    )

    tick_all <- reactivePoll(cfg$poll_ms, session,
      checkFunc = function() {
        maybe_advance(cfg$db_path)
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        dbGetQuery(con, "
        SELECT (SELECT status || current_round || COALESCE(round_ends_at, 0) FROM session)
               || '|' ||
               (SELECT COUNT(*) || '-' || COALESCE(MAX(updated_at), 0) || '-' ||
                       COALESCE(SUM(submitted), 0) FROM entries)
               || '|' ||
               (SELECT COUNT(*) FROM participants) AS v")$v
      },
      valueFunc = function() {
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        list(
          s            = get_session(con),
          topics       = dbGetQuery(con, "SELECT * FROM topics ORDER BY id"),
          participants = dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at"),
          entries      = dbGetQuery(con, "SELECT * FROM entries")
        )
      }
    )

    observeEvent(input$stored_pid, ignoreNULL = FALSE, {
      pid <- input$stored_pid
      if (is.null(pid) || identical(pid, "")) {
        return()
      }
      con <- db(cfg$db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      hit <- dbGetQuery(con, "SELECT pid FROM participants WHERE pid = :p",
        params = list(p = pid)
      )
      if (nrow(hit) == 1) {
        my_pid(pid)
      } else {
        my_pid(NULL)
        session$sendCustomMessage("bw_clear_pid", "")
      }
    })

    observe({
      m <- tick_meta()
      pid <- my_pid()
      if (!is.null(pid) && !(pid %in% m$participants$pid)) {
        # A new join can precede the next poll; validate against the database,
        # never clear a just-created identity from an older cached snapshot.
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        m$participants <- dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at")
        if (!(pid %in% m$participants$pid)) {
          my_pid(NULL)
          session$sendCustomMessage("bw_clear_pid", "")
          pid <- NULL
        }
      }
      grp <- NA
      idx <- NA
      if (!is.null(pid)) {
        me <- m$participants[m$participants$pid == pid, ]
        if (nrow(me)) {
          grp <- me$grp
          idx <- me$idx
        }
      }
      key <- paste(m$s$status, m$s$current_round, pid %||% "-", grp, idx, sep = "|")
      old <- isolate(part_state())
      if (is.null(old) || !identical(old$key, key)) {
        submitted_flag(NULL)
        part_state(list(key = key, m = m, pid = pid))
      }
    })

    observe({
      m <- tick_meta()
      key <- paste(m$s$status, m$s$current_round, sep = "|")
      old <- isolate(mod_state())
      if (is.null(old) || !identical(old$key, key)) {
        mod_state(list(key = key, s = m$s))
      }
    })

    edit_ctx <- reactive({
      m <- tick_meta()
      pid <- my_pid()
      if (is.null(pid) || m$s$status != "running") {
        return(NULL)
      }
      me <- m$participants[m$participants$pid == pid, ]
      if (!nrow(me) || is.na(me$grp)) {
        return(NULL)
      }
      t_id <- topic_for(me$grp, m$s$current_round, m$s$n_groups)
      ns <- m$topics$n_sheets[m$topics$id == t_id]
      if (!length(ns) || is.na(ns)) {
        return(NULL)
      }
      list(topic_id = t_id, sheet = sheet_for(me$idx, ns), round = m$s$current_round)
    })


    output$page <- renderUI({
      qs <- parseQueryString(session$clientData$url_search)
      if (identical(qs$mod, "1")) {
        if (!is_mod()) {
          return(div(
            class = "bw-card",
            h2("Moderator"),
            passwordInput("pin", "PIN", width = "100%"),
            actionButton("pin_btn", "Anmelden", class = "btn-primary w-100")
          ))
        }
        return(uiOutput("mod_view"))
      }
      uiOutput("part_view")
    })

    observeEvent(input$pin_btn, {
      if (identical(input$pin, cfg$mod_pin)) {
        is_mod(TRUE)
      } else {
        showNotification("Falsche PIN.", type = "error")
      }
    })


    observeEvent(input$join_btn, {
      if (!is.null(my_pid())) {
        return()
      }
      nm <- trimws(input$join_name %||% "")
      if (!nzchar(nm)) nm <- paste0("TN-", sample(100:999, 1))
      pid <- add_participant(cfg$db_path, nm)
      if (is.null(pid)) {
        showNotification("Beitritt derzeit nicht moeglich.", type = "warning")
        return()
      }
      my_pid(pid)
      session$sendCustomMessage("bw_store_pid", pid)
    })

    output$part_view <- renderUI({
      ps <- part_state()
      if (is.null(ps)) {
        return(NULL)
      }
      m <- ps$m
      s <- m$s
      pid <- ps$pid

      if (is.null(pid)) {
        if (s$status == "finished") {
          return(div(
            class = "bw-card", h2("Session beendet"),
            p("Ein Beitritt ist nicht mehr moeglich.")
          ))
        }
        if (s$status == "setup") {
          return(div(
            class = "bw-card",
            h2("Brainwriting 6-3-5"),
            p("Die Session wird gerade vorbereitet \u2014 Seite einfach offen lassen.")
          ))
        }
        return(div(
          class = "bw-card",
          h2("Brainwriting 6-3-5"),
          p(class = "bw-status", "Mit Name oder Pseudonym beitreten:"),
          textInput("join_name", "Name / Pseudonym",
                    placeholder = "Name / Pseudonym", width = "100%"),
          actionButton("join_btn", "Teilnehmen", class = "btn-primary w-100 btn-lg")
        ))
      }

      me <- m$participants[m$participants$pid == pid, ]

      if (s$status %in% c("setup", "lobby")) {
        return(div(
          class = "bw-card",
          h3(paste0("Hallo ", me$name, "!")),
          p("Du bist dabei. Warten auf den Start \u2026"),
          uiOutput("lobby_count")
        ))
      }
      if (s$status == "finished") {
        return(div(
          class = "bw-card",
          h3("Geschafft \u2014 danke!"),
          p("Alle Beitraege sind gesichert. Die Ergebnisse besprechen wir jetzt gemeinsam.")
        ))
      }
      if (is.na(me$grp)) {
        return(div(class = "bw-card",
                   p("Einen Moment \u2014 du wirst einer Gruppe zugeteilt \u2026")))
      }

      t_id <- topic_for(me$grp, s$current_round, s$n_groups)
      tp <- m$topics[m$topics$id == t_id, ]
      sh <- sheet_for(me$idx, tp$n_sheets)
      ent <- isolate(tick_all())$entries
      mine1 <- ent$text[ent$pid == pid & ent$round == s$current_round & ent$question == 1]
      mine2 <- ent$text[ent$pid == pid & ent$round == s$current_round & ent$question == 2]

      tagList(
        div(
          class = "bw-card",
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              div(
                class = "bw-status",
                paste0(
                  "Runde ", s$current_round, "/", s$n_rounds,
                  " \u00b7 Gruppe ", me$grp, " \u00b7 Bogen ", sh
                )
              ),
              h3(class = "mb-0", tp$title)
            ),
            uiOutput("timer")
          )
        ),
        div(
          class = "bw-card",
          tags$label(class = "bw-q", `for` = "a1", paste0("Frage 1 \u2014 ", tp$q1)),
          uiOutput("prev1"),
          textAreaInput("a1", NULL,
            value = if (length(mine1)) mine1[1] else "",
            placeholder = "Aufgreifen, ergaenzen, weiterentwickeln \u2026",
            width = "100%"
          ),
          tags$label(class = "bw-q", `for` = "a2", paste0("Frage 2 \u2014 ", tp$q2)),
          uiOutput("prev2"),
          textAreaInput("a2", NULL,
            value = if (length(mine2)) mine2[1] else "",
            placeholder = "Aufgreifen, ergaenzen, weiterentwickeln \u2026",
            width = "100%"
          ),
          uiOutput("submit_area"),
          p(class = "bw-status mt-2", "Speichert automatisch beim Tippen.")
        )
      )
    })

    output$lobby_count <- renderUI({
      m <- tick_meta()
      p(class = "bw-status", paste0(nrow(m$participants), " Teilnehmer im Raum"))
    })

    prev_block <- function(qn) {
      ctx <- edit_ctx()
      if (is.null(ctx)) {
        return(NULL)
      }
      st <- tick_all()
      pn <- stats::setNames(st$participants$name, st$participants$pid)
      e <- st$entries
      rows <- e[e$topic_id == ctx$topic_id & e$sheet == ctx$sheet &
                  e$round < ctx$round & e$question == qn & nzchar(e$text), ]
      if (!nrow(rows)) {
        return(p(class = "bw-status",
                 "Noch keine Vorbeitraege \u2014 dieser Bogen beginnt bei dir."))
      }
      rows <- rows[order(rows$round), ]
      lapply(seq_len(nrow(rows)), function(i) {
        div(
          class = "bw-prev",
          div(class = "who", paste0("Runde ", rows$round[i], " \u00b7 ", pn[[rows$pid[i]]])),
          div(rows$text[i])
        )
      })
    }
    output$prev1 <- renderUI(prev_block(1))
    output$prev2 <- renderUI(prev_block(2))

    draft <- function(value) {
      list(text = value, ctx = isolate(edit_ctx()), pid = isolate(my_pid()))
    }
    a1_d <- debounce(reactive(draft(input$a1)), 1200)
    a2_d <- debounce(reactive(draft(input$a2)), 1200)
    save_draft <- function(value, question) {
      ctx <- value$ctx
      if (is.null(ctx) || is.null(value$text) || is.null(value$pid)) {
        return()
      }
      save_entry(
        cfg$db_path, value$pid, ctx$topic_id, ctx$sheet,
        ctx$round, question, value$text
      )
    }
    observeEvent(a1_d(), ignoreInit = TRUE, {
      save_draft(a1_d(), 1L)
    })
    observeEvent(a2_d(), ignoreInit = TRUE, {
      save_draft(a2_d(), 2L)
    })

    observeEvent(input$submit_btn, {
      ctx <- edit_ctx()
      if (is.null(ctx)) {
        return()
      }
      save_entry(cfg$db_path, my_pid(), ctx$topic_id, ctx$sheet, ctx$round,
                 1L, input$a1 %||% "", submitted = 1L)
      save_entry(cfg$db_path, my_pid(), ctx$topic_id, ctx$sheet, ctx$round,
                 2L, input$a2 %||% "", submitted = 1L)
      submitted_flag(as.character(ctx$round))
      showNotification("Abgegeben \u2713", type = "message", duration = 2)
    })

    output$submit_area <- renderUI({
      ctx <- edit_ctx()
      if (is.null(ctx)) {
        return(NULL)
      }
      st <- tick_all()
      pid <- my_pid() %||% ""
      done_db <- nrow(st$entries) > 0 &&
        any(st$entries$pid == pid & st$entries$round == ctx$round & st$entries$submitted == 1)
      done <- done_db || identical(submitted_flag(), as.character(ctx$round))
      if (done) {
        div(
          class = "text-center bw-status py-2",
          "\u2713 Abgegeben \u2014 du kannst bis Rundenende weiter aendern."
        )
      } else {
        actionButton("submit_btn", "Abgeben", class = "btn-primary w-100 btn-lg")
      }
    })


    output$timer <- renderUI({
      invalidateLater(1000)
      m <- tick_meta()
      s <- m$s
      if (s$status != "running" || is.na(s$round_ends_at)) {
        return(NULL)
      }
      secs <- max(0, floor(s$round_ends_at - now()))
      cls <- if (secs < 60) "bw-timer low" else "bw-timer"
      div(class = cls, sprintf("%d:%02d", secs %/% 60, secs %% 60))
    })


    output$mod_view <- renderUI({
      req(is_mod())
      ms <- mod_state()
      if (is.null(ms)) {
        return(NULL)
      }
      s <- ms$s
      switch(s$status,
        setup    = mod_setup_ui(),
        lobby    = mod_lobby_ui(),
        running  = mod_running_ui(s),
        finished = mod_finished_ui()
      )
    })

    mod_setup_ui <- function() {
      tagList(
        div(
          class = "bw-card",
          h3("Session einrichten"),
          div(
            class = "row g-2",
            div(
              class = "col-12 col-sm-4",
              numericInput("n_groups", "Themen = Gruppen", 3, min = 2, max = 6, width = "100%")
            ),
            div(
              class = "col-12 col-sm-4",
              numericInput("n_rounds", "Runden", 3, min = 1, max = 12, width = "100%")
            ),
            div(
              class = "col-12 col-sm-4",
              numericInput("round_secs", "Sek./Runde", 300,
                min = 60, max = 1800,
                step = 30, width = "100%"
              )
            )
          ),
          p(
            class = "bw-status",
            paste0("Preset 15-2-5: 3 Themen, 3 Runden, 300 s. ",
                   "Nach K Runden hat jeder jedes Thema 1\u00d7 bearbeitet.")
          )
        ),
        uiOutput("topic_form"),
        actionButton("setup_save", "Speichern und Lobby oeffnen",
          class = "btn-primary w-100 btn-lg"
        )
      )
    }

    output$topic_form <- renderUI({
      req(is_mod())
      k <- suppressWarnings(as.integer(input$n_groups %||% 3))
      if (is.na(k) || k < 2) k <- 3
      if (k > 6) k <- 6
      lapply(seq_len(k), function(k) {
        tt <- isolate(input[[paste0("t_title_", k)]]) %||% ""
        qa <- isolate(input[[paste0("t_q1_", k)]]) %||% ""
        qb <- isolate(input[[paste0("t_q2_", k)]]) %||% ""
        div(
          class = "bw-card",
          strong(paste0("Thema ", k)),
          textInput(paste0("t_title_", k), "Titel des Themas",
            value = tt,
            placeholder = "Titel des Themas", width = "100%"
          ),
          textInput(paste0("t_q1_", k), "Frage 1",
            value = qa,
            placeholder = "Frage 1", width = "100%"
          ),
          textInput(paste0("t_q2_", k), "Frage 2",
            value = qb,
            placeholder = "Frage 2", width = "100%"
          )
        )
      })
    })

    observeEvent(input$setup_save, {
      req(is_mod())
      k <- suppressWarnings(as.integer(input$n_groups %||% 3))
      if (is.na(k) || k < 2 || k > 6) {
        showNotification("Gruppenzahl 2\u20136 waehlen.", type = "warning")
        return()
      }
      vals <- lapply(seq_len(k), function(k) {
        list(
          t = trimws(input[[paste0("t_title_", k)]] %||% ""),
          a = trimws(input[[paste0("t_q1_", k)]] %||% ""),
          b = trimws(input[[paste0("t_q2_", k)]] %||% "")
        )
      })
      if (any(vapply(
        vals, function(v) !nzchar(v$t) || !nzchar(v$a) || !nzchar(v$b),
        logical(1)
      ))) {
        showNotification("Bitte alle Titel und Fragen ausfuellen.", type = "warning")
        return()
      }
      nr <- input$n_rounds %||% 3
      rs <- input$round_secs %||% 300
      err <- configure_session(cfg$db_path, k, nr, rs, vals)
      if (!is.null(err)) showNotification(err, type = "warning")
    })

    mod_lobby_ui <- function() {
      tagList(
        div(
          class = "bw-card",
          h3("Lobby"),
          p(class = "bw-status", "Teilnehmer scannen den QR-Code oder oeffnen:"),
          tags$code(cfg$base_url),
          plotOutput("qr", height = "240px"),
          uiOutput("mod_lobby_list"),
          actionButton("start_btn", "Session starten", class = "btn-primary w-100 btn-lg mt-2")
        )
      )
    }

    output$qr <- renderPlot(
      {
        req(is_mod())
        op <- par(mar = c(0, 0, 0, 0))
        on.exit(par(op), add = TRUE)
        plot(qr_code(cfg$base_url))
      },
      height = 240
    )

    output$mod_lobby_list <- renderUI({
      req(is_mod())
      st <- tick_all()
      div(
        class = "mt-2",
        strong(paste0(nrow(st$participants), " Teilnehmer")),
        p(
          class = "bw-status",
          if (nrow(st$participants)) {
            paste(st$participants$name, collapse = ", ")
          } else {
            "Noch niemand da."
          }
        )
      )
    })

    observeEvent(input$start_btn, {
      req(is_mod())
      err <- start_session(cfg$db_path)
      if (!is.null(err)) showNotification(err, type = "warning")
    })

    mod_running_ui <- function(s) {
      tagList(
        div(
          class = "bw-card text-center",
          div(class = "bw-status", paste0("Runde ", s$current_round, " von ", s$n_rounds)),
          uiOutput("timer"),
          div(
            class = "mt-2 d-flex gap-2 justify-content-center",
            actionButton("plus60_btn", "+60 s", class = "btn btn-outline-secondary"),
            actionButton("next_btn", "Runde beenden", class = "btn-primary")
          )
        ),
        uiOutput("mod_progress")
      )
    }

    observeEvent(input$plus60_btn, {
      req(is_mod())
      con <- db(cfg$db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      dbExecute(con, "
      UPDATE session SET round_ends_at = COALESCE(round_ends_at, :n) + 60
      WHERE id = 1 AND status = 'running'", params = list(n = now()))
    })

    observeEvent(input$next_btn, {
      req(is_mod())
      maybe_advance(cfg$db_path, force = TRUE)
    })

    output$mod_progress <- renderUI({
      req(is_mod())
      st <- tick_all()
      s <- st$s
      if (s$status != "running") {
        return(NULL)
      }
      lapply(seq_len(s$n_groups), function(g) {
        t_id <- topic_for(g, s$current_round, s$n_groups)
        tp <- st$topics[st$topics$id == t_id, ]
        members <- st$participants[!is.na(st$participants$grp) & st$participants$grp == g, ]
        e <- st$entries
        subm <- e[e$round == s$current_round & e$submitted == 1 & e$topic_id == t_id, ]
        done <- length(unique(subm$pid[subm$pid %in% members$pid]))
        div(
          class = "bw-card",
          strong(paste0("Gruppe ", g, " \u2192 ", tp$title)),
          div(class = "bw-status", paste0(done, " / ", nrow(members), " abgegeben")),
          div(class = "bw-status", paste(members$name, collapse = ", "))
        )
      })
    })

    mod_finished_ui <- function() {
      tagList(
        div(
          class = "bw-card",
          h3("Ergebnisse"),
          div(
            class = "d-flex gap-2 flex-wrap align-items-center",
            downloadButton("dl_csv", "CSV"),
            downloadButton("dl_md", "Markdown"),
            actionButton("reset_btn", "Neue Session \u2026", class = "btn-outline-danger ms-auto")
          )
        ),
        uiOutput("mod_results")
      )
    }

    output$mod_results <- renderUI({
      req(is_mod())
      st <- tick_all()
      pn <- stats::setNames(st$participants$name, st$participants$pid)
      lapply(seq_len(nrow(st$topics)), function(i) {
        tp <- st$topics[i, ]
        sub <- st$entries[st$entries$topic_id == tp$id & nzchar(st$entries$text), ]
        div(
          class = "bw-card",
          h4(paste0("Thema ", tp$id, ": ", tp$title)),
          p(class = "bw-status", paste0("F1: ", tp$q1, "   \u00b7   F2: ", tp$q2)),
          if (!nrow(sub)) {
            p(class = "bw-status", "Keine Beitraege.")
          } else {
            lapply(sort(unique(sub$sheet)), function(sh) {
              ss <- sub[sub$sheet == sh, ]
              ss <- ss[order(ss$round, ss$question), ]
              div(
                class = "mb-2",
                strong(paste0("Bogen ", sh)),
                lapply(seq_len(nrow(ss)), function(j) {
                  div(
                    class = "bw-prev",
                    div(
                      class = "who",
                      paste0(
                        "R", ss$round[j], " \u00b7 F", ss$question[j],
                        " \u00b7 ", pn[[ss$pid[j]]]
                      )
                    ),
                    div(ss$text[j])
                  )
                })
              )
            })
          }
        )
      })
    })

    output$dl_csv <- downloadHandler(
      filename = function() paste0("brainwriting_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
      content = function(file) {
        req(is_mod())
        write.csv(export_df(cfg$db_path), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$dl_md <- downloadHandler(
      filename = function() paste0("brainwriting_", format(Sys.time(), "%Y%m%d_%H%M"), ".md"),
      content = function(file) {
        req(is_mod())
        writeLines(build_md(cfg$db_path), file, useBytes = TRUE)
      }
    )

    observeEvent(input$reset_btn, {
      req(is_mod())
      req(identical(isolate(tick_meta())$s$status, "finished"))
      reset_pending(TRUE)
      showModal(modalDialog(
        title = "Neue Session?",
        "Alle Teilnehmer, Boegen und Beitraege werden geloescht. Vorher exportieren!",
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("reset_confirm", "Ja, alles loeschen", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$reset_confirm, {
      req(is_mod())
      req(reset_pending())
      reset_pending(FALSE)
      reset_session(cfg$db_path)
      removeModal()
    })
  }
}
