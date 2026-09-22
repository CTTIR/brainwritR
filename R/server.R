#' Build a server bound to immutable configuration
#' @keywords internal
#' @noRd
app_server <- function(cfg) {
  force(cfg)
  auth <- moderator_auth()
  function(input, output, session) {
    is_mod <- moderator_login(input, session, auth, cfg$mod_pin)
    # Each page is bound to exactly one session when it connects.
    route <- resolve_route(cfg, request_query(session))
    if (route$kind == "sessions") {
      return(sessions_server(input, output, session, route$cfg, is_mod))
    }
    if (route$kind != "session") {
      return(notice_server(output, route))
    }
    cfg <- route$cfg
    prefill <- reactiveVal(route$prefill)
    # A deleted session must never be recreated; its open pages reload instead.
    session_alive <- function() {
      if (file.exists(cfg$db_path)) return(TRUE)
      session$sendCustomMessage("bw_reload", "")
      FALSE
    }
    is_archived_now <- function() isTRUE(session_target(cfg$main_db, cfg$code)$archived)
    my_pid <- reactiveVal(NULL)
    part_state <- reactiveVal(NULL)
    mod_state <- reactiveVal(NULL)
    submitted_flag <- reactiveVal(NULL)
    reset_pending <- reactiveVal(FALSE)

    tick_meta <- reactivePoll(cfg$poll_ms, session,
      checkFunc = function() {
        if (!session_alive()) return("gone")
        maybe_advance(cfg$db_path)
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        dbGetQuery(con, "
        SELECT status || '|' || current_round || '|' || current_turn || '|' ||
               COALESCE(round_ends_at, 0) || '|' ||
               (SELECT COUNT(*) FROM participants) || '|' ||
               (SELECT COALESCE(MAX(joined_at), 0) FROM participants) AS v
        FROM session")$v
      },
      valueFunc = function() {
        req(session_alive())
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        list(
          s            = get_session(con),
          topics       = dbGetQuery(con, "SELECT * FROM topics ORDER BY id"),
          participants = dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at, rowid")
        )
      }
    )

    tick_all <- reactivePoll(cfg$poll_ms, session,
      checkFunc = function() {
        if (!session_alive()) return("gone")
        maybe_advance(cfg$db_path)
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        dbGetQuery(con, "
        SELECT (SELECT status || current_round || '|' || current_turn ||
                       COALESCE(round_ends_at, 0) FROM session)
               || '|' ||
               (SELECT COUNT(*) || '-' || COALESCE(MAX(updated_at), 0) || '-' ||
                       COALESCE(SUM(submitted), 0) FROM entries)
               || '|' ||
               (SELECT COUNT(*) FROM participants) AS v")$v
      },
      valueFunc = function() {
        req(session_alive())
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        list(
          s            = get_session(con),
          topics       = dbGetQuery(con, "SELECT * FROM topics ORDER BY id"),
          participants = dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at, rowid"),
          entries      = dbGetQuery(con, "SELECT * FROM entries")
        )
      }
    )

    observeEvent(input$stored_pid, ignoreNULL = FALSE, {
      if (isolate(tick_meta())$s$mode == "hot_seat") {
        return()
      }
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

    active_pid <- function(m = tick_meta()) {
      if (m$s$mode != "hot_seat") {
        return(my_pid())
      }
      i <- m$s$current_turn
      if (m$s$status != "running" || i < 1L || i > nrow(m$participants)) {
        return(NULL)
      }
      m$participants$pid[i]
    }

    observe({
      m <- tick_meta()
      pid <- active_pid(m)
      if (!is.null(pid) && !(pid %in% m$participants$pid)) {
        # A new join can precede the next poll; validate against the database,
        # never clear a just-created identity from an older cached snapshot.
        con <- db(cfg$db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        m$participants <- dbGetQuery(con, "SELECT * FROM participants ORDER BY joined_at, rowid")
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
      if (m$s$mode == "hot_seat") {
        key <- paste(key, m$s$current_turn, is.na(m$s$round_ends_at), sep = "|")
      }
      old <- isolate(part_state())
      if (is.null(old) || !identical(old$key, key)) {
        submitted_flag(NULL)
        part_state(list(key = key, m = m, pid = pid))
      }
    })

    observe({
      m <- tick_meta()
      key <- paste(m$s$status, m$s$current_round, sep = "|")
      if (m$s$mode == "hot_seat") {
        key <- paste(key, m$s$current_turn, is.na(m$s$round_ends_at), sep = "|")
      }
      old <- isolate(mod_state())
      if (is.null(old) || !identical(old$key, key)) {
        mod_state(list(key = key, s = m$s))
      }
    })

    edit_ctx <- reactive({
      m <- tick_meta()
      pid <- active_pid(m)
      if (is.null(pid) || m$s$status != "running") {
        return(NULL)
      }
      if (m$s$mode == "hot_seat" && is.na(m$s$round_ends_at)) {
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
      if (route$mod) {
        if (!is_mod()) {
          return(mod_login_ui())
        }
        return(tagList(mod_nav_ui(cfg), uiOutput("mod_view")))
      }
      uiOutput("part_view")
    })


    observeEvent(input$join_btn, {
      if (!is.null(my_pid())) {
        return()
      }
      if (isolate(tick_meta())$s$mode != "individual") {
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

    group_pending <- reactiveVal(NULL)
    remember_author <- function(pid) {
      if (is.null(pid)) {
        return(invisible(NULL))
      }
      my_pid(pid)
      session$sendCustomMessage("bw_store_pid", pid)
    }
    output$group_choices <- renderUI({
      m <- tick_meta()
      req(m$s$mode == "group_device", m$s$status %in% c("lobby", "running"))
      names <- get_settings(cfg$db_path)$groups
      div(
        class = "bw-card", h2("Als Gruppe beitreten:"),
        lapply(seq_along(names), function(i) {
          tags$button(
            type = "button", class = "btn btn-outline-primary w-100 mb-2",
            onclick = sprintf("Shiny.setInputValue('group_choice', %d, {priority:'event'})", i),
            tags$span(translate = "no", names[i])
          )
        })
      )
    })
    observeEvent(input$group_choice, {
      m <- tick_meta()
      g <- input$group_choice
      req(m$s$mode == "group_device", m$s$status %in% c("lobby", "running"))
      req(
        is.numeric(g), length(g) == 1L, !is.na(g), g == floor(g),
        g >= 1L, g <= m$s$n_groups
      )
      con <- db(cfg$db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      claimed <- dbGetQuery(con, "SELECT pid FROM participants WHERE grp = :g",
        params = list(g = g)
      )
      if (nrow(claimed)) {
        group_pending(g)
        showModal(modalDialog(
          title = paste0(
            "Gruppe ", get_settings(cfg$db_path)$groups[g],
            " ist bereits verbunden \u2014 auf diesem Ger\u00e4t \u00fcbernehmen?"
          ),
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton("group_confirm", "\u00dcbernehmen", class = "btn-primary")
          )
        ))
      } else {
        remember_author(claim_group(cfg$db_path, g))
      }
    })
    observeEvent(input$group_confirm, {
      req(!is.null(group_pending()))
      remember_author(claim_group(cfg$db_path, group_pending()))
      group_pending(NULL)
      removeModal()
    })
    output$roster_choices <- renderUI({
      m <- tick_meta()
      req(m$s$mode == "individual")
      names <- get_settings(cfg$db_path)$participants
      lapply(seq_along(names), function(i) {
        tags$button(
          type = "button", class = "btn btn-outline-secondary me-2 mb-2",
          disabled = if (names[i] %in% m$participants$name) "disabled" else NULL,
          onclick = sprintf("Shiny.setInputValue('roster_choice', %d, {priority:'event'})", i),
          tags$span(translate = "no", names[i])
        )
      })
    })
    observeEvent(input$roster_choice, {
      req(is.null(my_pid()), tick_meta()$s$mode == "individual")
      names <- get_settings(cfg$db_path)$participants
      i <- input$roster_choice
      req(is.numeric(i), length(i) == 1L, !is.na(i), i == floor(i), i >= 1L, i <= length(names))
      con <- db(cfg$db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      if (names[i] %in% dbGetQuery(con, "SELECT name FROM participants")$name) {
        return()
      }
      remember_author(add_participant(cfg$db_path, names[i]))
    })
    observeEvent(input$turn_start, {
      req(tick_meta()$s$mode == "hot_seat")
      arm_turn(cfg$db_path, expected = tick_meta()$s)
    })
    observeEvent(input$turn_skip, {
      m <- tick_meta()
      req(m$s$mode == "hot_seat", is.na(m$s$round_ends_at))
      maybe_advance(cfg$db_path, force = TRUE, expected = tick_meta()$s)
    })
    observeEvent(input$start_override, {
      req(is_mod(), tick_meta()$s$mode == "group_device")
      err <- start_session(cfg$db_path, force = TRUE)
      if (!is.null(err)) showNotification(err, type = "warning")
    })
    output$current_turn_info <- renderUI({
      req(is_mod())
      m <- tick_meta()
      who <- active_pid(m)
      p(paste0("Aktuell: ", m$participants$name[match(who, m$participants$pid)]))
    })
    observeEvent(input$mod_skip, {
      req(is_mod(), tick_meta()$s$mode == "hot_seat")
      maybe_advance(cfg$db_path, force = TRUE, expected = tick_meta()$s)
    })
    observeEvent(input$abort_btn, {
      req(is_mod(), tick_meta()$s$mode == "hot_seat")
      showModal(modalDialog(
        title = "Session beenden?",
        "Die bisherigen Beitr\u00e4ge bleiben erhalten.",
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("abort_confirm", "Zur Ergebnisansicht", class = "btn-danger")
        )
      ))
    })
    observeEvent(input$abort_confirm, {
      req(is_mod(), tick_meta()$s$mode == "hot_seat")
      abort_session(cfg$db_path)
      removeModal()
    })
    observeEvent(input$late_add, {
      req(is_mod(), tick_meta()$s$mode == "hot_seat")
      pid <- add_participant(cfg$db_path, input$late_name %||% "")
      if (!is.null(pid)) shiny::updateTextInput(session, "late_name", value = "")
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
        if (s$mode == "group_device") {
          return(uiOutput("group_choices"))
        }
        return(div(
          class = "bw-card",
          h2("Brainwriting 6-3-5"),
          p(class = "bw-status", "Mit Name oder Pseudonym beitreten:"),
          uiOutput("roster_choices"),
          textInput("join_name", "Name / Pseudonym",
            placeholder = "Name / Pseudonym", width = "100%"
          ),
          actionButton("join_btn", "Teilnehmen", class = "btn-primary w-100 btn-lg")
        ))
      }

      me <- m$participants[m$participants$pid == pid, ]

      if (s$status %in% c("setup", "lobby")) {
        return(div(
          class = "bw-card",
          h3("Hallo ", tags$span(translate = "no", me$name), "!"),
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
      if (s$mode == "hot_seat" && is.na(s$round_ends_at)) {
        return(div(
          class = "bw-card text-center",
          h2("Weitergeben an: ", tags$span(translate = "no", me$name)),
          p(class = "bw-status", paste("Durchgang", s$current_round, "von", s$n_rounds)),
          actionButton("turn_start", "Los geht's", class = "btn-primary w-100 btn-lg"),
          actionButton("turn_skip", "\u00dcberspringen", class = "btn-outline-secondary mt-3")
        ))
      }
      if (is.na(me$grp)) {
        return(div(
          class = "bw-card",
          p("Einen Moment \u2014 du wirst einer Gruppe zugeteilt \u2026")
        ))
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
              h3(class = "mb-0", translate = "no", tp$title)
            ),
            uiOutput("timer")
          )
        ),
        div(
          class = "bw-card",
          tags$label(class = "bw-q", `for` = "a1", "Frage 1 \u2014 ",
                     tags$span(translate = "no", tp$q1)),
          uiOutput("prev1"),
          textAreaInput("a1", NULL,
            value = if (length(mine1)) mine1[1] else "",
            placeholder = "Aufgreifen, ergaenzen, weiterentwickeln \u2026",
            width = "100%"
          ),
          tags$label(class = "bw-q", `for` = "a2", "Frage 2 \u2014 ",
                     tags$span(translate = "no", tp$q2)),
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
        return(p(
          class = "bw-status",
          "Noch keine Vorbeitraege \u2014 dieser Bogen beginnt bei dir."
        ))
      }
      rows <- rows[order(rows$round), ]
      lapply(seq_len(nrow(rows)), function(i) {
        div(
          class = "bw-prev",
          div(class = "who", paste0("Runde ", rows$round[i], " \u00b7 ", pn[[rows$pid[i]]])),
          div(translate = "no", rows$text[i])
        )
      })
    }
    output$prev1 <- renderUI(prev_block(1))
    output$prev2 <- renderUI(prev_block(2))

    draft <- function(value) {
      list(text = value, ctx = isolate(edit_ctx()), pid = isolate(active_pid()))
    }
    a1_d <- debounce(reactive(draft(input$a1)), 1200)
    a2_d <- debounce(reactive(draft(input$a2)), 1200)
    save_draft <- function(value, question) {
      ctx <- value$ctx
      if (is.null(ctx) || is.null(value$text) || is.null(value$pid) || !session_alive()) {
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
      if (is.null(ctx) || !session_alive()) {
        return()
      }
      save_entry(cfg$db_path, active_pid(), ctx$topic_id, ctx$sheet, ctx$round,
        1L, input$a1 %||% "",
        submitted = 1L
      )
      save_entry(cfg$db_path, active_pid(), ctx$topic_id, ctx$sheet, ctx$round,
        2L, input$a2 %||% "",
        submitted = 1L
      )
      if (isolate(tick_meta())$s$mode == "hot_seat") {
        maybe_advance(cfg$db_path, force = TRUE, expected = tick_meta()$s)
      }
      submitted_flag(as.character(ctx$round))
      showNotification("Abgegeben \u2713", type = "message", duration = 2)
    })

    output$submit_area <- renderUI({
      ctx <- edit_ctx()
      if (is.null(ctx)) {
        return(NULL)
      }
      st <- tick_all()
      pid <- active_pid() %||% ""
      done_db <- nrow(st$entries) > 0 &&
        any(st$entries$pid == pid & st$entries$round == ctx$round & st$entries$submitted == 1)
      done <- done_db || identical(submitted_flag(), as.character(ctx$round))
      if (st$s$mode == "hot_seat") {
        return(actionButton("submit_btn", "Fertig \u2014 weitergeben",
          class = "btn-primary w-100 btn-lg"
        ))
      }
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

    setup_upload <- reactiveVal(list(topics = route$prefill$topics, revision = 0L))
    topic_form_gate <- reactiveVal(NULL)
    pending_topic_count <- reactiveVal(NULL)

    observe({
      k <- suppressWarnings(as.integer(input$n_groups %||% 3))
      if (length(k) != 1L || is.na(k) || k < 2L || k > 6L) k <- 3L
      uploaded <- setup_upload()
      old <- isolate(topic_form_gate())
      if (is.null(old) || uploaded$revision != old$revision) {
        target <- if (length(uploaded$topics)) length(uploaded$topics) else k
        pending_topic_count(if (k != target) target else NULL)
        topic_form_gate(list(k = target, revision = uploaded$revision, topics = uploaded$topics))
        return()
      }
      pending <- isolate(pending_topic_count())
      if (!is.null(pending)) {
        if (k == pending) pending_topic_count(NULL)
        return()
      }
      if (k != old$k) {
        topic_form_gate(list(k = k, revision = old$revision, topics = NULL))
      }
    })

    demo_previous <- reactiveVal(NULL)

    setup_apply_topics <- function(topics) {
      shiny::updateNumericInput(session, "n_groups", value = length(topics))
      for (i in seq_along(topics)) {
        for (field in c("title", "q1", "q2")) {
          shiny::updateTextInput(session, paste0("t_", field, "_", i),
                                 value = topics[[i]][[field]])
        }
      }
      setup_upload(list(topics = topics, revision = setup_upload()$revision + 1L))
    }

    observeEvent(input$demo_questions, {
      req(is_mod())
      con <- db(cfg$db_path)
      status <- get_session(con)$status
      dbDisconnect(con)
      if (status != "setup") return()
      if (isTRUE(input$demo_questions)) {
        k <- input$n_groups %||% 3
        if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
              k != floor(k) || k < 2 || k > 6) k <- 3
        previous <- lapply(seq_len(k), function(i) {
          list(title = input[[paste0("t_title_", i)]] %||% "",
               q1 = input[[paste0("t_q1_", i)]] %||% "",
               q2 = input[[paste0("t_q2_", i)]] %||% "")
        })
        demo_previous(previous)
        setup_apply_topics(example_topics(input$ui_language %||% "de"))
      } else {
        previous <- demo_previous()
        demo_previous(NULL)
        if (!is.null(previous)) setup_apply_topics(previous)
      }
    }, ignoreInit = TRUE)


    setup_names <- function(value) {
      names <- trimws(strsplit(value %||% "", "\n", fixed = TRUE)[[1]])
      names[nzchar(names)]
    }

    setup_config <- reactive({
      k <- input$n_groups %||% 3
      if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
            k != floor(k) || k < 2 || k > 6) {
        return("topics: muss 2 bis 6 Themen enthalten.")
      }
      groups <- setup_names(input$group_names)
      validate_settings(list(
        format = "brainwriting635-settings/1",
        mode = input$play_mode %||% "individual",
        rounds = input$n_rounds %||% 3,
        round_secs = input$round_secs %||% 300,
        turn_secs = input$turn_secs %||% 90,
        topics = lapply(seq_len(k), function(i) {
          list(
            title = input[[paste0("t_title_", i)]] %||% "",
            q1 = input[[paste0("t_q1_", i)]] %||% "",
            q2 = input[[paste0("t_q2_", i)]] %||% ""
          )
        }),
        groups = if (length(groups)) groups else NULL,
        participants = setup_names(input$roster)
      ))
    })

    mod_setup_ui <- function() {
      d <- setup_defaults(isolate(prefill()))
      tagList(
        div(
          class = "bw-card",
          h3("Session einrichten"),
          shiny::fileInput("settings_file", "Einstellungen laden (YAML)",
            accept = c(".yml", ".yaml"),
            buttonLabel = "Durchsuchen \u2026", placeholder = "Keine Datei ausgew\u00e4hlt"
          ),
          shiny::checkboxInput("demo_questions", "Beispiel-Fragenset verwenden", FALSE),
          p(class = "bw-status", paste0(
            "Drei bearbeitbare Beispielthemen nach dem 6-3-5-Prinzip: neue Ideen notieren, ",
            "Ideen von oben weiterentwickeln. ",
            "Abw\u00e4hlen stellt die vorherigen Themen wieder her."
          )),
          shiny::radioButtons("play_mode", "Spielmodus", choices = c(
            "Alle am eigenen Ger\u00e4t" = "individual",
            "Ein Ger\u00e4t pro Gruppe" = "group_device",
            "Reihum an einem Ger\u00e4t" = "hot_seat"
          ), selected = d$mode),
          div(
            class = "row g-2",
            div(
              class = "col-12 col-sm-4",
              numericInput("n_groups", "Themen = Gruppen", d$k,
                min = 2, max = 6,
                width = "100%"
              )
            ),
            div(
              class = "col-12 col-sm-4",
              numericInput("n_rounds", "Runden / Durchg\u00e4nge", d$rounds,
                min = 1, max = 12,
                width = "100%"
              )
            ),
            div(
              class = "col-12 col-sm-4",
              shiny::conditionalPanel(
                "input.play_mode !== 'hot_seat'",
                numericInput("round_secs", "Sek./Runde", d$round_secs,
                  min = 30, max = 1800,
                  step = 30, width = "100%"
                )
              ),
              shiny::conditionalPanel(
                "input.play_mode === 'hot_seat'",
                numericInput("turn_secs", "Sek./Person", d$turn_secs,
                  min = 20, max = 600,
                  step = 10, width = "100%"
                )
              )
            )
          ),
          shiny::conditionalPanel(
            "input.play_mode !== 'group_device'",
            textAreaInput("roster", "Namen (ein Name pro Zeile)",
              value = d$roster, rows = 5,
              placeholder = "Im Einzelmodus optional", width = "100%"
            )
          ),
          shiny::conditionalPanel(
            "input.play_mode === 'group_device'",
            textAreaInput("group_names", "Gruppennamen (ein Name pro Zeile, optional)",
              value = d$groups, rows = 4, width = "100%"
            )
          ),
          uiOutput("duration_estimate"),
          p(class = "bw-status", paste0(
            "Preset 15-2-5: 3 Themen, 3 Runden, 300 s. ",
            "Nach K Runden hat jeder jedes Thema 1\u00d7 bearbeitet."
          ))
        ),
        uiOutput("topic_form"),
        downloadButton("dl_settings", "Einstellungen exportieren (YAML)"),
        actionButton("setup_save", "Speichern und Lobby \u00f6ffnen",
          class = "btn-primary w-100 btn-lg mt-2"
        )
      )
    }

    observeEvent(input$play_mode, {
      shiny::updateActionButton(session, "setup_save",
        label =
          if (identical(input$play_mode, "hot_seat")) {
            "Speichern und starten"
          } else {
            "Speichern und Lobby \u00f6ffnen"
          }
      )
    })

    output$duration_estimate <- renderUI({
      hot <- identical(input$play_mode, "hot_seat")
      seconds <- (input$n_rounds %||% 3) * if (hot) {
        length(setup_names(input$roster)) * (input$turn_secs %||% 90)
      } else {
        input$round_secs %||% 300
      }
      if (!is.finite(seconds)) {
        return(NULL)
      }
      p(class = "bw-status", sprintf(
        "Gesch\u00e4tzte Arbeitszeit: %.1f Minuten (ohne \u00dcbergaben und Pausen).",
        seconds / 60
      ))
    })

    output$topic_form <- renderUI({
      req(is_mod())
      state <- topic_form_gate()
      req(!is.null(state))
      lapply(seq_len(state$k), function(i) {
        seed <- if (length(state$topics) >= i) state$topics[[i]] else NULL
        value <- function(field, id) {
          if (!is.null(seed)) seed[[field]] else isolate(input[[paste0(id, i)]]) %||% ""
        }
        div(
          class = "bw-card", strong(paste0("Thema ", i)),
          textInput(paste0("t_title_", i), "Titel des Themas",
            value = value("title", "t_title_"), width = "100%"
          ),
          textAreaInput(paste0("t_q1_", i), "Frage 1", rows = 2,
            value = value("q1", "t_q1_"), width = "100%"
          ),
          textAreaInput(paste0("t_q2_", i), "Frage 2", rows = 2,
            value = value("q2", "t_q2_"), width = "100%"
          )
        )
      })
    })

    observeEvent(input$settings_file, {
      req(is_mod())
      con <- db(cfg$db_path)
      status <- get_session(con)$status
      dbDisconnect(con)
      if (status != "setup") {
        return()
      }
      warnings <- new.env(parent = emptyenv())
      warnings$messages <- character()
      parsed <- withCallingHandlers(parse_settings(input$settings_file$datapath),
        warning = function(w) {
          warnings$messages <- c(warnings$messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      if (!inherits(parsed, "bw_settings")) {
        shiny::showModal(shiny::modalDialog(
          title = "Einstellungen pr\u00fcfen",
          tags$ul(lapply(parsed, tags$li)), easyClose = TRUE
        ))
        return()
      }
      if (length(warnings$messages)) {
        showNotification(paste(warnings$messages, collapse = "\n"),
          type = "warning", duration = NULL
        )
      }
      demo_previous(NULL)
      shiny::updateCheckboxInput(session, "demo_questions", value = FALSE)
      shiny::updateRadioButtons(session, "play_mode", selected = parsed$mode)
      shiny::updateNumericInput(session, "n_groups", value = length(parsed$topics))
      shiny::updateNumericInput(session, "n_rounds", value = parsed$rounds)
      shiny::updateNumericInput(session, "round_secs", value = parsed$round_secs)
      shiny::updateNumericInput(session, "turn_secs", value = parsed$turn_secs)
      shiny::updateTextAreaInput(session, "roster",
        value = paste(parsed$participants, collapse = "\n")
      )
      shiny::updateTextAreaInput(session, "group_names",
        value = paste(parsed$groups, collapse = "\n")
      )
      for (i in seq_along(parsed$topics)) {
        for (field in c("title", "q1", "q2")) {
          shiny::updateTextInput(session, paste0("t_", field, "_", i),
            value = parsed$topics[[i]][[field]]
          )
        }
      }
      setup_upload(list(topics = parsed$topics, revision = setup_upload()$revision + 1L))
      showNotification("Einstellungen geladen. Bitte pr\u00fcfen und best\u00e4tigen.",
        type = "message"
      )
    })

    output$dl_settings <- downloadHandler(
      filename = function() {
        paste0(
          "brainwriting_einstellungen_", format(Sys.Date(), "%Y%m%d"),
          ".yml"
        )
      },
      content = function(file) {
        req(is_mod())
        con <- db(cfg$db_path)
        status <- get_session(con)$status
        dbDisconnect(con)
        settings <- if (status == "setup") isolate(setup_config()) else get_settings(cfg$db_path)
        write_settings(settings, file)
      }
    )

    observeEvent(input$setup_save, {
      req(is_mod())
      settings <- setup_config()
      if (!inherits(settings, "bw_settings")) {
        shiny::showModal(shiny::modalDialog(
          title = "Einstellungen pr\u00fcfen",
          tags$ul(lapply(settings, tags$li)), easyClose = TRUE
        ))
        return()
      }
      vals <- lapply(settings$topics, function(topic) {
        list(t = topic$title, a = topic$q1, b = topic$q2)
      })
      err <- configure_session(cfg$db_path, length(vals), settings$rounds,
        settings$round_secs, vals,
        settings = settings
      )
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
          downloadButton("dl_settings", "Einstellungen exportieren (YAML)"),
          actionButton("start_btn", "Session starten", class = "btn-primary w-100 btn-lg mt-2"),
          if (isolate(tick_meta())$s$mode == "group_device") {
            actionButton("start_override", "Ohne alle Gruppen starten",
              class = "btn-outline-secondary w-100 mt-2"
            )
          }
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
            tags$span(translate = "no", paste(st$participants$name, collapse = ", "))
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
            actionButton("plus60_btn", if (s$mode == "hot_seat") "+30 s" else "+60 s",
              class = "btn btn-outline-secondary"
            ),
            actionButton("next_btn", "Runde beenden", class = "btn-primary")
          )
        ),
        if (s$mode == "hot_seat") {
          div(
            class = "bw-card",
            uiOutput("current_turn_info"),
            actionButton("mod_skip", "\u00dcberspringen", class = "btn-outline-secondary"),
            actionButton("abort_btn", "Abbrechen zur Ergebnisansicht",
              class = "btn-outline-danger"
            ),
            textInput("late_name", "Name / Pseudonym"),
            actionButton("late_add", "Nachz\u00fcgler hinzuf\u00fcgen")
          )
        },
        uiOutput("mod_progress")
      )
    }

    observeEvent(input$plus60_btn, {
      req(is_mod())
      seconds <- if (isolate(tick_meta())$s$mode == "hot_seat") 30 else 60
      extend_clock(cfg$db_path, seconds)
    })

    observeEvent(input$next_btn, {
      req(is_mod())
      if (isolate(tick_meta())$s$mode == "hot_seat") {
        end_pass(cfg$db_path, expected = tick_meta()$s)
      } else {
        maybe_advance(cfg$db_path, force = TRUE)
      }
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
          div(class = "bw-status", translate = "no", paste(members$name, collapse = ", "))
        )
      })
    })

    mod_finished_ui <- function() {
      archived <- is_archived_now()
      tagList(
        if (archived) {
          div(class = "bw-card bw-status", "Archiviert \u2014 nur Ansicht und Export.")
        },
        div(
          class = "bw-card",
          h3("Ergebnisse"),
          div(
            class = "d-flex gap-2 flex-wrap align-items-center",
            downloadButton("dl_csv", "CSV"),
            downloadButton("dl_md", "Markdown"),
            downloadButton("dl_rds", "RDS"),
            downloadButton("dl_xlsx", "XLSX"),
            downloadButton("dl_pdf", "Bericht (PDF)"),
            if (!archived) {
              actionButton("reset_btn", "Zur\u00fccksetzen \u2026",
                           class = "btn-outline-danger ms-auto")
            }
          )
        ),
        shiny::checkboxInput("anonymize", "Namen pseudonymisieren (TN-01, TN-02, \u2026)", FALSE),
        shiny::tabsetPanel(
          shiny::tabPanel("Beitr\u00e4ge", uiOutput("mod_results")),
          shiny::tabPanel("Auswertung", uiOutput("analytics_view"))
        )
      )
    }

    output$analytics_view <- renderUI({
      req(is_mod(), tick_meta()$s$status == "finished")
      st <- tick_all()
      tagList(
        div(
          class = "bw-card", uiOutput("analytics_kpis"),
          shiny::checkboxInput("exclude_prompt", "Begriffe aus Themen und Fragen ausblenden", TRUE),
          p(
            class = "bw-status",
            "Deskriptive Textauswertung; keine Bewertung der Ideenqualit\u00e4t."
          )
        ),
        div(class = "bw-card", plotOutput("fig_contributions", height = "300px")),
        div(class = "bw-card", plotOutput("fig_terms", height = "420px")),
        div(
          class = "bw-card",
          shiny::selectInput("analytics_topic", "Thema", choices = c(
            "Alle Themen" = "all", stats::setNames(as.character(st$topics$id), st$topics$title)
          )),
          numericInput("min_cooc", "Min. gemeinsames Auftreten", 2, min = 1, max = 100),
          plotOutput("fig_network", height = "360px"),
          plotOutput("fig_wordcloud", height = "300px")
        ),
        div(
          class = "bw-card", plotOutput("fig_buildon", height = "280px"),
          p(class = "bw-status", paste0(
            "Ankn\u00fcpfungsgrad: mittlere Jaccard-\u00dcberlappung der Begriffe ",
            "aufeinanderfolgender Runden je Bogen und Frage. ",
            "Wortwiederholung ist kein Nachweis inhaltlicher Weiterentwicklung."
          ))
        )
      )
    })
    analytics_data <- reactive({
      req(is_mod(), tick_meta()$s$status == "finished")
      tick_all()
    })
    analytics_selected <- reactive({
      e <- analytics_data()$entries
      topic <- input$analytics_topic %||% "all"
      if (topic != "all") e <- e[e$topic_id == suppressWarnings(as.integer(topic)), , drop = FALSE]
      e
    })
    output$analytics_kpis <- renderUI({
      st <- analytics_data()
      k <- bw_kpis(st$entries, st$topics, exclude_prompt = input$exclude_prompt %||% TRUE)
      values <- c(
        k$contributions, round(k$mean_words, 1),
        if (is.na(k$submission_rate)) {
          "\u2014"
        } else {
          paste0(round(100 * k$submission_rate), "%")
        }, k$distinct_terms
      )
      values[is.na(values)] <- "\u2014"
      labels <- c(
        "Beitr\u00e4ge", "\u00d8 W\u00f6rter je Beitrag", "Abgabequote",
        "Verschiedene Begriffe"
      )
      div(class = "row g-3", lapply(seq_along(labels), function(i) {
        div(
          class = "col-6 col-sm-3", div(class = "fs-3 fw-bold", values[i]),
          div(class = "bw-status", labels[i])
        )
      }))
    })
    output$fig_contributions <- renderPlot({
      st <- analytics_data()
      bw_plot_language(bw_plot_contributions(st$entries, st$topics), input$ui_language %||% "de")
    })
    output$fig_terms <- renderPlot({
      st <- analytics_data()
      bw_plot_language(
        bw_plot_terms(st$entries, st$topics, exclude_prompt = input$exclude_prompt %||% TRUE),
        input$ui_language %||% "de"
      )
    })
    output$fig_network <- renderPlot({
      st <- analytics_data()
      threshold <- input$min_cooc %||% 2
      req(is.numeric(threshold), is.finite(threshold), threshold >= 1)
      bw_plot_language(bw_plot_network(analytics_selected(), st$topics,
                         min_cooc = threshold,
                         exclude_prompt = input$exclude_prompt %||% TRUE
                       ), input$ui_language %||% "de")
    })
    output$fig_wordcloud <- renderPlot({
      st <- analytics_data()
      plot <- bw_plot_language(bw_plot_wordcloud(analytics_selected(), st$topics,
                                 exclude_prompt = input$exclude_prompt %||% TRUE
                               ), input$ui_language %||% "de")
      # ggwordcloud reseeds while drawing; keep participant and group draws random.
      withr::with_preserve_seed(print(plot))
    })
    output$fig_buildon <- renderPlot({
      st <- analytics_data()
      bw_plot_language(bw_plot_buildon(st$entries, st$topics,
                         exclude_prompt = input$exclude_prompt %||% TRUE
                       ), input$ui_language %||% "de")
    })

    output$mod_results <- renderUI({
      req(is_mod())
      st <- tick_all()
      pn <- stats::setNames(st$participants$name, st$participants$pid)
      lapply(seq_len(nrow(st$topics)), function(i) {
        tp <- st$topics[i, ]
        sub <- st$entries[st$entries$topic_id == tp$id & nzchar(st$entries$text), ]
        div(
          class = "bw-card",
          h4(paste0("Thema ", tp$id, ": "), tags$span(translate = "no", tp$title)),
          p(
            class = "bw-status", translate = "no",
            paste0("F1: ", tp$q1, "   \u00b7   F2: ", tp$q2)
          ),
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
                    div(translate = "no", ss$text[j])
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
        data <- export_df(cfg$db_path)
        if (isTRUE(input$anonymize)) data <- export_snapshot_df(download_data())
        write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$dl_md <- downloadHandler(
      filename = function() paste0("brainwriting_", format(Sys.time(), "%Y%m%d_%H%M"), ".md"),
      content = function(file) {
        req(is_mod())
        text <- if (isTRUE(input$anonymize)) {
          build_snapshot_md(download_data())
        } else {
          build_md(cfg$db_path)
        }
        writeLines(text, file, useBytes = TRUE)
      }
    )

    download_data <- function() {
      req(is_mod())
      data <- collect_data(cfg$db_path)
      if (isTRUE(input$anonymize)) data <- pseudonymize_data(data)
      data
    }
    output$dl_rds <- downloadHandler(
      filename = function() paste0("brainwriting_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds"),
      content = function(file) write_rds(download_data(), file)
    )
    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("brainwriting_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
      content = function(file) write_xlsx(download_data(), file)
    )
    output$dl_pdf <- downloadHandler(
      filename = function() {
        paste0("brainwriting_bericht_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) build_report(download_data(), file)
    )

    observeEvent(input$reset_btn, {
      req(is_mod())
      req(identical(isolate(tick_meta())$s$status, "finished"), !is_archived_now())
      reset_pending(TRUE)
      showModal(modalDialog(
        title = "Session zur\u00fccksetzen?",
        "Alle Teilnehmer, Boegen und Beitraege werden geloescht. Vorher exportieren!",
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("reset_confirm", "Ja, alles loeschen", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$reset_confirm, {
      req(is_mod())
      req(reset_pending(), !is_archived_now())
      reset_pending(FALSE)
      reset_session(cfg$db_path)
      demo_previous(NULL)
      prefill(NULL)
      setup_upload(list(
        topics = replicate(3L, list(title = "", q1 = "", q2 = ""), simplify = FALSE),
        revision = setup_upload()$revision + 1L
      ))
      removeModal()
    })
  }
}
