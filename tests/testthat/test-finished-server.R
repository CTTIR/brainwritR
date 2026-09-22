test_that("finished analytics render and downloads retain the selected privacy boundary", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  participants <- read_table(path, "participants")
  for (r in 1:3) {
    for (i in 1:3) {
      topic <- topic_for(participants$grp[i], r, 3)
      save_entry(path, participants$pid[i], topic, 1, r, 1, "Lernen gemeinsam gestalten", 1)
      save_entry(path, participants$pid[i], topic, 1, r, 2, "Gemeinsam gestalten hilft", 1)
    }
    maybe_advance(path, TRUE)
  }
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    expect_error(analytics_data(), class = "shiny.silent.error")
    session$setInputs(pin = "secret", pin_btn = 1, analytics_topic = "all",
                      min_cooc = 2, exclude_prompt = TRUE)
    expect_match(output$analytics_view$html, "fig_network", fixed = TRUE)
    expect_match(output$analytics_kpis$html, "100%", fixed = TRUE)
    expect_equal(nrow(analytics_selected()), 18)
    session$setInputs(analytics_topic = "1", exclude_prompt = FALSE)
    expect_equal(nrow(analytics_selected()), 6)
    for (id in c("fig_contributions", "fig_terms", "fig_network", "fig_wordcloud", "fig_buildon")) {
      expect_true(nzchar(output[[id]]$src))
    }
    session$setInputs(anonymize = TRUE)
    snapshot <- readRDS(output$dl_rds)
    expect_identical(snapshot$participants$name, sprintf("TN-%02d", 1:3))
    csv <- utils::read.csv(output$dl_csv, check.names = FALSE)
    expect_equal(nrow(csv), 18)
    expect_setequal(csv$Teilnehmer, snapshot$participants$name)
    markdown <- paste(readLines(output$dl_md, warn = FALSE), collapse = "\n")
    workbook <- output$dl_xlsx
    expect_equal(length(openxlsx::getSheetNames(workbook)), 4)
    spreadsheet <- openxlsx::read.xlsx(workbook, sheet = 1)
    expect_setequal(spreadsheet$Teilnehmer, snapshot$participants$name)
    for (name in participants$name) expect_false(grepl(name, markdown, fixed = TRUE))
    expect_false(grepl(participants$name[1], output$mod_results$html, fixed = TRUE))
    expect_match(output$mod_results$html, "TN-01", fixed = TRUE)
    if (capabilities("cairo")) expect_true(file.exists(output$dl_pdf))
  })
})

test_that("settings downloads use reviewed form values before setup and frozen values after", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    expect_error(output$dl_settings, class = "shiny.silent.error")
    session$setInputs(pin = "secret", pin_btn = 1, n_groups = 2, n_rounds = 2,
                      round_secs = 120, turn_secs = 45, play_mode = "individual",
                      roster = "Alice\nBob", group_names = "Nord\nSued",
                      t_title_1 = "A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "B", t_q1_2 = "B1", t_q2_2 = "B2")
    reviewed <- setup_config()
    expect_identical(parse_settings(output$dl_settings), reviewed)
    session$setInputs(setup_save = 1)
    expect_identical(read_table(path, "session")$status, "lobby")
    session$setInputs(roster = "Changed", round_secs = 180)
    expect_identical(parse_settings(output$dl_settings), reviewed)
    expect_identical(get_settings(path), reviewed)
  })
})
