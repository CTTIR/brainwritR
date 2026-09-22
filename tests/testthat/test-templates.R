test_that("example topics follow 6-3-5 and are translated for every supported language", {
  first <- c(de = "Mehr Beteiligung in Gruppenarbeit", en = "More participation in group work",
             fr = "Plus de participation en groupe")
  three <- c(de = "bis zu drei neue Ideen", en = "up to three new ideas",
             fr = "jusqu'\u00e0 trois nouvelles id\u00e9es")
  build <- c(de = "entwickle sie weiter", en = "develop it further", fr = "d\u00e9veloppez-la")
  for (language in names(first)) {
    topics <- example_topics(language)
    expect_length(topics, 3L)
    expect_identical(topics[[1]]$title, unname(first[[language]]))
    # One open problem per sheet: new ideas first, then building on the sheet.
    for (topic in topics) {
      expect_match(topic$q1, "?", fixed = TRUE)
      expect_match(topic$q1, three[[language]], fixed = TRUE)
      expect_match(topic$q2, build[[language]], fixed = TRUE)
    }
    expect_length(unique(vapply(topics, `[[`, "", "q2")), 1L)
    settings <- list(format = "brainwriting635-settings/1", mode = "individual",
                     rounds = 3, round_secs = 300, turn_secs = 90, topics = topics)
    expect_s3_class(validate_settings(settings), "bw_settings")
  }
  expect_identical(example_topics("unknown"), example_topics("de"))
  expect_identical(example_topics(NA_character_), example_topics("de"))
  expect_identical(example_topics(character()), example_topics("de"))
})

test_that("example toggle restores previous editable topics and leaves other settings alone", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, demo_questions = FALSE,
                      ui_language = "en", n_groups = 2,
                      play_mode = "hot_seat", turn_secs = 45, roster = "Alex",
                      t_title_1 = "Original A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "Original B", t_q1_2 = "B1", t_q2_2 = "B2")
    original <- list(list(title = "Original A", q1 = "A1", q2 = "A2"),
                     list(title = "Original B", q1 = "B1", q2 = "B2"))
    session$setInputs(demo_questions = TRUE)
    expect_identical(demo_previous(), original)
    expect_identical(setup_upload()$topics, example_topics("en"))
    expect_match(output$topic_form$html, "More participation in group work", fixed = TRUE)
    expect_identical(input$play_mode, "hot_seat")
    expect_equal(input$turn_secs, 45)
    expect_identical(input$roster, "Alex")
    session$setInputs(ui_language = "fr")
    expect_identical(setup_upload()$topics, example_topics("en"))
    session$setInputs(demo_questions = FALSE)
    expect_null(demo_previous())
    expect_identical(setup_upload()$topics, original)
    expect_match(output$topic_form$html, "Original B", fixed = TRUE)
    expect_equal(nrow(read_table(path, "topics")), 0L)
    expect_identical(read_table(path, "session")$status, "setup")
  })
})

test_that("a valid upload supersedes the demo and cannot restore stale topics", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  file <- system.file("extdata", "settings-example.yml", package = "brainwritR")
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, demo_questions = FALSE,
                      ui_language = "en", n_groups = 2, t_title_1 = "Original")
    session$setInputs(demo_questions = TRUE)
    expect_false(is.null(demo_previous()))
    session$setInputs(settings_file = data.frame(
      name = basename(file), size = file.info(file)$size, type = "text/yaml", datapath = file
    ))
    expect_null(demo_previous())
    expect_identical(setup_upload()$topics, parse_settings(file)$topics)
    revision <- setup_upload()$revision
    session$setInputs(demo_questions = FALSE)
    expect_identical(setup_upload()$topics, parse_settings(file)$topics)
    expect_identical(setup_upload()$revision, revision)
  })
})

test_that("late topic-count acknowledgements do not rerender edited example fields", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, n_groups = 2,
                      demo_questions = FALSE, ui_language = "en")
    session$setInputs(demo_questions = TRUE)
    state <- topic_form_gate()
    expect_equal(state$k, 3)
    expect_equal(pending_topic_count(), 3)
    session$setInputs(t_q1_1 = "An immediate edit", ui_language = "fr")
    expect_identical(topic_form_gate(), state)
    session$setInputs(n_groups = 3)
    expect_identical(topic_form_gate(), state)
    expect_null(pending_topic_count())
    session$setInputs(n_groups = 4)
    expect_equal(topic_form_gate()$k, 4)
    expect_null(topic_form_gate()$topics)
  })
})

test_that("the setup form explains the 6-3-5 structure of the example set in every language", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    html <- output$mod_view$html
    hint <- regmatches(html, regexpr("Drei bearbeitbare Beispielthemen[^<]*", html))
    expect_match(hint, "6-3-5", fixed = TRUE)
    expect_match(bw_translate(hint, "en"), "6-3-5 principle", fixed = TRUE)
    expect_match(bw_translate(hint, "fr"), "principe 6-3-5", fixed = TRUE)
  })
})
