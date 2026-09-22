test_that("moderator setup offers mode controls and a live duration estimate", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    html <- as.character(mod_setup_ui())
    expect_match(html, "play_mode")
    expect_match(html, "group_names")
    expect_match(html, "roster")
    expect_match(html, "turn_secs")
    expect_match(html, "conditional")
    session$setInputs(play_mode = "hot_seat", roster = "Alice\nBob", n_rounds = 3,
                      turn_secs = 90)
    expect_match(output$duration_estimate$html, "9.0 Minuten", fixed = TRUE)
    expect_identical(read_table(path, "session")$status, "setup")
  })
})

test_that("settings upload prefills topics without starting or writing the database", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  file <- system.file("extdata", "settings-example.yml", package = "brainwritR")
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    before <- read_table(path, "session")
    session$setInputs(settings_file = data.frame(
      name = basename(file), size = file.info(file)$size, type = "text/yaml", datapath = file
    ))
    expect_identical(read_table(path, "session"), before)
    expect_equal(nrow(read_table(path, "topics")), 0)
    expect_equal(setup_upload()$revision, 1L)
    expect_identical(setup_upload()$topics, parse_settings(file)$topics)
    expect_match(output$topic_form$html, "Lernen im Kurs")
  })
})

test_that("setup config validates and round-trips all editable fields", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, n_groups = 2, n_rounds = 4,
                      round_secs = 120, turn_secs = 45, play_mode = "group_device",
                      group_names = "Nord\nS\u00fcd", roster = "Alice\nBob",
                      t_title_1 = "A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "B", t_q1_2 = "B1", t_q2_2 = "B2")
    settings <- setup_config()
    expect_s3_class(settings, "bw_settings")
    expect_identical(settings$groups, c("Nord", "S\u00fcd"))
    expect_identical(settings$participants, c("Alice", "Bob"))
    expect_identical(settings$turn_secs, 45L)
    file <- withr::local_tempfile(fileext = ".yml")
    write_settings(settings, file)
    expect_identical(parse_settings(file), settings)
    session$setInputs(t_q2_2 = "")
    expect_type(setup_config(), "character")
  })
})

test_that("the format summary follows participants, topics, rounds and timing", {
  expect_identical(format_summary("individual", 15, 3, 3, 300), c(
    "Format 15-2-5: 3 Themen, 3 Runden, 300 s pro Runde.", "Je Gruppe 5 Personen.",
    "Nach 3 Runden hat jede Person jedes Thema 1\u00d7 bearbeitet."
  ))
  expect_identical(format_summary("group_device", 16, 3, 2, 90), c(
    "Format 16-2-1,5: 3 Themen, 2 Runden, 90 s pro Runde.", "Je Gruppe 5\u20136 Personen.",
    "Mit 2 Runden bearbeitet jede Person 2 von 3 Themen."
  ))
  expect_identical(format_summary("individual", 12, 3, 4, 300)[3], paste(
    "Nach 3 Runden hat jede Person jedes Thema bearbeitet;",
    "danach wiederholen sich die Themen."
  ))
  expect_identical(format_summary("individual", 2, 3, 3, 300)[2],
                   "F\u00fcr 3 Themen sind mindestens 3 Teilnehmende n\u00f6tig.")
  expect_identical(format_summary("hot_seat", NA, 3, 3, 300, turn_secs = 90, roster = 2)[1],
                   "Reihum: 2 Personen, 3 Durchg\u00e4nge, 90 s je Person.")
  expect_length(format_summary("individual", NA, 3, 3, 300), 2L)
})

test_that("setup asks for the planned group size and explains optional names", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    html <- output$mod_view$html
    expect_match(html, 'id="n_participants"', fixed = TRUE)
    expect_match(html, "Namen (optional, ein Name pro Zeile)", fixed = TRUE)
    expect_match(html, "gerne ein Pseudonym", fixed = TRUE)
    expect_false(grepl("Preset 15-2-5", html, fixed = TRUE))
    session$setInputs(n_participants = 16, n_groups = 3, n_rounds = 3, round_secs = 300,
                      play_mode = "individual",
                      t_title_1 = "A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "B", t_q1_2 = "B1", t_q2_2 = "B2",
                      t_title_3 = "C", t_q1_3 = "C1", t_q2_3 = "C2")
    expect_match(output$format_summary$html, "Format 16-2-5", fixed = TRUE)
    expect_match(output$format_summary$html, "Je Gruppe 5\u20136 Personen.", fixed = TRUE)
    expect_identical(setup_config()$expected_participants, 16L)
    session$setInputs(setup_save = 1)
    session$elapse(100)
    expect_match(output$mod_lobby_list$html, "0 von 16 Teilnehmenden", fixed = TRUE)
  })
  expect_identical(get_settings(path)$expected_participants, 16L)
})

test_that("an emptied planned-participants field is simply not stored", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, n_participants = NA, n_groups = 2,
                      n_rounds = 2, round_secs = 120, play_mode = "individual",
                      t_title_1 = "A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "B", t_q1_2 = "B1", t_q2_2 = "B2")
    expect_s3_class(setup_config(), "bw_settings")
    expect_null(setup_config()$expected_participants)
    session$setInputs(setup_save = 1)
  })
  expect_identical(read_table(path, "session")$status, "lobby")
})

test_that("the group-device lobby counts connected groups, not planned people", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, n_participants = 16, n_groups = 3,
                      n_rounds = 3, round_secs = 300, play_mode = "group_device",
                      t_title_1 = "A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "B", t_q1_2 = "B1", t_q2_2 = "B2",
                      t_title_3 = "C", t_q1_3 = "C1", t_q2_3 = "C2", setup_save = 1)
    session$elapse(100)
    for (g in 1:2) claim_group(path, g)
    session$elapse(100)
    expect_match(output$mod_lobby_list$html, "2 von 3 Gruppen", fixed = TRUE)
  })
})
