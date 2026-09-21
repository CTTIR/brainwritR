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
                      group_names = "Nord\nSüd", roster = "Alice\nBob",
                      t_title_1 = "A", t_q1_1 = "A1", t_q2_1 = "A2",
                      t_title_2 = "B", t_q1_2 = "B1", t_q2_2 = "B2")
    settings <- setup_config()
    expect_s3_class(settings, "bw_settings")
    expect_identical(settings$groups, c("Nord", "Süd"))
    expect_identical(settings$participants, c("Alice", "Bob"))
    expect_identical(settings$turn_secs, 45L)
    file <- withr::local_tempfile(fileext = ".yml")
    write_settings(settings, file)
    expect_identical(parse_settings(file), settings)
    session$setInputs(t_q2_2 = "")
    expect_type(setup_config(), "character")
  })
})
