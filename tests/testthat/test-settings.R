settings_fixture <- function() {
  list(format = "brainwriting635-settings/1", mode = "individual", rounds = 3,
       round_secs = 300, turn_secs = 90,
       topics = list(list(title = "A", q1 = "A1?", q2 = "A2?"),
                     list(title = "B", q1 = "B1?", q2 = "B2?")))
}

test_that("settings normalize defaults and every mode", {
  for (mode in c("individual", "group_device", "hot_seat")) {
    x <- settings_fixture()
    x$mode <- mode
    config <- validate_settings(x)
    expect_s3_class(config, "bw_settings")
    expect_identical(config$groups, c("Gruppe 1", "Gruppe 2"))
    expect_identical(config$participants, character())
    expect_identical(config$rounds, 3L)
  }
  example <- system.file("extdata", "settings-example.yml", package = "brainwritR")
  config <- parse_settings(example)
  expect_s3_class(config, "bw_settings")
  expect_length(config$topics, 3L)
  expect_identical(config$participants, c("Alice", "Bob"))
  for (topic in config$topics) {
    expect_match(topic$q1, "bis zu drei neue Ideen", fixed = TRUE)
    expect_match(topic$q2, "entwickle sie weiter", fixed = TRUE)
  }
})

test_that("every settings validation rule reports German errors", {
  cases <- list(
    list(field = "format", value = "bad", pattern = "format:"),
    list(field = "mode", value = "bad", pattern = "mode:"),
    list(field = "rounds", value = 13, pattern = "rounds:"),
    list(field = "round_secs", value = 29, pattern = "round_secs:"),
    list(field = "turn_secs", value = 601, pattern = "turn_secs:"),
    list(field = "groups", value = "Solo", pattern = "groups: Anzahl"),
    list(field = "groups", value = c("A", " A "), pattern = "groups: Namen"),
    list(field = "participants", value = c("A", " A "), pattern = "participants:"),
    list(field = "participants", value = c("A", " "), pattern = "participants:")
  )
  for (case in cases) {
    x <- settings_fixture()
    x[[case$field]] <- case$value
    errors <- validate_settings(x)
    expect_type(errors, "character")
    expect_true(any(grepl(case$pattern, errors)))
  }
  x <- settings_fixture()
  x$topics[[2]]$q2 <- NULL
  expect_match(validate_settings(x), "Thema 2 hat keine Frage 2")
  x$mode <- "bad"
  x$rounds <- 0
  expect_length(validate_settings(x), 3L)
  expect_type(validate_settings(NULL), "character")
})

test_that("malformed types are rejected without crashing", {
  for (field in c("rounds", "round_secs", "turn_secs")) {
    for (value in list(NULL, NA, Inf, 1.5, "90", TRUE, numeric(), c(1, 2), list(3))) {
      x <- settings_fixture()
      x[field] <- list(value)
      expect_type(validate_settings(x), "character")
    }
  }
  for (value in list(NULL, "topic", list(NULL), rep(list(list()), 7))) {
    x <- settings_fixture()
    x["topics"] <- list(value)
    expect_type(validate_settings(x), "character")
  }
  for (value in list(NA, list(list("A")), 12)) {
    x <- settings_fixture()
    x$participants <- value
    expect_type(validate_settings(x), "character")
  }
})

test_that("unknown keys warn while portable settings round-trip exactly", {
  x <- settings_fixture()
  x$future_feature <- TRUE
  expect_warning(config <- validate_settings(x), "Unbekannte Einstellungen")
  expect_s3_class(config, "bw_settings")
  path <- withr::local_tempfile(fileext = ".yml")
  write_settings(config, path)
  expect_identical(parse_settings(path), config)
  config$participants <- c(" \u00c4nne ", "Bob")
  config$groups <- c("Nord", "S\u00fcd")
  normalized <- validate_settings(config)
  write_settings(normalized, path)
  expect_identical(parse_settings(path), normalized)
  expect_false(any(grepl("pin|secret", readLines(path), ignore.case = TRUE)))
  config$rounds <- 100
  expect_error(write_settings(config, path), "rounds:")
})

test_that("settings parser enforces size, YAML syntax and disables expressions", {
  expect_match(parse_settings("/does/not/exist.yml"), "nicht gefunden")
  expect_match(parse_settings(tempdir()), "h\u00f6chstens")
  path <- withr::local_tempfile(fileext = ".yml")
  writeLines(strrep("x", 100 * 1024 + 1), path)
  expect_match(parse_settings(path), "h\u00f6chstens")
  writeLines("topics: [", path)
  expect_match(parse_settings(path), "Syntax")
  writeLines("format: !expr stop('must not execute')", path)
  expect_type(suppressWarnings(parse_settings(path)), "character")
})

test_that("the planned number of participants is optional, bounded and round-trips", {
  x <- settings_fixture()
  expect_null(validate_settings(x)$expected_participants)
  x$expected_participants <- 15
  config <- validate_settings(x)
  expect_identical(config$expected_participants, 15L)
  file <- withr::local_tempfile(fileext = ".yml")
  write_settings(config, file)
  expect_identical(parse_settings(file), config)
  for (bad in list(0, 501, 2.5, "15")) {
    x$expected_participants <- bad
    expect_match(validate_settings(x), "expected_participants: muss zwischen 1 und 500",
                 all = FALSE)
  }
})
