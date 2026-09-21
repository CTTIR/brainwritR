report_fixture <- function(k = 3L, empty = FALSE) {
  topics <- data.frame(
    id = seq_len(k), title = paste("Thema", seq_len(k)),
    q1 = "Was hilft?", q2 = "Wie gelingt es?", n_sheets = 1L
  )
  entries <- expand.grid(topic_id = seq_len(k), sheet = 1L, round = 1:3, question = 1:2)
  entries$id <- seq_len(nrow(entries))
  entries$pid <- "test-pid"
  entries$text <- "Gemeinsam kreativ lernen gemeinsam kreativ arbeiten"
  entries$submitted <- 1L
  entries$updated_at <- 1000
  if (empty) entries <- entries[FALSE, ]
  list(
    session = data.frame(
      id = 1L, mode = "individual", n_groups = k, n_rounds = 3L,
      round_secs = 300L, current_round = 3L, status = "finished"
    ),
    topics = topics,
    participants = data.frame(
      pid = "test-pid", name = "FixturePerson",
      grp = 1L, idx = 1L, joined_at = 1000
    ),
    entries = entries, generated_at = as.POSIXct("2026-01-01", tz = "UTC"),
    package_version = "0.3.0"
  )
}

test_that("populated and empty reports render exactly two A4 pages", {
  skip_if_not(capabilities("cairo"))
  for (empty in c(FALSE, TRUE)) {
    file <- withr::local_tempfile(fileext = ".pdf")
    expect_no_warning(build_report(report_fixture(empty = empty), file))
    expect_true(file.exists(file))
    expect_gt(file.info(file)$size, 1000)
    if (requireNamespace("pdftools", quietly = TRUE)) {
      expect_equal(pdftools::pdf_info(file)$pages, 2L)
      text <- pdftools::pdf_text(file)
      expect_match(text[1], "Ergebnisbericht")
      expect_match(text[2], "brainwritR 0.3.0", fixed = TRUE)
      expect_false(any(grepl("FixturePerson", text, fixed = TRUE)))
    }
  }
})

test_that("report handles six topics, legacy session and hot seat", {
  skip_if_not(capabilities("cairo"))
  file <- withr::local_tempfile(fileext = ".pdf")
  fixture <- report_fixture(k = 6L)
  fixture$session$mode <- NULL
  expect_no_warning(build_report(fixture, file))
  fixture$session$mode <- "hot_seat"
  fixture$settings <- list(turn_secs = 45L)
  expect_no_warning(build_report(fixture, file))
  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_equal(pdftools::pdf_info(file)$pages, 2L)
    expect_match(pdftools::pdf_text(file)[1], "45 Sekunden", fixed = TRUE)
  }
})

test_that("report numeric labels and empty table are explicit", {
  expect_identical(bw_report_number(c(NA, NaN, Inf)), rep("\u2014", 3))
  expect_identical(
    bw_report_number(c(0, 0.25, 1), percent = TRUE),
    c("0,0 %", "25,0 %", "100,0 %")
  )
  fixture <- report_fixture(empty = TRUE)
  expect_s3_class(bw_report_table(fixture$entries, fixture$topics), "ggplot")
  expect_s3_class(bw_report_table(fixture$entries, fixture$topics[FALSE, ]), "ggplot")
  expect_s3_class(bw_report_text(character()), "ggplot")
})

test_that("pseudonymized group reports preserve statistics and omit identities", {
  skip_if_not(capabilities("cairo"))
  fixture <- report_fixture()
  fixture$session$mode <- "group_device"
  file <- withr::local_tempfile(fileext = ".pdf")
  expect_no_warning(build_report(fixture, file, anonymize = TRUE))
  expect_identical(fixture$participants$name, "FixturePerson")
  skip_if_not_installed("pdftools")
  expect_equal(pdftools::pdf_info(file)$pages, 2L)
  text <- pdftools::pdf_text(file)
  expect_false(any(grepl("FixturePerson|test-pid", text)))
  expect_match(text[1], "1 Gruppen", fixed = TRUE)
  expect_match(text[2], "Inhalte & Vernetzung", fixed = TRUE)
})
