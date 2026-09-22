export_fixture <- function(path) {
  new_db(path, n = 3)
  start_session(path)
  p <- read_table(path, "participants")
  for (i in seq_len(nrow(p))) {
    save_entry(path, p$pid[i], p$grp[i], p$idx[i], 1, 1,
               paste("Ideen verbinden Lernen", i), submitted = 1)
    save_entry(path, p$pid[i], p$grp[i], p$idx[i], 1, 2, "")
  }
  collect_data(path)
}

test_that("snapshot exports preserve raw tables and existing CSV/Markdown contracts", {
  path <- withr::local_tempfile()
  data <- export_fixture(path)
  for (table in c("session", "topics", "participants", "entries")) {
    expect_identical(data[[table]], read_table(path, table))
  }
  expect_identical(export_snapshot_df(data), export_df(path))
  expect_identical(build_snapshot_md(data), build_md(path))
  expect_s3_class(data$generated_at, "POSIXct")
  file <- withr::local_tempfile(fileext = ".rds")
  write_rds(data, file)
  expect_identical(readRDS(file), data)
})

test_that("pseudonymization replaces names, ids, and preparatory identity echoes", {
  data <- export_fixture(withr::local_tempfile())
  data$settings$participants <- c(data$participants$name, "NotYetJoined")
  data$settings$groups <- c("SpecialName", "AnotherName", "FinalName")
  # Test future/migrated settings storage without depending on its implementation.
  data$session$settings_yaml <- yaml::as.yaml(unclass(data$settings))
  original <- data
  anon <- pseudonymize_data(data)
  expect_identical(data, original)
  expect_identical(anon$participants$name, sprintf("TN-%02d", 1:3))
  expect_identical(anon$participants$pid, anon$participants$name)
  expect_setequal(anon$entries$pid, anon$participants$pid)
  expect_equal(nrow(anon$entries), nrow(data$entries))
  forbidden <- c(data$participants$name, data$participants$pid,
                 "NotYetJoined", "SpecialName", "AnotherName", "FinalName")
  rendered <- paste(capture.output(dput(anon)), collapse = "\n")
  for (name in forbidden) expect_false(grepl(name, rendered, fixed = TRUE))
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(export_snapshot_df(anon), csv, row.names = FALSE)
  markdown <- build_snapshot_md(anon)
  for (name in forbidden) {
    expect_false(any(grepl(name, readLines(csv), fixed = TRUE)))
    expect_false(grepl(name, markdown, fixed = TRUE))
  }
  file <- withr::local_tempfile(fileext = ".rds")
  write_rds(anon, file)
  expect_identical(readRDS(file), anon)
  data$session$mode <- "group_device"
  expect_identical(pseudonymize_data(data)$participants$name,
                   sprintf("Gruppe-%02d", 1:3))
})

test_that("spreadsheet contains four sheets with all entry rows and no original identities", {
  data <- export_fixture(withr::local_tempfile())
  anon <- pseudonymize_data(data)
  file <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(anon, file)
  expect_identical(openxlsx::getSheetNames(file),
                   c("Beitr\u00e4ge", "Teilnehmer", "Themen", "Kennzahlen"))
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = 1)), nrow(data$entries))
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = 2)), nrow(data$participants))
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = 3)), nrow(data$topics))
  expect_equal(nrow(openxlsx::read.xlsx(file, sheet = 4, rows = 1:2)), 1)
  tables <- lapply(1:4, function(sheet) openxlsx::read.xlsx(file, sheet = sheet))
  text <- paste(unlist(tables), collapse = " ")
  for (name in c(data$participants$name, data$participants$pid)) {
    expect_false(grepl(name, text, fixed = TRUE))
  }
})

test_that("empty snapshots produce valid empty exports", {
  path <- withr::local_tempfile()
  init_db(path)
  data <- collect_data(path)
  expect_equal(nrow(export_snapshot_df(data)), 0)
  expect_match(build_snapshot_md(data), "Ergebnisse")
  expect_equal(nrow(pseudonymize_data(data)$entries), 0)
  file <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_warning(write_xlsx(data, file))
  expect_equal(length(openxlsx::getSheetNames(file)), 4)
})

test_that("snapshot timestamps preserve SQLite subsecond rounding", {
  path <- withr::local_tempfile()
  data <- export_fixture(path)
  con <- db(path)
  epoch <- c(1700000000.9994, 1700000000.9995, 1700000000.9999,
             NA_real_, 1700000000.5, 1700000001)
  for (i in seq_along(epoch)) {
    DBI::dbExecute(con, "UPDATE entries SET updated_at = ? WHERE id = ?",
                   params = list(epoch[i], data$entries$id[i]))
  }
  DBI::dbDisconnect(con)
  snapshot <- collect_data(path)
  expect_identical(export_snapshot_df(snapshot), export_df(path))
  expect_identical(export_snapshot_df(pseudonymize_data(snapshot))$Zeit, export_df(path)$Zeit)
})

test_that("an optional spreadsheet-safe CSV defuses formula-like cells", {
  x <- data.frame(Beitrag = c("=1+1", "+49 Idee", "-Liste", "@SUMME(A1)", "\tTab",
                              "\rCR", "normal", "a=b", NA), Runde = 1:9)
  safe <- spreadsheet_safe(x)
  expect_identical(safe$Beitrag, c("'=1+1", "'+49 Idee", "'-Liste", "'@SUMME(A1)", "'\tTab",
                                   "'\rCR", "normal", "a=b", NA))
  expect_identical(safe$Runde, x$Runde)
})

test_that("CSV downloads stay raw unless the moderator asks for safe cells", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  pid <- read_table(path, "participants")$pid[1]
  save_entry(path, pid, 1, 1, 1, 1, "=1+1", 1)
  for (i in 1:3) maybe_advance(path, TRUE)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_match(output$mod_view$html, "csv_safe", fixed = TRUE)
    raw <- utils::read.csv(output$dl_csv, check.names = FALSE)
    expect_identical(raw$Beitrag, "=1+1")
    session$setInputs(csv_safe = TRUE)
    safe <- utils::read.csv(output$dl_csv, check.names = FALSE)
    expect_identical(safe$Beitrag, "'=1+1")
  })
})

test_that("formula triggers after embedded separators or line breaks are defused too", {
  x <- data.frame(Beitrag = c("x;=1+1;y", "a,=1+1", "Zeile\n=1+1", "Zeile\r+2", "a - b",
                              "a;b", "\t=1"))
  expect_identical(spreadsheet_safe(x)$Beitrag,
                   c("x;'=1+1;y", "a,'=1+1", "Zeile\n'=1+1", "Zeile\r'+2", "a - b",
                     "a;b", "'\t'=1"))
})
