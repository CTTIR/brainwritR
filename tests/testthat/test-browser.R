browser_ready <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  chrome <- suppressWarnings(chromote::find_chrome())
  testthat::skip_if(is.null(chrome) || !nzchar(chrome), "Chrome is unavailable")
}

new_browser_app <- function(cfg, name) {
  directory <- withr::local_tempdir(.local_envir = parent.frame())
  saveRDS(cfg, file.path(directory, "config.rds"))
  writeLines(c(
    "library(brainwritR)",
    'cfg <- readRDS("config.rds")',
    "shiny::shinyApp(brainwritR:::app_ui(cfg), brainwritR:::app_server(cfg))"
  ), file.path(directory, "app.R"))
  app <- shinytest2::AppDriver$new(directory, name = name, width = 390, height = 844,
                                   timeout = 10000)
  app
}

restore_browser_tracer <- function(app) {
  tracer <- system.file("internal/js/shiny-tracer.js", package = "shinytest2")
  app$run_js(paste(readLines(tracer, warn = FALSE), collapse = "\n"))
  app$wait_for_js("window.shinytest2 && window.shinytest2.ready")
  app$wait_for_idle()
}

test_that("mobile participant text survives polls and reconnect", {
  browser_ready()
  path <- new_db(withr::local_tempfile(), n = 2)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  app <- new_browser_app(cfg, "participant")
  on.exit(app$stop(), add = TRUE)
  app$wait_for_js("!!document.querySelector('#join_name')")
  expect_match(app$get_js("document.body.innerText"), "Teilnehmen")
  app$set_inputs(join_name = "Mobil")
  app$click("join_btn")
  app$wait_for_js("document.body.innerText.includes('Hallo Mobil')")
  expect_equal(nrow(read_table(path, "participants")), 3)
  pid <- app$get_js("localStorage.getItem('bw_pid')")
  expect_true(pid %in% read_table(path, "participants")$pid)
  start_session(path)
  app$wait_for_js("!!document.querySelector('#a1')")
  app$run_js("window.originalTextarea = document.querySelector('#a1')")
  app$set_inputs(a1 = "Eine mobile Idee", a2 = "Ein zweiter Ansatz")
  Sys.sleep(3)
  app$wait_for_idle()
  expect_equal(
    read_table(path, "entries")$text[read_table(path, "entries")$question == 1],
    "Eine mobile Idee"
  )
  expect_true(app$get_js("document.querySelector('#a1') === window.originalTextarea"))
  expect_equal(app$get_js("document.querySelector('#a1').value"), "Eine mobile Idee")
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
  expect_equal(app$get_js("document.documentElement.lang"), "de")
  app$click("submit_btn")
  app$wait_for_js("document.body.innerText.includes('Abgegeben')")
  app$set_inputs(a1 = "Nach Abgabe bearbeitet")
  Sys.sleep(3)
  app$wait_for_idle()
  expect_true(all(read_table(path, "entries")$submitted == 1))
  # Trigger the exact mobile-disconnect recovery handler.
  app$run_js("$(document).trigger('shiny:disconnected')")
  app$wait_for_js("!window.originalTextarea && !!document.querySelector('#a1')",
    timeout = 15000
  )
  restore_browser_tracer(app)
  expect_equal(app$get_js("document.querySelector('#a1').value"), "Nach Abgabe bearbeitet")
  expect_equal(nrow(read_table(path, "participants")), 3)
  expect_identical(app$get_js("localStorage.getItem('bw_pid')"), pid)
  expect_false(app$get_js(paste0(
    "performance.getEntriesByType('resource').some(x => ",
    "/fonts\\.googleapis|fonts\\.gstatic/.test(x.name))"
  )))
  # Two prior contributors sharing the next sheet must both remain visible.
  people <- read_table(path, "participants")
  me <- people[people$pid == pid, ]
  next_topic <- topic_for(me$grp, 2, 3)
  next_sheet <- sheet_for(me$idx, read_table(path, "topics")$n_sheets[next_topic])
  other <- people$pid[people$pid != pid]
  save_entry(path, other[1], next_topic, next_sheet, 1, 1, "Erster Vorbeitrag")
  save_entry(path, other[2], next_topic, next_sheet, 1, 1,
             "<img src=x onerror=window.bwInjected=true> Zweiter Vorbeitrag")
  # No moderator client is connected: the participant's polls advance the clock.
  exec_sql(path, "UPDATE session SET round_ends_at=0")
  app$wait_for_js("document.body.innerText.includes('Runde 2/3')")
  expect_equal(read_table(path, "session")$current_round, 2)
  app$wait_for_js("document.body.innerText.includes('Erster Vorbeitrag')")
  expect_match(app$get_js("document.body.innerText"), "Zweiter Vorbeitrag")
  expect_false(app$get_js("window.bwInjected === true"))
  expect_equal(app$get_js("document.querySelector('#a1').value"), "")
})

test_that("moderator route requires PIN and reset clears stale identities", {
  browser_ready()
  path <- new_db(withr::local_tempfile(), n = 3)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  app <- new_browser_app(cfg, "moderator")
  on.exit(app$stop(), add = TRUE)
  app$run_js("location.search = '?mod=1'")
  app$wait_for_js("!!document.querySelector('#pin')")
  restore_browser_tracer(app)
  app$set_inputs(pin = "wrong")
  app$click("pin_btn")
  app$wait_for_js("document.body.innerText.includes('Falsche PIN.')")
  expect_false(app$get_js("!!document.querySelector('#start_btn')"))
  app$set_inputs(pin = "secret")
  app$click("pin_btn")
  app$wait_for_js("!!document.querySelector('#start_btn')")
  app$click("start_btn")
  app$wait_for_js("!!document.querySelector('#next_btn')")
  before <- read_table(path, "session")$round_ends_at
  app$click("plus60_btn")
  # Browser clicks are asynchronous: await the committed deadline, then assert
  # the exact increment. A lost or duplicated action still fails this check.
  for (attempt in seq_len(100)) {
    if (identical(read_table(path, "session")$round_ends_at, before + 60)) break
    Sys.sleep(0.1)
  }
  expect_equal(read_table(path, "session")$round_ends_at, before + 60)
  for (r in 2:3) {
    app$click("next_btn")
    app$wait_for_js(sprintf("document.body.innerText.includes('Runde %s von 3')", r))
  }
  app$click("next_btn")
  app$wait_for_js("!!document.querySelector('#reset_btn')")
  app$wait_for_js("!!document.querySelector('#dl_csv').getAttribute('href')")
  csv_file <- app$get_download("dl_csv")
  expect_named(utils::read.csv(csv_file), c("Thema", "Bogen", "Runde", "Frage",
                                            "Teilnehmer", "Beitrag", "abgegeben", "Zeit"))
  app$wait_for_js("!!document.querySelector('#dl_md').getAttribute('href')")
  md_file <- app$get_download("dl_md")
  expect_match(paste(readLines(md_file), collapse = "\n"), "Brainwriting 6-3-5")
  app$click("reset_btn")
  expect_equal(nrow(read_table(path, "participants")), 3)
  app$wait_for_js("!!document.querySelector('#reset_confirm.shiny-bound-input')")
  app$click("reset_confirm")
  app$wait_for_js("!!document.querySelector('#setup_save')")
  expect_equal(nrow(read_table(path, "participants")), 0)
  app$run_js("localStorage.setItem('bw_pid','dangling'); location.search = ''")
  app$wait_for_js("localStorage.getItem('bw_pid') === null")
  expect_identical(read_table(path, "session")$status, "setup")
})
