modes_browser_ready <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  chrome <- suppressWarnings(chromote::find_chrome())
  testthat::skip_if(is.null(chrome) || !nzchar(chrome), "Chrome is unavailable")
}

modes_browser_app <- function(cfg, name) {
  directory <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_envvar(c(TMPDIR = directory), .local_envir = parent.frame())
  previous <- if (chromote::has_default_chromote_object()) {
    chromote::default_chromote_object()
  } else {
    NULL
  }
  browser <- tryCatch(chromote::Chromote$new(), error = function(e) {
    testthat::skip(paste("Chrome cannot start:", conditionMessage(e)))
  })
  chromote::set_default_chromote_object(browser)
  app <- NULL
  withr::defer({
    if (!is.null(app)) app$stop()
    browser$close()
    if (!is.null(previous)) chromote::set_default_chromote_object(previous)
  }, envir = parent.frame())
  saveRDS(cfg, file.path(directory, "config.rds"))
  source_root <- normalizePath(testthat::test_path("../.."))
  loader <- "library(brainwritR)"
  if (file.exists(file.path(source_root, "R", "server.R")) &&
        requireNamespace("pkgload", quietly = TRUE)) {
    loader <- sprintf("pkgload::load_all(%s, quiet = TRUE)", dQuote(source_root, FALSE))
  }
  writeLines(c(loader, 'cfg <- readRDS("config.rds")',
               "shiny::shinyApp(brainwritR:::app_ui(cfg), brainwritR:::app_server(cfg))"),
             file.path(directory, "app.R"))
  app <- shinytest2::AppDriver$new(directory, name = name, width = 390, height = 844,
                                   timeout = 15000)
  app
}

modes_browser_moderator <- function(app) {
  app$run_js("location.search = '?mod=1'")
  app$wait_for_js("!!document.querySelector('#pin')")
  tracer <- system.file("internal/js/shiny-tracer.js", package = "shinytest2")
  app$run_js(paste(readLines(tracer, warn = FALSE), collapse = "\n"))
  app$wait_for_js("window.shinytest2 && window.shinytest2.ready")
  app$wait_for_idle()
  app$set_inputs(pin = "secret")
  app$click("pin_btn")
}

modes_browser_capture <- function(app, name) {
  directory <- Sys.getenv("BRAINWRITR_SCREENSHOTS", "")
  if (!nzchar(directory)) return(invisible(NULL))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  app$get_screenshot(file.path(directory, paste0(name, ".png")))
}
