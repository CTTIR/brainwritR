test_that("one device keeps separate identities per session and moderators stay signed in", {
  modes_browser_ready()
  x <- multi_fixture()
  new_db(x$main)
  code <- create_session(x$main, "Kurs Browser")
  path <- session_path(x$main, code)
  new_db(path)
  cfg <- app_config(x$main, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "sessions-identity")
  app$wait_for_js("!!document.querySelector('#join_name')")
  app$set_inputs(join_name = "Standard")
  app$click("join_btn")
  app$wait_for_js("document.body.innerText.includes('Hallo Standard')")
  standard <- app$get_js("localStorage.getItem('bw_pid')")
  expect_identical(read_table(x$main, "participants")$pid, standard)

  app$run_js(sprintf("location.search = '?s=%s'", code))
  app$wait_for_js("!!document.querySelector('#join_name')")
  restore_tracer <- system.file("internal/js/shiny-tracer.js", package = "shinytest2")
  app$run_js(paste(readLines(restore_tracer, warn = FALSE), collapse = "\n"))
  app$wait_for_js("window.shinytest2 && window.shinytest2.ready")
  app$wait_for_idle()
  app$set_inputs(join_name = "Kurs")
  app$click("join_btn")
  app$wait_for_js("document.body.innerText.includes('Hallo Kurs')")
  coded <- app$get_js(sprintf("localStorage.getItem('bw_pid:%s')", code))
  expect_identical(read_table(path, "participants")$pid, coded)
  expect_identical(app$get_js("localStorage.getItem('bw_pid')"), standard)

  modes_browser_moderator(app)
  app$wait_for_js("document.body.innerText.includes('Alle Sessions')")
  app$run_js(sprintf("location.search = '?mod=1&s=%s'", code))
  app$wait_for_js("!!document.body && document.body.innerText.includes('Kurs Browser')",
                  timeout = 15000)
  # The moderator bar renders before the session view; wait for its controls.
  app$wait_for_js("!!document.querySelector('#start_btn')")
  expect_false(app$get_js("!!document.querySelector('#pin')"))

  app$run_js(sprintf("location.search = '?s=%s'", code))
  app$wait_for_js("!!document.body && document.body.innerText.includes('Hallo Kurs')",
                  timeout = 15000)
  expect_null(delete_session(x$main, code))
  app$wait_for_js("!!document.body && document.body.innerText.includes('Session nicht gefunden')",
                  timeout = 15000)
  expect_false(file.exists(path))
})

test_that("a reconnecting participant returns straight to the sheet without the join form", {
  modes_browser_ready()
  path <- new_db(withr::local_tempfile(), n = 2)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "reconnect-sheet")
  app$wait_for_js("!!document.querySelector('#join_name')")
  app$set_inputs(join_name = "Mobil")
  app$click("join_btn")
  app$wait_for_js("document.body.innerText.includes('Hallo Mobil')")
  start_session(path)
  app$wait_for_js("!!document.querySelector('#a1')")
  # Watch every new page from its first byte: did the join form ever show?
  app$get_chromote_session()$Page$enable()
  app$get_chromote_session()$Page$addScriptToEvaluateOnNewDocument(source = paste0(
    "window.bwLoad = Number(sessionStorage.getItem('bwLoads') || 0) + 1;",
    "sessionStorage.setItem('bwLoads', String(window.bwLoad));",
    "new MutationObserver(function() {",
    "  if (document.querySelector('#join_name')) window.bwJoinSeen = true;",
    "}).observe(document, {subtree: true, childList: true});"
  ))
  app$run_js("location.reload()")
  app$wait_for_js(paste0("window.bwLoad === 1 && document.readyState === 'complete' && ",
                         "!!document.querySelector('#a1')"), timeout = 15000)
  expect_false(isTRUE(app$get_js("window.bwJoinSeen")))
})
