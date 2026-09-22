test_that("weighting sliders keep each topic within 100 percent in the browser", {
  modes_browser_ready()
  path <- plenum_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "plenum-sliders")
  app$run_js(sprintf("Shiny.setInputValue('stored_pid', '%s', {priority:'event'})", pid))
  app$wait_for_js("document.querySelectorAll('.bw-weight').length > 0")
  ids <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll('[data-topic=\"1\"] .bw-weight'))",
    ".map(x => Number(x.dataset.entry))"
  )))
  slider <- function(id) sprintf("document.querySelector('.bw-weight[data-entry=\"%s\"]')", id)
  move <- function(id, value) {
    app$run_js(sprintf(paste0(
      "var s = %s; s.value = %d; s.dispatchEvent(new Event('input', {bubbles: true}));",
      "s.dispatchEvent(new Event('change', {bubbles: true}));"
    ), slider(id), value))
  }
  move(ids[1], 70)
  move(ids[2], 60)
  expect_equal(app$get_js(paste0("Number(", slider(ids[2]), ".value)")), 30)
  remaining <- "document.querySelector('[data-topic=\"1\"] .bw-remaining').textContent"
  expect_identical(app$get_js(remaining), "Noch 0 % zu vergeben")
  for (i in 1:50) {
    if (sum(read_table(path, "votes")$points) == 100) break
    Sys.sleep(0.1)
  }
  votes <- read_table(path, "votes")
  expect_equal(votes$points[match(ids[1:2], votes$entry_id)], c(70, 30))
  app$run_js(paste0(
    "document.querySelector('#bw-language-slider').value='2'; ",
    "document.querySelector('#bw-language-slider').dispatchEvent(",
    "new Event('change',{bubbles:true}));"
  ))
  app$wait_for_js(paste(remaining, "=== '0 % left to distribute'"))
  move(ids[1], 50)
  app$wait_for_js(paste(remaining, "=== '20 % left to distribute'"))
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
})

test_that("screen readers get a distinct, meaningful name for every slider", {
  modes_browser_ready()
  path <- plenum_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "plenum-a11y")
  app$run_js(sprintf("Shiny.setInputValue('stored_pid', '%s', {priority:'event'})", pid))
  app$wait_for_js("document.querySelectorAll('.bw-weight').length > 0")
  tree <- app$get_chromote_session()$Accessibility$getFullAXTree()$nodes
  sliders <- Filter(function(n) identical(n$role$value, "slider"), tree)
  names <- vapply(sliders, function(n) n$name$value %||% "", "")
  names <- setdiff(names, c("Sprache", "Language", "Langue"))  # the language control
  texts <- plenum_items(path, pid)$text
  expect_length(names, length(texts))
  expect_false(anyDuplicated(names) > 0)
  for (text in texts) expect_true(any(grepl(text, names, fixed = TRUE)), label = text)
  app$run_js(paste0(
    "var s = document.querySelector('.bw-weight'); s.value = 35;",
    "s.dispatchEvent(new Event('input', {bubbles: true}));"
  ))
  value_text <- "document.querySelector('.bw-weight').getAttribute('aria-valuetext')"
  expect_identical(app$get_js(value_text), "35 %")
})

test_that("a voting page follows weights changed in another tab", {
  modes_browser_ready()
  path <- plenum_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  items <- plenum_items(path, pid)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 500)
  app <- modes_browser_app(cfg, "plenum-sync")
  app$run_js(sprintf("Shiny.setInputValue('stored_pid', '%s', {priority:'event'})", pid))
  app$wait_for_js("document.querySelectorAll('.bw-weight').length > 0")
  set_weight(path, pid, items$id[1], 75)
  app$wait_for_js(sprintf(
    "Number(document.querySelector('.bw-weight[data-entry=\"%d\"]').value) === 75", items$id[1]
  ), timeout = 10000)
  remaining <- sprintf("document.querySelector('#bw-remaining-%d').textContent", items$topic_id[1])
  expect_identical(app$get_js(remaining), "Noch 25 % zu vergeben")
})

test_that("a drag released at its starting value does not detach the slider from sync", {
  modes_browser_ready()
  path <- plenum_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  items <- plenum_items(path, pid)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 500)
  app <- modes_browser_app(cfg, "plenum-release")
  app$run_js(sprintf("Shiny.setInputValue('stored_pid', '%s', {priority:'event'})", pid))
  app$wait_for_js("document.querySelectorAll('.bw-weight').length > 0")
  slider <- sprintf("document.querySelector('.bw-weight[data-entry=\"%d\"]')", items$id[1])
  # Moving and returning to the same value fires input events but no change event.
  app$run_js(paste0(
    "var s = ", slider, "; s.dispatchEvent(new PointerEvent('pointerdown', {bubbles: true}));",
    "s.value = 40; s.dispatchEvent(new Event('input', {bubbles: true}));",
    "s.value = 0; s.dispatchEvent(new Event('input', {bubbles: true}));",
    "s.dispatchEvent(new PointerEvent('pointerup', {bubbles: true}));"
  ))
  set_weight(path, pid, items$id[1], 65)
  app$wait_for_js(paste0("Number(", slider, ".value) === 65"), timeout = 10000)
  expect_false(app$get_js(paste0("!!", slider, ".dataset.moving")))
})
