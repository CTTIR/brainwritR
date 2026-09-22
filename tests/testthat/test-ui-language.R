test_that("a dark mode toggle sits next to the language control", {
  html <- as.character(app_ui(app_config(tempfile(), "x", "http://localhost:3838")))
  expect_match(html, 'id="bw-theme-toggle"', fixed = TRUE)
  expect_match(html, 'aria-label="Dunkles Design"', fixed = TRUE)
  expect_match(app_css(), "[data-bs-theme='dark']", fixed = TRUE)
  js <- app_js()
  expect_match(js, "bw_theme", fixed = TRUE)
  expect_match(js, "prefers-color-scheme", fixed = TRUE)
  expect_match(app_css(), "[data-bs-theme='dark'] .form-range::-webkit-slider-thumb", fixed = TRUE)
  expect_match(app_css(), "[data-bs-theme='dark'] .form-range::-moz-range-thumb", fixed = TRUE)
  expect_true("Dunkles Design" %in% bw_language_dictionary()$de)
  # A toggle button keeps its label; only its pressed state changes.
  expect_false(grepl("Helles Design", js, fixed = TRUE))
})

test_that("the footer credits CTTIR with a GitHub link and an inline icon", {
  html <- as.character(app_ui(app_config(tempfile(), "x", "http://localhost:3838")))
  footer <- regmatches(html, regexpr("<footer[\\s\\S]*</footer>", html, perl = TRUE))
  expect_match(footer, 'href="https://github.com/CTTIR/brainwritR"', fixed = TRUE)
  expect_match(footer, "CTTIR", fixed = TRUE)
  expect_match(footer, "<svg", fixed = TRUE)
  expect_false(grepl("Pseudonyme willkommen", html, fixed = TRUE))
  expect_false(grepl("font-awesome|fontawesome", html))
})

test_that("language changes preserve focused drafts and never translate course content", {
  modes_browser_ready()
  path <- new_db(withr::local_tempfile(), n = 3)
  exec_sql(path, "UPDATE topics SET title = 'Abgeben', q1 = 'Teilnehmen', q2 = 'Runde beenden'")
  start_session(path)
  people <- read_table(path, "participants")
  me <- people[1, ]
  topic <- topic_for(me$grp, 2, 3)
  save_entry(path, people$pid[2], topic, me$idx, 1, 1, "Ergebnisse", 1)
  maybe_advance(path, TRUE)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "language-draft")
  app$run_js(sprintf("Shiny.setInputValue('stored_pid', '%s', {priority:'event'})", me$pid))
  app$wait_for_js("!!document.querySelector('#a1')")
  app$wait_for_js("document.body.innerText.includes('Ergebnisse')")
  expect_equal(app$get_js("document.documentElement.lang"), "de")
  app$set_inputs(a1 = "Abgeben und Ergebnisse: my own words")
  app$run_js(paste0(
    "window.originalDraft = document.querySelector('#a1'); ",
    "originalDraft.focus(); originalDraft.setSelectionRange(7, 12);"
  ))
  for (language in c("en", "fr", "de")) {
    app$run_js(sprintf(paste0(
      "document.querySelector('#bw-language-slider').value='%s'; ",
      "document.querySelector('#bw-language-slider').dispatchEvent(",
      "new Event('change',{bubbles:true}));"
    ), match(language, c("de", "en", "fr"))))
    app$wait_for_js(sprintf("document.documentElement.lang === '%s'", language))
    expected <- switch(language, en = "Submit", fr = "Envoyer", de = "Abgeben")
    app$wait_for_js(sprintf("document.querySelector('#submit_btn').textContent.includes('%s')",
                            expected))
    expect_true(app$get_js("document.querySelector('#a1') === window.originalDraft"))
    expect_equal(app$get_js("document.querySelector('#a1').value"),
                 "Abgeben und Ergebnisse: my own words")
    expect_equal(app$get_js("document.querySelector('#a1').selectionStart"), 7)
    expect_equal(app$get_js("document.querySelector('#a1').selectionEnd"), 12)
    expect_equal(app$get_js("document.querySelector('h3[translate=no]').textContent"), "Abgeben")
    expect_equal(app$get_js("document.querySelector('label[for=a1] [translate=no]').textContent"),
                 "Teilnehmen")
    expect_true(app$get_js(paste0(
      "Array.from(document.querySelectorAll('[translate=no]')).some(",
      "x => x.textContent === 'Ergebnisse')"
    )))
  }
  app$run_js(paste0(
    "document.querySelector('#bw-language-slider').value='2'; ",
    "document.querySelector('#bw-language-slider').dispatchEvent(",
    "new Event('change',{bubbles:true}));"
  ))
  app$wait_for_js("document.querySelector('#submit_btn').textContent.includes('Submit')")
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
})

test_that("newly mounted moderator forms inherit the selected interface language", {
  modes_browser_ready()
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "language-dynamic-form")
  app$run_js(paste0(
    "document.querySelector('#bw-language-slider').value='2'; ",
    "document.querySelector('#bw-language-slider').dispatchEvent(",
    "new Event('change',{bubbles:true}));"
  ))
  app$wait_for_js("document.documentElement.lang === 'en'")
  modes_browser_moderator(app)
  app$wait_for_js("document.body.innerText.includes('Prepare session')")
  app$wait_for_js(
    "document.querySelector('#setup_save').textContent.includes('Save and open lobby')"
  )
  expect_match(app$get_js("document.querySelector('#setup_save').textContent"),
               "Save and open lobby")
  app$set_inputs(n_groups = 4)
  app$wait_for_js("!!document.querySelector('#t_title_4')")
  app$wait_for_js("document.body.innerText.includes('Topic 4')")
  expect_equal(app$get_js("document.querySelector('label[for=t_title_4]').textContent"),
               "Topic title")
  expect_equal(app$get_js("document.documentElement.lang"), "en")
})

test_that("English example questions remain editable and disabling restores prior setup", {
  modes_browser_ready()
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "language-demo")
  modes_browser_moderator(app)
  app$wait_for_js("!!document.querySelector('#demo_questions')")
  app$run_js(paste0(
    "document.querySelector('#bw-language-slider').value='2'; ",
    "document.querySelector('#bw-language-slider').dispatchEvent(",
    "new Event('change',{bubbles:true}));"
  ))
  app$wait_for_js("document.documentElement.lang === 'en'")
  app$set_inputs(n_groups = 2)
  app$wait_for_js(paste0(
    "!document.querySelector('#t_title_3') && ",
    "['t_title_1','t_title_2','t_q1_1','t_q2_1','t_q1_2','t_q2_2'].every(",
    "id => document.getElementById(id)?.classList.contains('shiny-bound-input'))"
  ))
  app$set_inputs(t_title_1 = "Custom prior topic", t_q1_1 = "Custom prior question?",
                 t_q2_1 = "Custom prior second question?", t_title_2 = "Second custom topic",
                 t_q1_2 = "Another prior question?", t_q2_2 = "Another second question?")
  app$set_inputs(demo_questions = TRUE)
  app$wait_for_js("Number(document.querySelector('#n_groups').value) === 3")
  app$wait_for_js("!!document.querySelector('#t_title_3')")
  app$wait_for_js("document.querySelector('#t_title_1').value !== 'Custom prior topic'")
  first <- app$get_js("document.querySelector('#t_title_1').value")
  expect_equal(first, "More participation in group work")
  expect_equal(app$get_js("document.querySelector('#t_q1_1').value"),
               example_topics("en")[[1]]$q1)
  app$wait_for_js("!!document.querySelector('#t_q1_1.shiny-bound-input')")
  app$set_inputs(t_q1_1 = "Our edited English example question?")
  app$run_js(paste0(
    "document.querySelector('#bw-language-slider').value='3'; ",
    "document.querySelector('#bw-language-slider').dispatchEvent(",
    "new Event('change',{bubbles:true}));"
  ))
  app$wait_for_js("document.documentElement.lang === 'fr'")
  expect_equal(app$get_js("document.querySelector('#t_q1_1').value"),
               "Our edited English example question?")
  expect_equal(app$get_js("document.querySelector('#t_title_1').value"), first)
  app$set_inputs(demo_questions = FALSE)
  app$wait_for_js("Number(document.querySelector('#n_groups').value) === 2")
  app$wait_for_js("document.querySelector('#t_title_1').value === 'Custom prior topic'")
  expect_equal(app$get_js("document.querySelector('#t_q1_1').value"), "Custom prior question?")
  expect_equal(app$get_js("document.querySelector('#t_q2_2').value"), "Another second question?")
  expect_false(app$get_js("!!document.querySelector('#t_title_3')"))
  expect_identical(read_table(path, "session")$status, "setup")
})

test_that("analytics selectors preserve topic names that match translated interface labels", {
  modes_browser_ready()
  path <- new_db(withr::local_tempfile(), n = 3)
  exec_sql(path, "UPDATE topics SET title = 'Abgeben' WHERE id = 1")
  start_session(path)
  for (i in 1:3) maybe_advance(path, force = TRUE)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "language-analytics-topic")
  modes_browser_moderator(app)
  app$wait_for_js("!!document.querySelector('a[data-value=\"Auswertung\"]')")
  app$run_js(paste0(
    "document.querySelector('#bw-language-slider').value='2'; ",
    "document.querySelector('#bw-language-slider').dispatchEvent(",
    "new Event('change',{bubbles:true}));"
  ))
  app$wait_for_js("document.documentElement.lang === 'en'")
  app$run_js("document.querySelector('a[data-value=\"Auswertung\"]').click()")
  app$wait_for_js("!!document.querySelector('#analytics_topic')?.selectize")
  app$wait_for_js(paste0(
    "document.querySelector('.selectize-input [data-value=\"all\"]')",
    "?.textContent === 'All topics'"
  ))
  app$run_js("document.querySelector('#analytics_topic').selectize.open()")
  app$wait_for_js("!!document.querySelector('.selectize-dropdown [data-value=\"1\"]')")
  expect_equal(app$get_js(paste0(
    "document.querySelector('.selectize-dropdown [data-value=\"1\"]').textContent"
  )), "Abgeben")
  app$run_js("document.querySelector('.selectize-dropdown [data-value=\"1\"]').click()")
  app$wait_for_js("!!document.querySelector('.selectize-input [data-value=\"1\"]')")
  expect_equal(app$get_js(paste0(
    "document.querySelector('.selectize-input [data-value=\"1\"]').textContent"
  )), "Abgeben")
  expect_equal(app$get_js("document.querySelector('#analytics_topic option[value=\"1\"]').text"),
               "Abgeben")
  expect_identical(read_table(path, "topics")$title[1], "Abgeben")
})

test_that("the dark mode toggle switches colours in place and is remembered", {
  modes_browser_ready()
  path <- new_db(withr::local_tempfile(), n = 2)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "dark-mode")
  app$wait_for_js("!!document.querySelector('#join_name')")
  # Do not depend on the runner's colour scheme: start from an explicit light one.
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-color-scheme", value = "light"))
  )
  app$run_js("window.originalJoin = document.querySelector('#join_name')")
  background <- "getComputedStyle(document.body).backgroundColor"
  light <- app$get_js(background)
  app$run_js("document.querySelector('#bw-theme-toggle').click()")
  app$wait_for_js("document.documentElement.getAttribute('data-bs-theme') === 'dark'")
  expect_false(identical(app$get_js(background), light))
  expect_true(app$get_js("document.querySelector('#join_name') === window.originalJoin"))
  expect_identical(app$get_js("localStorage.getItem('bw_theme')"), "dark")
  toggle <- "document.querySelector('#bw-theme-toggle')"
  expect_identical(app$get_js(paste0(toggle, ".getAttribute('aria-label')")), "Dunkles Design")
  expect_identical(app$get_js(paste0(toggle, ".getAttribute('aria-pressed')")), "true")
  app$run_js("location.reload()")
  app$wait_for_js(paste0("!!document.querySelector('#join_name') && ",
                         "document.documentElement.getAttribute('data-bs-theme') === 'dark'"),
                  timeout = 15000)
  app$run_js("document.querySelector('#bw-theme-toggle').click()")
  app$wait_for_js("document.documentElement.getAttribute('data-bs-theme') === 'light'")
  expect_identical(app$get_js(background), light)
  expect_true(app$get_js("document.documentElement.scrollWidth <= window.innerWidth"))
})

test_that("without a stored choice the theme follows the device preference", {
  modes_browser_ready()
  path <- new_db(withr::local_tempfile(), n = 2)
  cfg <- app_config(path, "secret", "http://localhost:3838")
  app <- modes_browser_app(cfg, "dark-preference")
  app$wait_for_js("!!document.querySelector('#join_name')")
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-color-scheme", value = "dark"))
  )
  theme <- "document.documentElement.getAttribute('data-bs-theme')"
  app$run_js("localStorage.removeItem('bw_theme'); location.reload()")
  app$wait_for_js(paste0("!!document.querySelector('#join_name') && ", theme, " === 'dark'"),
                  timeout = 15000)
  expect_identical(app$get_js(
    "document.querySelector('#bw-theme-toggle').getAttribute('aria-pressed')"
  ), "true")
  app$run_js("localStorage.setItem('bw_theme', 'light'); location.reload()")
  app$wait_for_js(paste0("!!document.querySelector('#join_name') && ", theme, " === 'light'"),
                  timeout = 15000)
})
