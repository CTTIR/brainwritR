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
  app$wait_for_js("!document.querySelector('#t_title_3')")
  app$set_inputs(t_title_1 = "Custom prior topic", t_q1_1 = "Custom prior question?",
                 t_q2_1 = "Custom prior second question?", t_title_2 = "Second custom topic",
                 t_q1_2 = "Another prior question?", t_q2_2 = "Another second question?")
  app$set_inputs(demo_questions = TRUE)
  app$wait_for_js("Number(document.querySelector('#n_groups').value) === 3")
  app$wait_for_js("!!document.querySelector('#t_title_3')")
  app$wait_for_js("document.querySelector('#t_title_1').value !== 'Custom prior topic'")
  first <- app$get_js("document.querySelector('#t_title_1').value")
  expect_equal(first, "Learning together")
  expect_equal(app$get_js("document.querySelector('#t_q1_1').value"),
               "What helps us learn from one another?")
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
