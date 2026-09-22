test_that("random identifiers use the requested alphabet and never touch the R seed", {
  set.seed(1)
  before <- .Random.seed
  alphabet <- c("a", "b", "c")
  x <- bw_random_chars(500, alphabet)
  expect_identical(.Random.seed, before)
  expect_identical(nchar(x), 500L)
  expect_setequal(strsplit(x, "")[[1]], alphabet)
  expect_match(bw_participant_id(), "^[a-z0-9]{14}$")
  expect_match(bw_session_code(), "^[2-9abcdefghjkmnpqrstuvwxyz]{6}$")
  expect_match(bw_token(), "^[A-Za-z0-9]{32}$")
  expect_error(bw_random_chars(0, alphabet), "n")
})

test_that("participant identities stay unpredictable after the R seed is fixed", {
  path <- new_db(withr::local_tempfile())
  set.seed(635)
  first <- add_participant(path, "A")
  set.seed(635)
  second <- add_participant(path, "B")
  expect_false(identical(first, second))
})

test_that("drawing wordclouds in the app or the report keeps the global seed", {
  path <- new_db(withr::local_tempfile(), n = 3)
  start_session(path)
  p <- read_table(path, "participants")
  for (r in 1:3) {
    for (i in 1:3) {
      topic <- topic_for(p$grp[i], r, 3)
      save_entry(path, p$pid[i], topic, 1, r, 1, "Ideen sammeln Ideen teilen Ideen", 1)
    }
    maybe_advance(path, TRUE)
  }
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1, analytics_topic = "all",
                      min_cooc = 2, exclude_prompt = TRUE)
    set.seed(99)
    before <- .Random.seed
    expect_true(nzchar(output$fig_wordcloud$src))
    expect_identical(.Random.seed, before)
  })
  skip_if_not(capabilities("cairo"))
  set.seed(99)
  before <- .Random.seed
  build_report(collect_data(path), withr::local_tempfile(fileext = ".pdf"))
  expect_identical(.Random.seed, before)
})
