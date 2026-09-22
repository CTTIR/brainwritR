weigh <- function(session, entry, points) {
  session$setInputs(weight = list(entry = entry, points = points, nonce = runif(1)))
}

finished_fixture <- function(path) {
  new_db(path, n = 3)
  start_session(path)
  for (i in 1:3) maybe_advance(path, TRUE)
  plenum_fixture_entries(path)
  invisible(path)
}

test_that("participants weigh all contributions anonymously once the plenum opens", {
  path <- finished_fixture(withr::local_tempfile())
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  people <- read_table(path, "participants")
  e <- read_table(path, "entries")
  e <- e[nzchar(e$text), ]
  t1 <- e$id[e$topic_id == 1]
  session <- capturing_session()
  shiny::testServer(app_server(cfg), session = session, {
    session$setInputs(stored_pid = people$pid[1])
    session$elapse(100)
    expect_match(output$part_view$html, "Geschafft")
    open_plenum(path)
    session$elapse(100)
    html <- output$part_view$html
    expect_match(html, "Gewichtung", fixed = TRUE)
    expect_match(html, "Noch 100 % zu vergeben", fixed = TRUE)
    for (id in e$id) expect_match(html, sprintf('data-entry="%d"', id), fixed = TRUE)
    for (name in people$name) expect_false(grepl(name, html, fixed = TRUE))
    weigh(session, t1[1], 60)
    weigh(session, t1[2], 70)
    close_plenum(path)
    session$elapse(100)
    expect_match(output$part_view$html, "Gewichtung ist abgeschlossen")
  })
  votes <- read_table(path, "votes")
  expect_equal(votes$points[match(t1[1:2], votes$entry_id)], c(60, 40))
  echo <- sent(session, "bw_weight")
  expect_length(echo, 1L)
  expect_equal(echo[[1]]$message$points, 40)
})

test_that("people without a session identity cannot weigh", {
  path <- finished_fixture(withr::local_tempfile())
  open_plenum(path)
  e <- read_table(path, "entries")
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$elapse(100)
    expect_match(output$part_view$html, "Session beendet")
    weigh(session, e$id[1], 50)
  })
  expect_equal(nrow(read_table(path, "votes")), 0)
})

test_that("the moderator runs the plenum and gets a collected overview", {
  path <- finished_fixture(withr::local_tempfile())
  people <- read_table(path, "participants")$pid
  e <- read_table(path, "entries")
  e <- e[nzchar(e$text), ]
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(plenum_open = 1)
    expect_identical(read_table(path, "session")$plenum, "none")
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_match(output$mod_view$html, "Analyse &amp; Plenum", fixed = TRUE)
    expect_match(output$plenum_view$html, "Gewichtung starten", fixed = TRUE)
    session$setInputs(plenum_open = 2)
    session$elapse(100)
    expect_identical(read_table(path, "session")$plenum, "open")
    set_weight(path, people[1], e$id[e$topic_id == 1][1], 100)
    session$elapse(100)
    html <- output$plenum_view$html
    expect_match(html, "1 von 3 hat 100 % vergeben", fixed = TRUE)
    expect_match(html, "Ergebnisse erscheinen nach dem Beenden", fixed = TRUE)
    expect_false(grepl(e$text[e$topic_id == 1][1], html, fixed = TRUE))
    session$setInputs(plenum_close = 1)
    session$elapse(100)
    expect_identical(read_table(path, "session")$plenum, "closed")
    html <- output$plenum_view$html
    expect_match(html, e$text[e$topic_id == 1][1], fixed = TRUE)
    expect_match(html, "100 %", fixed = TRUE)
    expect_true(nzchar(output$fig_weights$src))
    # One weighted bar in one of three topics: the chart stays short.
    expect_equal(output$fig_weights$height, 140)
    csv <- utils::read.csv(output$dl_weights, check.names = FALSE)
    expect_identical(names(csv)[1:3], c("Thema", "Rang", "Beitrag"))
    session$setInputs(plenum_reopen = 1)
    session$elapse(100)
    expect_identical(read_table(path, "session")$plenum, "open")
  })
})

test_that("on a shared device the weighting passes from person to person", {
  path <- withr::local_tempfile()
  init_db(path)
  topics <- rep(list(list(t = "Thema", a = "Frage 1", b = "Frage 2")), 2)
  settings <- list(format = "brainwriting635-settings/1", mode = "hot_seat", rounds = 1,
                   round_secs = 300, turn_secs = 60,
                   topics = lapply(topics, function(x) list(title = x$t, q1 = x$a, q2 = x$b)),
                   participants = c("Ada", "Bo", "Cy"))
  configure_session(path, 2, 1, 300, topics, settings = settings)
  save_entry(path, current_author(path)$pid, 1, 1, 1, 1, "Gemeinsame Idee", 1)
  end_pass(path)
  open_plenum(path)
  entry <- read_table(path, "entries")$id
  people <- read_table(path, "participants")
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Ada")
    weigh(session, entry, 50)
    expect_equal(nrow(read_table(path, "votes")), 0)
    session$setInputs(plenum_start = 1)
    session$elapse(100)
    weigh(session, entry, 50)
    session$setInputs(plenum_done = 1)
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Bo")
    session$setInputs(plenum_skip = 1)
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Cy")
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_match(output$plenum_view$html, "Aktuell: Cy", fixed = TRUE)
    session$setInputs(plenum_skip_mod = 1)
    session$elapse(100)
    expect_match(output$part_view$html, "Alle haben gewichtet")
  })
  votes <- read_table(path, "votes")
  expect_identical(votes$pid, people$pid[people$name == "Ada"])
})

test_that("every weighting slider is named by its own contribution and budget", {
  path <- plenum_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  items <- plenum_items(path, pid)
  html <- as.character(weights_ui(items, read_table(path, "topics")))
  labelled <- regmatches(html, gregexpr('aria-labelledby="[^"]+"', html))[[1]]
  expect_length(labelled, nrow(items) + 3L)
  expect_false(grepl('aria-label="Gewichtung"', html, fixed = TRUE))
  for (id in items$id) {
    expect_match(html, sprintf('aria-labelledby="bw-q-%d bw-t-%d"', id, id), fixed = TRUE)
    expect_match(html, sprintf('id="bw-t-%d"', id), fixed = TRUE)
  }
  expect_match(html, 'aria-describedby="bw-remaining-1"', fixed = TRUE)
  expect_match(html, 'role="group" aria-labelledby="bw-topic-1"', fixed = TRUE)
  expect_match(html, 'aria-valuetext="0 %"', fixed = TRUE)
})

test_that("weights stored elsewhere reach every open page of the same voter", {
  path <- plenum_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  items <- plenum_items(path, pid)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  session <- capturing_session()
  shiny::testServer(app_server(cfg), session = session, {
    session$setInputs(stored_pid = pid)
    session$elapse(100)
    set_weight(path, pid, items$id[1], 75)  # e.g. from a second tab
    session$elapse(100)
  })
  pushed <- sent(session, "bw_weights")
  expect_gt(length(pushed), 0L)
  last <- pushed[[length(pushed)]]$message
  expect_equal(unlist(last$points)[match(items$id[1], unlist(last$entries))], 75)
})

test_that("a started hot-seat turn is dropped when the stored turn moves on", {
  path <- hot_plenum_fixture(withr::local_tempfile())
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$elapse(100)
    session$setInputs(plenum_start = 1)
    session$elapse(100)
    advance_plenum(path, 1L)  # the moderator skips Ada while her turn is started
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Bo")
    session$setInputs(plenum_skip = 1)
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Cy")
    close_plenum(path)
    session$elapse(100)
    open_plenum(path)  # starts again with the first person, who must be handed the device
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Ada")
    session$setInputs(plenum_start = 2)
    session$elapse(100)
    close_plenum(path)
    session$elapse(100)
    open_plenum(path)
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Ada")
  })
})

test_that("starting a hot-seat turn only arms the person shown on screen", {
  path <- hot_plenum_fixture(withr::local_tempfile())
  entry <- read_table(path, "entries")$id
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 2500)
  shiny::testServer(app_server(cfg), {
    session$elapse(100)
    expect_match(output$part_view$html, "Weitergeben an: .*Ada")
    advance_plenum(path, 1L)  # the moderator skips Ada before the device polls
    session$setInputs(plenum_start = 1)
    expect_null(plenum_armed())
    session$setInputs(weight = list(entry = entry, points = 50, nonce = 1))
  })
  expect_equal(nrow(read_table(path, "votes")), 0)
})

test_that("the weights CSV honours the spreadsheet-safe option", {
  path <- finished_fixture(withr::local_tempfile())
  pid <- read_table(path, "participants")$pid[1]
  exec_sql(path, "UPDATE entries SET text = '=1+1' WHERE id = (SELECT MIN(id) FROM entries)")
  open_plenum(path)
  set_weight(path, pid, min(read_table(path, "entries")$id), 100)
  close_plenum(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    raw <- utils::read.csv(output$dl_weights, check.names = FALSE)
    expect_true("=1+1" %in% raw$Beitrag)
    session$setInputs(csv_safe = TRUE)
    safe <- utils::read.csv(output$dl_weights, check.names = FALSE)
    expect_true("'=1+1" %in% safe$Beitrag)
    expect_false("=1+1" %in% safe$Beitrag)
  })
})

test_that("a closed weighting without any weights says so instead of promising exports", {
  path <- finished_fixture(withr::local_tempfile())
  open_plenum(path)
  close_plenum(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    html <- output$plenum_view$html
    expect_match(html, "Es wurden keine Gewichte vergeben.", fixed = TRUE)
    expect_false(grepl("fig_weights", html, fixed = TRUE))
    expect_false(grepl("auch in XLSX", html, fixed = TRUE))
  })
})
