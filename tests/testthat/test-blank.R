blank_session <- function(path) {
  path <- catalog_fixture(parent.frame(2))
  new_db(path, n = 3)
  start_session(path)
  p <- read_table(path, "participants")
  blanks <- c("\t\r\n", "\u3000 \u00a0")
  for (i in 1:3) {
    topic <- topic_for(p$grp[i], 1, 3)
    text <- if (i == 1) "Eine echte Idee\nmit Zeilenumbruch" else blanks[i - 1]
    save_entry(path, p$pid[i], topic, 1, 1, 1, text, 1)
  }
  path
}

test_that("one predicate decides whether saved text is a contribution", {
  blanks <- c("", " ", "\t", "\r\n", " \n\t ", "\u00a0", "\u3000", "\u200b", NA)
  expect_false(any(bw_has_text(blanks)))
  expect_true(all(bw_has_text(c("x", " Idee\n", "\u00a0a", "\u200bb"))))
})

test_that("whitespace-only entries are ignored by analytics, weighting and protocols", {
  path <- blank_session()
  entries <- read_table(path, "entries")
  topics <- read_table(path, "topics")
  blank_ids <- entries$id[!grepl("echte", entries$text)]
  expect_equal(bw_kpis(entries)$contributions, 1)
  expect_equal(nrow(plenum_results(entries, topics, NULL)), 1)
  expect_equal(list_sessions(path, "http://x.org")$contributions[1], 1)
  md <- build_md(path)
  expect_equal(lengths(regmatches(md, gregexpr("- \\*\\*R1", md))), 1)
  expect_identical(build_snapshot_md(collect_data(path)), md)
  for (i in 1:3) maybe_advance(path, TRUE)
  expect_null(open_plenum(path))
  pid <- read_table(path, "participants")$pid[1]
  expect_identical(plenum_items(path, pid)$text, "Eine echte Idee\nmit Zeilenumbruch")
  expect_null(set_weight(path, pid, blank_ids[1], 10))
  exec_sql(path, "UPDATE entries SET text = '\t\n' WHERE text LIKE 'Eine echte%'")
  close_plenum(path)
  expect_match(open_plenum(path), "keine Beitr")
})

test_that("participants and moderators never see whitespace-only contributions", {
  path <- blank_session()
  p <- read_table(path, "participants")
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    for (i in 1:3) maybe_advance(path, TRUE)
    session$elapse(100)
    html <- output$mod_results$html
    expect_equal(lengths(regmatches(html, gregexpr('class="bw-prev"', html))), 1)
  })
  path <- blank_session()
  p <- read_table(path, "participants")
  maybe_advance(path, TRUE)
  e <- read_table(path, "entries")
  blank_author <- e$pid[!grepl("echte", e$text)][1]
  # The person who meets the blank entry's sheet in round 2 sees no prior text.
  sheet_topic <- e$topic_id[e$pid == blank_author]
  round2 <- vapply(seq_len(nrow(p)), function(i) topic_for(p$grp[i], 2, 3), 0)
  reader <- p[round2 == sheet_topic, ][1, ]
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(stored_pid = reader$pid)
    session$elapse(100)
    expect_match(output$prev1$html, "Noch keine Vorbeitraege")
  })
})
