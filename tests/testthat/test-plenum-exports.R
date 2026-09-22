weighted_snapshot <- function(path) {
  plenum_fixture(path)
  p <- read_table(path, "participants")$pid
  e <- read_table(path, "entries")
  e <- e[nzchar(e$text), ]
  set_weight(path, p[1], e$id[e$topic_id == 1][1], 70)
  set_weight(path, p[1], e$id[e$topic_id == 1][2], 30)
  set_weight(path, p[2], e$id[e$topic_id == 1][1], 100)
  close_plenum(path)
  collect_data(path)
}

test_that("snapshots carry the weights and pseudonymize the voters consistently", {
  path <- withr::local_tempfile()
  data <- weighted_snapshot(path)
  expect_identical(data$votes, read_table(path, "votes"))
  anon <- pseudonymize_data(data)
  expect_true(all(anon$votes$pid %in% anon$participants$pid))
  expect_false(any(data$participants$pid %in% anon$votes$pid))
  expect_identical(anon$votes$points, data$votes$points)
})

test_that("the weights table ranks contributions with German column names", {
  data <- weighted_snapshot(withr::local_tempfile())
  w <- export_weights_df(data)
  expect_named(w, c("Thema", "Rang", "Beitrag", "Frage", "Bogen", "Runde", "Punkte",
                    "Anteil", "Durchschnitt je Person", "Unterst\u00fctzende"))
  top <- w[w$Rang == 1, ]
  expect_equal(top$Punkte[top$Thema == data$topics$title[1]], 170)
  expect_equal(top$Anteil[top$Thema == data$topics$title[1]], 0.85)
  expect_equal(nrow(w), sum(nzchar(trimws(data$entries$text))))
})

test_that("workbook, protocol, plot and report add the plenum only when weights exist", {
  data <- weighted_snapshot(withr::local_tempfile())
  file <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(data, file)
  expect_identical(openxlsx::getSheetNames(file)[5], "Gewichtung")
  md <- build_snapshot_md(data)
  expect_match(md, "## Gewichtung im Plenum", fixed = TRUE)
  expect_match(md, "1. **85 %**", fixed = TRUE)
  expect_s3_class(bw_plot_weights(plenum_results(data$entries, data$topics, data$votes),
                                  data$topics), "ggplot")
  none <- data
  none$votes <- none$votes[FALSE, ]
  expect_false(grepl("Gewichtung im Plenum", build_snapshot_md(none), fixed = TRUE))
  skip_if_not(capabilities("cairo"))
  skip_if_not_installed("pdftools")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  build_report(data, pdf)
  expect_equal(pdftools::pdf_info(pdf)$pages, 3L)
  expect_match(pdftools::pdf_text(pdf)[3], "Gewichtung im Plenum")
  build_report(none, pdf)
  expect_equal(pdftools::pdf_info(pdf)$pages, 2L)
})

test_that("the weighting chart numbers its bars and sizes itself to the shown rows", {
  results <- data.frame(
    topic_id = c(1, 1, 1, 2), title = c("A", "A", "A", "B"), entry_id = 1:4, sheet = 1,
    round = 1, question = 1,
    text = c(strrep("Eine sehr lange Idee ", 5), "Kurz", "Null", "Zweites Thema"),
    points = c(160, 40, 0, 50), share = c(0.8, 0.2, 0, 1), mean = c(80, 20, 0, 50),
    supporters = c(2, 1, 0, 1), rank = c(1, 2, 3, 1)
  )
  wide <- bw_weight_labels(results[results$points > 0, ], compact = FALSE, numbered = TRUE)
  compact <- bw_weight_labels(results[results$points > 0, ], compact = TRUE, numbered = TRUE)
  expect_match(wide, "^[0-9]+\\. ")
  expect_match(compact, "^[0-9]+\\. ")
  expect_true(all(nchar(unlist(strsplit(compact, "\n"))) <= 26))
  expect_false(grepl("^[0-9]+\\. ", bw_weight_labels(results[1, ], FALSE, FALSE)))
  one_topic <- bw_weights_height(results[results$topic_id == 1, ], compact = TRUE)
  both <- bw_weights_height(results, compact = TRUE)
  expect_gt(both, one_topic)
  expect_lt(one_topic, 300)
  expect_identical(bw_weights_height(results[FALSE, ], compact = TRUE), 200)
  p <- bw_plot_weights(results, NULL, compact = TRUE, numbered = TRUE)
  expect_null(p$labels$title)
})

test_that("continuity bars run horizontally so topic names stay readable", {
  path <- plenum_fixture(withr::local_tempfile())
  p <- bw_plot_buildon(read_table(path, "entries"), read_table(path, "topics"))
  expect_identical(rlang::as_label(p$mapping$y), "topic")
})

test_that("narrow screens get one facet column and a smaller network", {
  expect_true(bw_compact(390))
  expect_false(bw_compact(800))
  expect_false(bw_compact(NULL))
})

weight_rows <- function(bars, share) {
  data.frame(topic_id = rep(seq_along(bars), bars), title = "T", entry_id = seq_len(sum(bars)),
             sheet = 1, round = 1, question = 1, text = paste("Idee", seq_len(sum(bars))),
             points = 10, share = share, mean = 10, supporters = 1,
             rank = sequence(bars))
}

test_that("each topic panel of the weighting chart is as tall as its bars", {
  p <- bw_plot_weights(weight_rows(c(8, 1), 0.3), NULL, numbered = TRUE)
  withr::local_pdf(NULL)  # measuring the gtable needs a device; leave no Rplots.pdf
  g <- ggplot2::ggplotGrob(p)
  panels <- g$layout[grepl("^panel", g$layout$name), ]
  heights <- vapply(panels$t, function(i) as.numeric(g$heights[i]), 0)
  # Discrete panels span their bars plus a fixed 0.6 padding on each side.
  expect_equal(max(heights) / min(heights), (8 + 0.2) / (1 + 0.2))
})

test_that("the share axis stretches to the largest share instead of a fixed 120 %", {
  small <- ggplot2::layer_scales(bw_plot_weights(weight_rows(c(3, 3), 0.09), NULL))$x
  expect_lt(small$get_limits()[2], 0.2)
  large <- ggplot2::layer_scales(bw_plot_weights(weight_rows(c(1, 1), 1), NULL))$x
  expect_equal(large$get_limits()[2], 1.2)
})

test_that("the report page shortens labels and shows fewer bars for many topics", {
  skip_if_not(capabilities("cairo"))
  path <- withr::local_tempfile()
  new_db(path, k = 6, n = 6)
  start_session(path)
  p <- read_table(path, "participants")
  for (r in 1:6) {
    for (i in seq_len(nrow(p))) {
      save_entry(path, p$pid[i], topic_for(p$grp[i], r, 6), 1, r, 1,
                 paste("Eine ausfuehrliche Idee mit vielen Worten", i, r), 1)
    }
    maybe_advance(path, TRUE)
  }
  open_plenum(path)
  items <- plenum_items(path, p$pid[1])
  for (t in 1:6) set_weight(path, p$pid[1], items$id[items$topic_id == t][1], 100)
  close_plenum(path)
  page <- bw_report_weights(collect_data(path))
  expect_match(bw_report_weights_lines(collect_data(path))[1], "^1 Person hat gewichtet;")
  expect_s3_class(page, "patchwork")
})

test_that("topics whose bars are all zero do not add chart height", {
  rows <- weight_rows(c(2, 3), 0.2)
  rows$points[rows$topic_id == 2] <- 0
  expect_identical(bw_weights_height(rows), bw_weights_height(rows[rows$topic_id == 1, ]))
})
