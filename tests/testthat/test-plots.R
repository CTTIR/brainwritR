test_that("all shared figures draw without warnings for populated and small sessions", {
  topics <- data.frame(
    id = 1:2, title = c("Verkehr", "Campus"),
    q1 = c("Was hilft?", "Was fehlt?"), q2 = c("Wie?", "Warum?")
  )
  e <- data.frame(
    topic_id = rep(1:2, 3), sheet = 1L,
    round = rep(1:3, each = 2), question = 1L, pid = letters[1:6],
    text = c(
      "Bus Zug Fahrrad", "Bus Zug Garten", "Bus Zug Fahrrad",
      "Bus Zug Garten", "Bus Zug Fahrrad", "Bus Zug Garten"
    ),
    submitted = 1L
  )
  figures <- list(
    bw_plot_contributions, bw_plot_terms, bw_plot_network,
    bw_plot_wordcloud, bw_plot_buildon
  )
  file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (data in list(e, e[FALSE, ], transform(e[1, ], text = "und"))) {
    for (figure in figures) {
      expect_warning(plot <- figure(data, topics), NA)
      expect_s3_class(plot, "ggplot")
      expect_warning(print(plot), NA)
    }
  }
})

test_that("figure data retain the requested limits and grouping", {
  e <- data.frame(
    topic_id = 1L, sheet = 1L, round = 1:2, question = 1L,
    text = rep("Bus Zug Fahrrad Garten Haus", 2)
  )
  expect_equal(nrow(bw_plot_terms(e, top_n = 2)$data), 2)
  expect_equal(nrow(bw_plot_wordcloud(e, max_terms = 3)$data), 3)
  expect_equal(bw_plot_contributions(e)$data$n, c(1, 1))
  expect_equal(bw_plot_buildon(e)$data$buildon, 1)
})

test_that("figure localization changes labels while retaining topic content", {
  e <- data.frame(topic_id = 1L, sheet = 1L, round = 1L, question = 1L,
                  text = "Bus Zug")
  topics <- data.frame(id = 1L, title = "Beitr\u00e4ge", q1 = "", q2 = "")
  original <- bw_plot_contributions(e, topics)
  translated <- bw_plot_language(original, "en")
  expect_identical(translated$data, original$data)
  expect_identical(translated$labels$title, "Contributions by topic and round")
  expect_identical(bw_plot_language(original, "de"), original)
  empty <- bw_plot_language(bw_placeholder("Begriffsnetz"), "fr")
  file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_warning(print(translated))
  expect_no_warning(print(empty))
})
