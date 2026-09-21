figure_contract_fixture <- function() {
  topics <- data.frame(id = 1:6, title = c("Gleich", "Gleich", paste("Thema", 3:6)),
                       q1 = "Frage eins", q2 = "Frage zwei", n_sheets = 1L)
  entries <- expand.grid(topic_id = 1:6, sheet = 1L, round = 1:3, question = 1:2)
  entries$text <- "kreativ gemeinsam lernen arbeiten"
  entries$submitted <- 1L
  list(entries = entries, topics = topics)
}

test_that("duplicate topic titles remain separate categories and facets", {
  data <- figure_contract_fixture()
  for (fun in list(bw_plot_contributions, bw_plot_terms, bw_plot_buildon)) {
    plot <- fun(data$entries, data$topics)
    expect_length(unique(plot$data$topic), 6L)
  }
  plot <- bw_plot_terms(data$entries, data$topics)
  expect_equal(nrow(ggplot2::ggplot_build(plot)$layout$layout), 6L)
})

test_that("network layouts repeat exactly without altering caller RNG", {
  data <- figure_contract_fixture()
  withr::local_seed(42)
  before <- .Random.seed
  first <- bw_plot_network(data$entries, data$topics)
  expect_identical(.Random.seed, before)
  second <- bw_plot_network(data$entries, data$topics)
  expect_identical(first$data$x, second$data$x)
  expect_identical(first$data$y, second$data$y)
  expect_identical(.Random.seed, before)
})

test_that("all figures draw quietly for empty, tiny and six-topic data", {
  skip_if_not(capabilities("cairo"))
  data <- figure_contract_fixture()
  path <- withr::local_tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(path, width = 8, height = 5)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (rows in list(integer(), 1L, seq_len(nrow(data$entries)))) {
    for (fun in list(bw_plot_contributions, bw_plot_terms, bw_plot_network,
                     bw_plot_wordcloud, bw_plot_buildon)) {
      plot <- fun(data$entries[rows, ], data$topics)
      expect_s3_class(plot, "ggplot")
      expect_no_warning(print(plot))
    }
  }
})

test_that("a sixty-term wordcloud draws quietly at report panel dimensions", {
  skip_if_not(capabilities("cairo"))
  data <- figure_contract_fixture()
  terms <- paste0("begriff", rep(letters[1:3], each = 20), letters[1:20])
  data$entries$text <- paste(terms, collapse = " ")
  plot <- bw_plot_wordcloud(data$entries, data$topics)
  expect_equal(nrow(plot$data), 60L)
  path <- withr::local_tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(path, width = 8, height = 2.6)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_warning(print(plot))
})
