analytics_entries <- function(text, topic = rep(1L, length(text)),
                              round = rep(1L, length(text))) {
  data.frame(
    topic_id = topic, sheet = 1L, round = round, question = 1L,
    pid = paste0("p", seq_along(text)), text = text, submitted = 1L
  )
}

analytics_topics <- function() {
  data.frame(id = 1:2, title = c("Mobilität", "Gärten"),
             q1 = c("Wie helfen Busse?", "Was wächst?"),
             q2 = c("Welche Wege?", "Welche Bäume?"))
}

test_that("tokenization preserves German Unicode and removes only declared vocabulary", {
  result <- bw_tokenize(c("Die GRÜẞE und Bäume! Öl, Bus 123", NA, ""))
  expect_identical(result[[1]], c("grüße", "bäume", "bus"))
  expect_identical(result[[2]], character())
  expect_identical(result[[3]], character())
  expect_identical(bw_tokenize("Bus bus BUS", "BUS")[[1]], character())
  expect_identical(bw_tokenize(character()), list())
  e <- analytics_entries(c("Mobilität Busse Radwege", "Mobilität Bäume"), topic = 1:2)
  counts <- bw_term_counts(e, topics = analytics_topics())
  expect_setequal(counts$term, c("radwege", "mobilität"))
  expect_true("busse" %in% bw_term_counts(e, topics = analytics_topics(),
                                          exclude_prompt = FALSE)$term)
})

test_that("term counts preserve occurrences and grouping", {
  e <- analytics_entries(c("Rad Bus Rad", "Bus Zug", "Rad"), topic = c(1, 1, 2))
  e$question <- c(1L, 2L, 1L)
  counts <- bw_term_counts(e)
  expect_identical(counts$term, c("bus", "rad", "zug", "rad"))
  expect_equal(counts$n, c(2, 2, 1, 1))
  detailed <- bw_term_counts(e, by = "question")
  expect_named(detailed, c("topic_id", "question", "term", "n"))
  expect_equal(detailed$n[detailed$topic_id == 1 & detailed$question == 1 &
                            detailed$term == "rad"], 2)
  expect_equal(nrow(bw_term_counts(e[FALSE, ], "question")), 0)
  expect_equal(nrow(bw_term_counts(analytics_entries("und die"))), 0)
})

test_that("co-occurrence counts entries rather than repeated words", {
  e <- analytics_entries(c("Rad Bus Rad", "Rad Bus Zug", "Bus Zug", "Rad"))
  edges <- bw_cooccurrence(e, min_cooc = 1)
  expect_identical(edges$from, c("bus", "bus", "rad"))
  expect_identical(edges$to, c("rad", "zug", "zug"))
  expect_equal(edges$weight, c(2, 2, 1))
  expect_equal(bw_cooccurrence(e)$weight, c(2, 2))
  capped <- bw_cooccurrence(e, min_cooc = 1, max_nodes = 2)
  expect_identical(attr(capped, "nodes")$term, c("rad", "bus"))
  expect_equal(nrow(capped), 1)
  expect_equal(capped$weight, 2)
  expect_equal(nrow(bw_cooccurrence(e, max_nodes = 1)), 0)
  expect_equal(nrow(bw_cooccurrence(e[FALSE, ])), 0)
  expect_equal(nrow(bw_cooccurrence(e, min_cooc = 10)), 0)
  expect_error(bw_cooccurrence(e, min_cooc = 0), "min_cooc")
  expect_error(bw_cooccurrence(e, max_nodes = c(1, 2)), "scalar")
})

test_that("build-on pools authors and computes exact consecutive-round Jaccard", {
  e <- analytics_entries(c("Rad Bus", "Rad Zug", "Zug Garten"), round = 1:3)
  expect_equal(bw_buildon(e)$buildon, 1 / 3)
  e <- rbind(e, analytics_entries("Bus Garten", round = 2))
  # Round 2 now contains all four terms; both comparisons are 2/4.
  expect_equal(bw_buildon(e)$buildon, 0.5)
  gap <- analytics_entries(c("Rad Bus", "Rad Bus"), round = c(1, 3))
  expect_true(is.na(bw_buildon(gap)$buildon))
  blank <- analytics_entries(c("Rad Bus", "und die", "Rad Bus"), round = 1:3)
  expect_true(is.na(bw_buildon(blank)$buildon))
  expect_true(is.na(bw_buildon(analytics_entries("Rad Bus"))$buildon))
  expect_equal(nrow(bw_buildon(e[FALSE, ])), 0)
  empty_topics <- bw_buildon(e[FALSE, ], topics = analytics_topics())
  expect_equal(empty_topics$topic_id, 1:2)
  expect_true(all(is.na(empty_topics$buildon)))
  e$question[4] <- 2L
  expect_equal(bw_buildon(e)$buildon, 1 / 3)
})

test_that("KPIs explicitly distinguish raw words from filtered distinct terms", {
  e <- analytics_entries(c("Der Bus fährt", "Bus Bus", " ", NA))
  e$submitted <- c(1L, 0L, 1L, 0L)
  kpi <- bw_kpis(e)
  expect_equal(kpi$contributions, 2)
  expect_equal(kpi$mean_words, 2.5)
  expect_equal(kpi$submission_rate, 0.5)
  expect_equal(kpi$distinct_terms, 2)
  empty <- bw_kpis(e[FALSE, ])
  expect_equal(empty$contributions, 0)
  expect_equal(empty$distinct_terms, 0)
  expect_true(is.na(empty$mean_words))
  expect_true(is.na(empty$submission_rate))
  summary <- bw_topic_summary(e, topics = analytics_topics())
  expect_equal(summary$topic_id, 1:2)
  expect_equal(summary$contributions, c(2, 0))
  expect_equal(nrow(bw_topic_summary(e[FALSE, ])), 0)
  e$pid <- rep("canonical-group", nrow(e))
  expect_identical(bw_kpis(e), kpi)
})
