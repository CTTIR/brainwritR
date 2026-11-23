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

test_that("topic labels follow topic order and mark shortened titles", {
  topics <- data.frame(id = 1:3, title = c(
    "Kurz", strrep("Sehr langer Titel ", 5), "Zuletzt"
  ))
  labelled <- bw_topic_labels(data.frame(topic_id = c(3L, 1L, 2L)), topics)
  expect_s3_class(labelled$topic, "factor")
  expect_identical(levels(labelled$topic)[c(1, 3)], c("1 \u00b7 Kurz", "3 \u00b7 Zuletzt"))
  expect_match(levels(labelled$topic)[2], "^2 \u00b7 Sehr")
  expect_match(levels(labelled$topic)[2], "\u2026$")
  expect_identical(bw_shorten(c("kurz", "abcdefghij", NA), 6L), c("kurz", "abcde\u2026", NA))
  expect_identical(nchar(bw_shorten(strrep("x", 80), 60L)), 60L)
  # Cuts fall between words unless that would drop more than half the budget.
  expect_identical(bw_shorten("Rollen klar verteilen und au\u00dferdem eine Idee", 32L),
                   "Rollen klar verteilen und\u2026")
  expect_identical(bw_shorten("Rollen verteilen", 12L), "Rollen\u2026")
  expect_identical(bw_shorten("Rollen klar verteilen", 12L), "Rollen klar\u2026")
  expect_identical(bw_shorten("Ab Zusammenarbeit", 12L), "Ab Zusammen\u2026")
  # The topic number stays on the line of a long first word.
  long_word <- bw_topic_labels(
    data.frame(topic_id = 5L),
    data.frame(id = 5L, title = "Pr\u00fcfungsvorbereitung in Lerngruppen")
  )
  expect_identical(as.character(long_word$topic),
                   "5 \u00b7 Pr\u00fcfungsvorbereitung\nin Lerngruppen")
  fallback <- bw_topic_labels(data.frame(topic_id = 2:1), NULL)
  expect_identical(levels(fallback$topic), c("Thema 1", "Thema 2"))
})

test_that("topic labels keep to three lines so six topics fit beside their bars", {
  topics <- data.frame(id = 1:3, title = c(
    "Wie können wir die Zusammenarbeit in Lerngruppen verbessern?",
    "Mehr Beteiligung in Gruppenarbeit", "Kommunikation zwischen Studierenden und Lehrenden"
  ))
  labelled <- bw_topic_labels(data.frame(topic_id = 1:3), topics)
  lines <- strsplit(levels(labelled$topic), "\n", fixed = TRUE)
  expect_true(all(lengths(lines) <= 3L))
  expect_identical(lines[[1]], c("1 · Wie können wir", "die Zusammenarbeit",
                                 "in Lerngruppen…"))
  # Titles that fit into three lines stay whole.
  expect_identical(lines[[2]], c("2 · Mehr Beteiligung", "in Gruppenarbeit"))
  expect_identical(lines[[3]], c("3 · Kommunikation", "zwischen Studierenden",
                                 "und Lehrenden"))
})

test_that("horizontal bars read top-down from topic 1 with whole-number counts", {
  topics <- data.frame(id = 1:3, title = c("Erstes", "Zweites", "Drittes"),
                       q1 = "Was?", q2 = "Wie?")
  e <- data.frame(
    topic_id = rep(1:3, each = 2), sheet = 1L, round = rep(1:2, 3), question = 1L,
    text = "Bus Zug Fahrrad Garten", submitted = 1L
  )
  for (plot in list(bw_plot_contributions(e, topics), bw_plot_buildon(e, topics))) {
    limits <- ggplot2::layer_scales(plot)$y$get_limits()
    expect_identical(limits, c("3 \u00b7 Drittes", "2 \u00b7 Zweites", "1 \u00b7 Erstes"))
  }
  bars <- ggplot2::layer_data(bw_plot_contributions(e, topics))
  first_topic <- bars[bars$y > 2.5, ]
  # Round 1 is the lightest fill and sits above round 2 within its topic.
  light <- first_topic$fill == "#BAD9DC"
  expect_gt(first_topic$y[light], first_topic$y[!light])
  expect_identical(bw_count_breaks(c(0, 1)), c(0, 1))
  expect_identical(bw_count_breaks(c(0, 0.4)), c(0, 1))
  expect_true(all(bw_count_breaks(c(0, 37)) %% 1 == 0))
  for (plot in list(bw_plot_contributions(e[1, ], topics), bw_plot_terms(e[1, ], topics))) {
    breaks <- ggplot2::layer_scales(plot)$x$get_breaks()
    expect_true(all(breaks[!is.na(breaks)] %% 1 == 0))
  }
})

test_that("the term chart grows with its rows of topic panels", {
  expect_identical(bw_terms_height(3), 460)
  expect_identical(bw_terms_height(4), 460)
  expect_gt(bw_terms_height(6), bw_terms_height(4))
  expect_lt(bw_terms_height(2), bw_terms_height(3))
  expect_identical(bw_terms_height(0), bw_terms_height(1))
  expect_identical(bw_terms_height(3, compact = TRUE), 720)
})

test_that("network labels repel one another reproducibly without touching the RNG", {
  e <- data.frame(
    topic_id = 1L, sheet = 1L, round = 1:4, question = 1L,
    text = "Bus Zug Fahrrad Garten Haus"
  )
  plot <- bw_plot_network(e, min_cooc = 2)
  geoms <- vapply(plot$layers, function(layer) class(layer$geom)[1], character(1))
  expect_true("GeomTextRepel" %in% geoms)
  file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(1)
  expected <- stats::runif(1)
  set.seed(1)
  expect_warning(print(plot), NA)
  expect_identical(stats::runif(1), expected)
})

test_that("dense network labels land on the same spots however long drawing takes", {
  words <- c("bus", "zug", "rad", "haus", "garten", "wald", "feld", "raum", "licht", "tafel",
             "pause", "kaffee", "ziele", "rollen", "fragen", "ideen", "lernen", "gruppe",
             "projekt", "methode", "material", "notizen", "handy", "tablet", "beamer",
             "plan", "termin", "ablauf", "regeln", "rueckmeldung", "austausch", "thema")
  text <- withr::with_seed(11, vapply(seq_len(72), function(i) {
    paste(sample(words, 6), collapse = " ")
  }, character(1)))
  e <- data.frame(topic_id = rep(1:3, 24), sheet = 1L, round = rep(1:3, each = 24),
                  question = 1L, text = text)
  label_positions <- function(plot) {
    grDevices::pdf(NULL, width = 8.3, height = 4)
    on.exit(grDevices::dev.off())
    print(plot)
    grid::grid.force()
    tree <- grid::grid.grab()
    texts <- list()
    collect <- function(grob) {
      if (inherits(grob, "text")) texts[[length(texts) + 1L]] <<- grob
      for (child in grob$children) collect(child)
    }
    collect(tree)
    vapply(texts, function(grob) {
      paste(grob$label, as.character(grob$x), as.character(grob$y))
    }, character(1))
  }
  drawn <- label_positions(bw_plot_network(e, max_nodes = 40L))
  expect_gt(length(drawn), 20L)
  # An iteration budget, not the wall clock, ends the label placement: with
  # unlimited time the labels end up exactly where they are drawn normally.
  # Layers are reference objects, so the unhurried figure is built afresh.
  unhurried <- bw_plot_network(e, max_nodes = 40L)
  repel <- which(vapply(unhurried$layers, function(layer) {
    inherits(layer$geom, "GeomTextRepel")
  }, logical(1)))
  unhurried$layers[[repel]]$geom_params$max.time <- 600
  expect_identical(label_positions(unhurried), drawn)
})

test_that("overlap bars carry their value, so zero overlap is not read as missing", {
  e <- data.frame(
    topic_id = rep(1:2, each = 2), sheet = 1L, round = rep(1:2, 2), question = 1L,
    text = c("Bus Zug", "Bus Rad", "Haus Garten", "Wald Feld"), submitted = 1L
  )
  plot <- bw_plot_buildon(e)
  expect_identical(plot$data$value, c("33 %", "0 %"))
  labels <- ggplot2::layer_data(plot, 2L)$label
  expect_identical(labels, c("33 %", "0 %"))
  expect_identical(ggplot2::layer_scales(plot)$x$get_labels()[1], "0 %")
})

test_that("a full overlap bar keeps its value inside the panel on phones", {
  e <- data.frame(
    topic_id = rep(1:2, each = 3), sheet = 1L, round = rep(1:3, 2), question = 1L,
    text = c(rep("Bus Zug Rad", 3), "Haus Garten", "Wald Feld", "Haus Feld"), submitted = 1L
  )
  plot <- bw_plot_buildon(e)
  text <- ggplot2::layer_data(plot, 2L)
  full <- text$label == "100 %"
  expect_true(any(full))
  # The label of a long bar ends inside the bar instead of beyond the axis.
  expect_true(all(text$hjust[full] > 1))
  expect_identical(unique(text$colour[full]), "white")
  expect_true(all(text$hjust[!full] < 0))
  expect_identical(unique(text$colour[!full]), bw_palette()[["ink"]])
  expect_true(all(text$x <= 1))
})

test_that("network groups get their own tile and a dense group is not drawn as a knot", {
  clique <- utils::combn(paste0("w", 1:8), 2)
  edges <- data.frame(from = c(clique[1, ], "a", "c"), to = c(clique[2, ], "b", "d"),
                      weight = c(rep(6, ncol(clique)), 1, 1))
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
  layout <- withr::with_seed(635, bw_network_layout(graph))
  expect_equal(dim(layout), c(igraph::vcount(graph), 2))
  group <- igraph::components(graph)$membership
  boxes <- lapply(split(seq_len(nrow(layout)), group), function(i) {
    apply(layout[i, , drop = FALSE], 2, range)
  })
  # No two groups share any part of their bounding boxes.
  for (pair in utils::combn(length(boxes), 2, simplify = FALSE)) {
    a <- boxes[[pair[1]]]
    b <- boxes[[pair[2]]]
    apart <- a[2, 1] < b[1, 1] || b[2, 1] < a[1, 1] || a[2, 2] < b[1, 2] || b[2, 2] < a[1, 2]
    expect_true(apart)
  }
  # Terms of the dense group keep a readable distance from one another.
  dense <- layout[group == group[1], ]
  expect_gt(min(stats::dist(dense)) / diff(range(dense[, 1])), 0.1)
  expect_identical(withr::with_seed(635, bw_network_layout(graph)), layout)
})

test_that("wide network panels place more groups side by side", {
  edges <- data.frame(from = c("a", "c", "e", "g"), to = c("b", "d", "f", "h"), weight = 2)
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
  centres <- function(aspect) {
    layout <- withr::with_seed(635, bw_network_layout(graph, aspect))
    length(unique(round(stats::ave(layout[, 2], igraph::components(graph)$membership), 6)))
  }
  expect_identical(centres(1), 2L)
  expect_identical(centres(5), 1L)
  plot <- bw_plot_network(data.frame(topic_id = 1L, sheet = 1L, round = 1:2, question = 1L,
                                     text = "Bus Zug"), aspect = 5)
  expect_s3_class(plot, "ggplot")
})

test_that("figure titles start at the plot edge so they fit narrow panels", {
  expect_identical(bw_theme()$plot.title.position, "plot")
  topics <- data.frame(id = 1:2, title = c("Mehr Beteiligung in Gruppenarbeit", "Kurz"))
  e <- data.frame(topic_id = 1:2, sheet = 1L, round = 1L, question = 1L, text = "Bus Zug")
  expect_identical(bw_plot_contributions(e, topics)$theme$plot.title.position, "plot")
})

test_that("legends and the overlap axis title stay inside narrow phone figures", {
  skip_if_not("legend.location" %in% names(ggplot2::get_element_tree()))
  topics <- data.frame(id = 1:3, title = c(
    "Wie können wir die Zusammenarbeit in Lerngruppen verbessern?",
    "Prüfungsvorbereitung in Lerngruppen und Selbststudium", "Kurz"
  ))
  e <- data.frame(topic_id = rep(1:3, each = 3), sheet = 1L, round = rep(1:3, 3),
                  question = 1L, text = "Bus Zug Rad", submitted = 1L)
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  layout <- ggplot2::ggplotGrob(bw_plot_contributions(e, topics))$layout
  legend <- layout[layout$name == "guide-box-bottom", c("l", "r")]
  title <- layout[layout$name == "title", c("l", "r")]
  # The round legend is centred on the whole figure, not on the panel that
  # long topic labels push to the right edge.
  expect_identical(legend, title, ignore_attr = TRUE)
  overlap <- bw_plot_buildon(e, topics)
  expect_identical(ggplot2::calc_element("axis.title.x", overlap$theme)$hjust, 1)
})

test_that("report text blocks keep their title on the edge of their text", {
  expect_identical(bw_report_text("Datum", "Bericht")$theme$plot.title.position, "panel")
})

test_that("the first report page aligns the header with the figure titles", {
  topics <- data.frame(id = 1:2, title = c("Mehr Beteiligung in Gruppenarbeit", "Kurz"))
  e <- data.frame(topic_id = 1:2, sheet = 1L, round = 1L, question = 1L, text = "Bus Zug")
  page <- bw_report_overview("Datum: heute", c("Beitr\u00e4ge\n 2", "Abgabequote\n 100 %"),
                             e, topics)
  expect_s3_class(page, "patchwork")
  # The page itself holds the last figure (terms); the patches hold the rest.
  plots <- c(page$patches$plots, page$patches$plots[[2]]$patches$plots, list(page))
  positions <- vapply(plots, function(p) p$theme$plot.title.position, character(1))
  expect_identical(positions, rep("panel", length(plots)))
})
