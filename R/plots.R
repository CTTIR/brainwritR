#' Shared print and app colours
#' @keywords internal
#' @noRd
bw_palette <- function() {
  c(ink = "#22303c", muted = "#5b6b7a", petrol = "#0e6e78", panel = "#ffffff")
}

#' Shared figure theme
#' @keywords internal
#' @noRd
bw_theme <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = bw_palette()[["ink"]]),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "bottom", strip.text = ggplot2::element_text(face = "bold")
    )
}

#' A legible empty figure
#' @keywords internal
#' @noRd
bw_placeholder <- function(title) {
  ggplot2::ggplot() +
    ggplot2::annotate("text",
      x = 0, y = 0,
      label = "Zu wenig Text f\u00fcr diese Ansicht.",
      colour = bw_palette()[["muted"]], size = 3.5
    ) +
    ggplot2::labs(title = title) +
    bw_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Shorten text to a character budget and mark the cut with an ellipsis
#' @param text Character vector.
#' @param limit Maximum number of characters, including the ellipsis.
#' @return Character vector; texts within the budget are unchanged.
#' @keywords internal
#' @noRd
bw_shorten <- function(text, limit) {
  long <- !is.na(text) & nchar(text) > limit
  text[long] <- paste0(trimws(substr(text[long], 1, limit - 1L), "right"), "\u2026")
  text
}

#' Whole-number axis breaks for counts
#' @param limits Axis limits.
#' @return Numeric breaks without fractions.
#' @keywords internal
#' @noRd
bw_count_breaks <- function(limits) {
  breaks <- pretty(c(0, max(1, limits, na.rm = TRUE)))
  breaks[breaks == round(breaks)]
}

#' Attach display titles to a figure table
#'
#' The label is a factor in topic order, so axes and panels follow the topic
#' numbers. Titles beyond 60 characters end with an ellipsis.
#' @keywords internal
#' @noRd
bw_topic_labels <- function(x, topics) {
  x$topic <- paste("Thema", x$topic_id)
  if (!is.null(topics) && nrow(topics)) {
    labels <- topics$title[match(x$topic_id, topics$id)]
    keep <- !is.na(labels)
    labels <- stringi::stri_wrap(
      paste0(
        x$topic_id[keep], " \u00b7 ",
        bw_shorten(labels[keep], 60L)
      ),
      width = 22,
      simplify = FALSE
    )
    # A long first word would leave the topic number alone on the first line.
    labels <- lapply(labels, function(lines) {
      if (length(lines) > 1L && grepl("^[0-9]+ \u00b7$", lines[1])) {
        lines <- c(paste(lines[1], lines[2]), lines[-(1:2)])
      }
      lines
    })
    x$topic[keep] <- vapply(labels, paste, character(1), collapse = "\n")
  }
  x$topic <- factor(x$topic, levels = unique(x$topic[order(x$topic_id)]))
  x
}

#' Contributions by topic and round
#' @keywords internal
#' @noRd
bw_plot_contributions <- function(entries, topics = NULL) {
  title <- "Beitr\u00e4ge je Thema und Runde"
  entries <- entries[bw_has_text(entries$text), , drop = FALSE]
  if (!nrow(entries)) {
    return(bw_placeholder(title))
  }
  d <- stats::aggregate(
    rep(1L, nrow(entries)),
    entries[c("topic_id", "round")], sum
  )
  names(d)[3] <- "n"
  d <- bw_topic_labels(d, topics)
  d$round <- factor(d$round, levels = sort(unique(d$round)))
  tint <- grDevices::colorRampPalette(c("#bad9dc", bw_palette()[["petrol"]]))
  colours <- tint(nlevels(d$round))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$topic, fill = .data$round)) +
    # Read top-down like the legend: topic 1 and round 1 come first.
    ggplot2::geom_col(position = ggplot2::position_dodge2(reverse = TRUE, padding = 0),
                      width = 0.75) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_x_continuous(breaks = bw_count_breaks) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(title = title, x = "Beitr\u00e4ge", y = NULL, fill = "Runde") +
    bw_theme()
}

#' Most frequent terms per topic
#' @keywords internal
#' @noRd
bw_plot_terms <- function(entries, topics = NULL, top_n = 8L, exclude_prompt = TRUE,
                          ncol = 2L) {
  title <- "Top-Begriffe je Thema"
  d <- bw_term_counts(entries, by = "topic", topics = topics, exclude_prompt = exclude_prompt)
  if (!nrow(d)) {
    return(bw_placeholder(title))
  }
  d <- do.call(rbind, lapply(split(d, d$topic_id), function(x) {
    utils::head(x[order(-x$n, x$term), ], top_n)
  }))
  d <- bw_topic_labels(d, topics)
  d$label <- factor(paste(d$term, d$topic_id, sep = "___"),
    levels = unique(paste(d$term, d$topic_id, sep = "___"))[order(d$n)]
  )
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$label)) +
    ggplot2::geom_col(fill = bw_palette()[["petrol"]], width = 0.7) +
    ggplot2::facet_wrap(~topic, scales = "free_y", ncol = ncol) +
    ggplot2::scale_x_continuous(breaks = bw_count_breaks) +
    ggplot2::scale_y_discrete(labels = function(x) sub("___[0-9]+$", "", x)) +
    ggplot2::labs(title = title, x = "Nennungen", y = NULL) +
    bw_theme()
}

#' Pixel height of the term chart for its rows of topic panels
#' @param n_topics Number of topics.
#' @param compact Phone layout with one panel per row instead of two.
#' @keywords internal
#' @noRd
bw_terms_height <- function(n_topics, compact = FALSE) {
  n_topics <- max(1, n_topics)
  if (compact) 90 + 210 * n_topics else 120 + 170 * ceiling(n_topics / 2)
}

#' Whether a plot area is narrow enough for the compact phone layout
#' @param width Plot width in CSS pixels, or NULL before the first render.
#' @keywords internal
#' @noRd
bw_compact <- function(width) {
  is.numeric(width) && length(width) == 1L && !is.na(width) && width < 560
}

#' Axis labels of the weighting chart
#' @param d Rows of plenum_results() that are shown.
#' @param compact Shorter, narrower labels for phones.
#' @param numbered Prefix the rank, matching the ranking list in the app.
#' @param wrap Break labels into lines. Single-line labels suit a wide page
#'   with many short panels, where wrapped labels would run into one another.
#' @keywords internal
#' @noRd
bw_weight_labels <- function(d, compact = FALSE, numbered = FALSE, wrap = TRUE) {
  limit <- if (compact) 44L else 70L
  text <- bw_shorten(d$text, limit)
  if (numbered) text <- paste0(d$rank, ". ", text)
  if (!wrap) return(gsub("\\s+", " ", text))
  lines <- stringi::stri_wrap(text, if (compact) 24L else 36L, simplify = FALSE)
  vapply(lines, paste, "", collapse = "\n")
}

#' Pixel height of the weighting chart for the bars it shows
#' @keywords internal
#' @noRd
bw_weights_height <- function(results, top_n = 8L, compact = FALSE) {
  d <- results[!is.na(results$share) & results$points > 0, , drop = FALSE]
  if (!nrow(d)) return(200)
  rows <- vapply(split(d$entry_id, d$topic_id), function(x) min(length(x), top_n), 0)
  as.numeric(60 + sum(40 + rows * if (compact) 46 else 40))
}

#' Plenum weights: the highest-weighted contributions per topic
#' @param results Output of plenum_results().
#' @param topics Raw topics table.
#' @param top_n Contributions shown per topic.
#' @param compact Phone layout with short labels.
#' @param numbered Rank-numbered labels without a title, as in the app where the
#'   ranking list below carries the full text.
#' @param wrap Break axis labels into lines, see bw_weight_labels().
#' @keywords internal
#' @noRd
bw_plot_weights <- function(results, topics = NULL, top_n = 8L, compact = FALSE,
                            numbered = FALSE, wrap = TRUE) {
  title <- "Gewichtung im Plenum"
  d <- results[!is.na(results$share) & results$points > 0, , drop = FALSE]
  if (!nrow(d)) {
    return(bw_placeholder(title))
  }
  d <- do.call(rbind, lapply(split(d, d$topic_id), function(x) {
    utils::head(x[order(x$rank, x$entry_id), ], top_n)
  }))
  d <- bw_topic_labels(d, topics)
  keys <- paste(bw_weight_labels(d, compact, numbered, wrap), d$entry_id, sep = "___")
  d$label <- factor(keys, levels = rev(unique(keys)))
  d$value <- paste0(round(100 * d$share), " %")
  # Stretch the axis to the largest share (with room for its label), so small
  # shares in large classes stay comparable.
  upper <- min(1.2, max(0.1, 1.3 * max(d$share)))
  # facet_wrap gained free panel space in ggplot2 4.0. Older supported
  # installations use facet_grid, which has long supported this layout.
  facets <- if ("space" %in% names(formals(ggplot2::facet_wrap))) {
    ggplot2::facet_wrap(~topic, scales = "free_y", ncol = 1, space = "free_y")
  } else {
    ggplot2::facet_grid(rows = "topic", scales = "free_y", space = "free_y")
  }
  ggplot2::ggplot(d, ggplot2::aes(x = .data$share, y = .data$label)) +
    ggplot2::geom_col(fill = bw_palette()[["petrol"]], width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$value), hjust = -0.15, size = 3,
                       colour = bw_palette()[["ink"]]) +
    # Panels are as tall as their bars, so topics with many bars stay legible.
    facets +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), " %"),
                                limits = c(0, upper), expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = function(x) sub("___[0-9]+$", "", x)) +
    ggplot2::labs(title = if (!numbered) title,
                  x = if (compact) "Anteil" else "Anteil der vergebenen Punkte", y = NULL) +
    bw_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = if (compact) 9 else 10),
                   strip.text = ggplot2::element_text(face = "bold", size = 11),
                   plot.title.position = "plot",
                   plot.margin = ggplot2::margin(4, 16, 4, 4))
}

#' Positions of a term network, one tile per connected group of terms
#'
#' Each group is laid out on its own without edge weights, which would pull
#' the terms of one long sentence (they co-occur in every copy of it) into a
#' knot; edge widths still show the weights. Groups are scaled to their size
#' and placed on a grid with a gap, largest first, so that no group crowds
#' another.
#' @param graph An igraph graph.
#' @return A two-column matrix of node positions.
#' @keywords internal
#' @noRd
bw_network_layout <- function(graph) {
  parts <- igraph::components(graph)
  ids <- order(-parts$csize, seq_along(parts$csize))
  ncol <- ceiling(sqrt(length(ids) * 2))
  radii <- 0.3 * sqrt(parts$csize)
  step <- 2 * max(radii) + 0.5
  layout <- matrix(0, igraph::vcount(graph), 2)
  for (k in seq_along(ids)) {
    members <- which(parts$membership == ids[k])
    part <- igraph::induced_subgraph(graph, members)
    xy <- igraph::layout_with_fr(part, coords = igraph::layout_in_circle(part), niter = 500,
                                 grid = "nogrid", weights = NA)
    xy <- sweep(xy, 2, colMeans(xy))
    extent <- max(abs(xy))
    if (extent > 0) xy <- xy * radii[ids[k]] / extent
    cell <- c((k - 1L) %% ncol, -((k - 1L) %/% ncol))
    layout[members, ] <- sweep(xy, 2, cell * step, "+")
  }
  layout
}

#' Co-occurrence network with deterministic initial positions
#' @keywords internal
#' @noRd
bw_plot_network <- function(entries, topics = NULL, min_cooc = 2L,
                            max_nodes = 40L, exclude_prompt = TRUE, label_size = 3) {
  title <- "Begriffsnetz"
  edges <- bw_cooccurrence(entries, min_cooc, max_nodes, topics, exclude_prompt)
  if (!nrow(edges)) {
    return(bw_placeholder(title))
  }
  counts <- bw_term_counts(entries,
    by = "topic", topics = topics,
    exclude_prompt = exclude_prompt
  )
  nodes <- stats::aggregate(n ~ term, counts, sum)
  nodes <- nodes[nodes$term %in% c(edges$from, edges$to), ]
  # Point sizes as drawn, so that labels keep clear of them. The size scale
  # below maps counts by area: rescaled to 0-1 (0.5 without spread), square
  # root, then onto its range of 3 to 10.
  spread <- diff(range(nodes$n))
  share <- if (spread > 0) (nodes$n - min(nodes$n)) / spread else rep(0.5, nrow(nodes))
  nodes$drawn <- 3 + 7 * sqrt(share)
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  # Preserve the caller's RNG state while fixing the network layout.
  layout <- withr::with_seed(635, bw_network_layout(graph))
  ggraph::ggraph(graph, layout = "manual", x = layout[, 1], y = layout[, 2]) +
    ggraph::geom_edge_link(ggplot2::aes(width = .data$weight), colour = "#9dbbc0", alpha = 0.65) +
    ggraph::geom_node_point(ggplot2::aes(size = .data$n), colour = bw_palette()[["petrol"]]) +
    # Labels move apart instead of covering one another; the fixed seed keeps
    # the figure identical between renders.
    ggraph::geom_node_text(ggplot2::aes(label = .data$name, point.size = .data$drawn),
                           size = label_size, repel = TRUE, seed = 635, max.overlaps = Inf,
                           point.padding = 0.1, box.padding = 0.15, min.segment.length = 0.5,
                           force = 3, force_pull = 0.5, max.time = 1, max.iter = 1e5,
                           segment.colour = bw_palette()[["muted"]], segment.size = 0.25) +
    ggraph::scale_edge_width(range = c(0.3, 1.7), guide = "none") +
    ggplot2::scale_size_continuous(range = c(3, 10), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.18)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.25)) +
    ggplot2::labs(title = title) + bw_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()
    )
}

#' Wordcloud shared by the app and PDF
#' @keywords internal
#' @noRd
bw_plot_wordcloud <- function(entries, topics = NULL, max_terms = 60L,
                              exclude_prompt = TRUE, size_factor = 1) {
  title <- "Wordcloud"
  d <- bw_term_counts(entries, by = "topic", topics = topics, exclude_prompt = exclude_prompt)
  if (!nrow(d)) {
    return(bw_placeholder(title))
  }
  d <- stats::aggregate(n ~ term, d, sum)
  d <- utils::head(d[order(-d$n, d$term), ], max_terms)
  ggplot2::ggplot(d, ggplot2::aes(label = .data$term, size = .data$n)) +
    ggwordcloud::geom_text_wordcloud(seed = 635, colour = bw_palette()[["petrol"]]) +
    # Narrow panels scale the budget down so no word is placed beyond the edge.
    ggplot2::scale_size_area(max_size = size_factor * min(
      13, 30 / sqrt(sum(d$n / max(d$n))),
      90 / max(nchar(d$term))
    )) +
    ggplot2::labs(title = title) +
    bw_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()
    )
}

#' Lexical overlap between successive rounds
#' @keywords internal
#' @noRd
bw_plot_buildon <- function(entries, topics = NULL, exclude_prompt = TRUE) {
  title <- "Ankn\u00fcpfung \u00fcber Runden"
  d <- bw_buildon(entries, topics = topics, exclude_prompt = exclude_prompt)
  d <- d[is.finite(d$buildon), , drop = FALSE]
  if (!nrow(d)) {
    return(bw_placeholder(title))
  }
  d <- bw_topic_labels(d, topics)
  # Value labels tell a measured overlap of zero apart from a missing bar.
  d$value <- paste0(round(100 * d$buildon), " %")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$buildon, y = .data$topic)) +
    ggplot2::geom_col(fill = bw_palette()[["petrol"]], width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = .data$value), hjust = -0.15, size = 3,
                       colour = bw_palette()[["ink"]]) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
                                labels = function(x) paste0(round(100 * x), " %"),
                                expand = ggplot2::expansion(mult = c(0.02, 0.1))) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(title = title, x = "Mittlere Begriffsoverlappung", y = NULL) +
    bw_theme()
}

#' Translate figure labels without changing contribution text or categories
#' @param plot A shared figure.
#' @param language Interface language code.
#' @return A ggplot with localized interface labels.
#' @keywords internal
#' @noRd
bw_plot_language <- function(plot, language = "de") {
  if (!language %in% c("en", "fr")) {
    return(plot)
  }
  labels <- c(
    "Beitr\u00e4ge je Thema und Runde", "Top-Begriffe je Thema", "Begriffsnetz",
    "Wordcloud", "Ankn\u00fcpfung \u00fcber Runden", "Beitr\u00e4ge", "Runde",
    "Nennungen", "Mittlere Begriffsoverlappung", "Zu wenig Text f\u00fcr diese Ansicht.",
    "Gewichtung im Plenum", "Anteil der vergebenen Punkte", "Anteil"
  )
  values <- if (language == "en") {
    c(
      "Contributions by topic and round", "Top terms by topic", "Term network",
      "Wordcloud", "Continuity across rounds", "Contributions", "Round",
      "Occurrences", "Mean lexical overlap", "Not enough text for this view.",
      "Plenum weighting", "Share of points given", "Share"
    )
  } else {
    c(
      "Contributions par th\u00e8me et tour", "Termes principaux par th\u00e8me",
      "R\u00e9seau de termes", "Nuage de mots", "Continuit\u00e9 entre les tours",
      "Contributions", "Tour", "Occurrences", "Recouvrement lexical moyen",
      "Texte insuffisant pour cette vue.", "Pond\u00e9ration en pl\u00e9ni\u00e8re",
      "Part des points attribu\u00e9s", "Part"
    )
  }
  translate <- function(x) {
    if (!is.character(x)) {
      return(x)
    }
    i <- match(x, labels)
    x[!is.na(i)] <- values[i[!is.na(i)]]
    x
  }
  plot <- plot + do.call(ggplot2::labs, lapply(as.list(plot$labels), translate))
  for (i in seq_along(plot$layers)) {
    if (!is.null(plot$layers[[i]]$aes_params$label)) {
      plot$layers[[i]]$aes_params$label <- translate(plot$layers[[i]]$aes_params$label)
    }
    data <- plot$layers[[i]]$data
    if (is.data.frame(data) && "label" %in% names(data)) {
      plot$layers[[i]]$data$label <- translate(data$label)
    }
  }
  plot
}
