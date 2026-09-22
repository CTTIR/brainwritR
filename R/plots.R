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

#' Attach display titles to a figure table
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
        substr(labels[keep], 1, 42)
      ),
      width = 22,
      simplify = FALSE
    )
    x$topic[keep] <- vapply(labels, paste, character(1), collapse = "\n")
  }
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
    ggplot2::geom_col(position = "dodge", width = 0.75) +
    ggplot2::scale_fill_manual(values = colours) +
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
    ggplot2::scale_y_discrete(labels = function(x) sub("___[0-9]+$", "", x)) +
    ggplot2::labs(title = title, x = "Nennungen", y = NULL) +
    bw_theme()
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
#' @keywords internal
#' @noRd
bw_weight_labels <- function(d, compact = FALSE, numbered = FALSE) {
  limit <- if (compact) 44L else 70L
  text <- ifelse(nchar(d$text) > limit, paste0(substr(d$text, 1, limit - 3L), "..."), d$text)
  if (numbered) text <- paste0(d$rank, ". ", text)
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
#' @keywords internal
#' @noRd
bw_plot_weights <- function(results, topics = NULL, top_n = 8L, compact = FALSE,
                            numbered = FALSE) {
  title <- "Gewichtung im Plenum"
  d <- results[!is.na(results$share) & results$points > 0, , drop = FALSE]
  if (!nrow(d)) {
    return(bw_placeholder(title))
  }
  d <- do.call(rbind, lapply(split(d, d$topic_id), function(x) {
    utils::head(x[order(x$rank, x$entry_id), ], top_n)
  }))
  d <- bw_topic_labels(d, topics)
  keys <- paste(bw_weight_labels(d, compact, numbered), d$entry_id, sep = "___")
  d$label <- factor(keys, levels = rev(unique(keys)))
  d$value <- paste0(round(100 * d$share), " %")
  # Stretch the axis to the largest share (with room for its label), so small
  # shares in large classes stay comparable.
  upper <- min(1.2, max(0.1, 1.3 * max(d$share)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$share, y = .data$label)) +
    ggplot2::geom_col(fill = bw_palette()[["petrol"]], width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$value), hjust = -0.15, size = 3,
                       colour = bw_palette()[["ink"]]) +
    # Panels are as tall as their bars, so topics with many bars stay legible.
    ggplot2::facet_wrap(~topic, scales = "free_y", ncol = 1, space = "free_y") +
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
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  # Preserve the caller's RNG state while fixing the network layout.
  start <- igraph::layout_in_circle(graph)
  layout <- withr::with_seed(
    635,
    igraph::layout_with_fr(graph, coords = start, niter = 500, grid = "nogrid")
  )
  ggraph::ggraph(graph, layout = "manual", x = layout[, 1], y = layout[, 2]) +
    ggraph::geom_edge_link(ggplot2::aes(width = .data$weight), colour = "#9dbbc0", alpha = 0.65) +
    ggraph::geom_node_point(ggplot2::aes(size = .data$n), colour = bw_palette()[["petrol"]]) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name), size = label_size, vjust = -0.8) +
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
  ggplot2::ggplot(d, ggplot2::aes(x = .data$buildon, y = .data$topic)) +
    ggplot2::geom_col(fill = bw_palette()[["petrol"]], width = 0.65) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x * 100, "%")) +
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
