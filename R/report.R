#' A text panel for the fixed two-page report
#' @param lines Text lines.
#' @param title Optional panel title.
#' @param size Text size in millimetres.
#' @return A ggplot.
#' @keywords internal
#' @noRd
bw_report_text <- function(lines, title = NULL, size = 3.5) {
  n <- max(1L, length(lines))
  ggplot2::ggplot() +
    ggplot2::annotate("text",
      x = 0, y = rev(seq_len(n)),
      label = if (length(lines)) lines else "", hjust = 0,
      size = size, colour = bw_palette()[["ink"]]
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.5, n + 0.5), expand = c(0, 0)) +
    ggplot2::labs(title = title) +
    bw_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank()
    )
}

#' Format a report statistic without printing undefined values
#' @param value Numeric statistic.
#' @param percent Whether to format a fraction as a percentage.
#' @param digits Decimal places.
#' @return A character vector.
#' @keywords internal
#' @noRd
bw_report_number <- function(value, percent = FALSE, digits = 1L) {
  out <- rep("\u2014", length(value))
  keep <- is.finite(value)
  out[keep] <- formatC(value[keep] * if (percent) 100 else 1,
    format = "f", digits = digits, decimal.mark = ","
  )
  if (percent) out[keep] <- paste0(out[keep], " %")
  out
}

#' Compose the compact per-topic report table
#' @param entries Raw entries table.
#' @param topics Raw topics table.
#' @return A ggplot.
#' @keywords internal
#' @noRd
bw_report_table <- function(entries, topics) {
  summary <- bw_topic_summary(entries, topics)
  if (!nrow(summary)) {
    return(bw_placeholder("Kennzahlen je Thema"))
  }
  labels <- topics$title[match(summary$topic_id, topics$id)]
  labels[is.na(labels)] <- paste("Thema", summary$topic_id[is.na(labels)])
  labels <- paste0(summary$topic_id, " \u00b7 ", labels)
  labels <- ifelse(nchar(labels) > 30, paste0(substr(labels, 1, 27), "..."), labels)
  values <- list(
    labels, bw_report_number(summary$contributions, digits = 0),
    bw_report_number(summary$mean_words),
    bw_report_number(summary$submission_rate, percent = TRUE),
    bw_report_number(summary$buildon, percent = TRUE)
  )
  headers <- c(
    "Thema", "Beitr\u00e4ge", "\u00d8 W\u00f6rter", "Abgabequote",
    "Ankn\u00fcpfung"
  )
  x <- c(0, 0.48, 0.63, 0.80, 0.98)
  plot <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.5, nrow(summary) + 2), expand = c(0, 0)) +
    ggplot2::labs(title = "Kennzahlen je Thema") +
    bw_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()
    )
  for (i in seq_along(values)) {
    align <- if (i == 1L) 0 else 1
    plot <- plot + ggplot2::annotate("text",
      x = x[i], y = nrow(summary) + 1.3,
      label = headers[i], hjust = align, size = 2.8,
      fontface = "bold", colour = bw_palette()[["ink"]]
    ) +
      ggplot2::annotate("text",
        x = x[i], y = rev(seq_len(nrow(summary))),
        label = values[[i]], hjust = align, size = 2.7,
        colour = bw_palette()[["ink"]]
      )
  }
  plot
}

#' Build a two- or three-page A4 descriptive report
#'
#' Uses the same figure functions and text statistics as the application. No
#' Pandoc, LaTeX or browser is required. The submission rate refers to non-empty
#' saved entries; lexical overlap describes shared vocabulary, not idea quality.
#' A third page with the plenum weights is added when weights exist.
#' @param data Snapshot list containing session, topics, participants and entries
#'   data frames, optionally generated_at, package_version and settings.
#' @param file Destination PDF path.
#' @param anonymize Replace author identities using the shared export mapping.
#' @return The destination path invisibly.
#' @keywords internal
#' @noRd
build_report <- function(data, file, anonymize = FALSE) {
  if (!isTRUE(capabilities("cairo"))) {
    stop("PDF-Berichte ben\u00f6tigen Cairo-Unterst\u00fctzung.", call. = FALSE)
  }
  if (anonymize) data <- pseudonymize_data(data)
  entries <- data$entries
  topics <- data$topics
  s <- data$session
  generated <- data$generated_at %||% Sys.time()
  stamp <- if (inherits(generated, "POSIXt")) {
    format(generated, "%d.%m.%Y %H:%M %Z")
  } else {
    as.character(generated)
  }
  version <- data$package_version %||% as.character(utils::packageVersion("brainwritR"))
  mode <- if ("mode" %in% names(s)) as.character(s$mode[1]) else NULL
  mode_names <- c(
    individual = "Alle am eigenen Ger\u00e4t",
    group_device = "Ein Ger\u00e4t pro Gruppe",
    hot_seat = "Reihum an einem Ger\u00e4t"
  )
  header <- c(
    paste("Datum:", stamp),
    if (!is.null(mode)) paste("Modus:", mode_names[[mode]]),
    sprintf(
      "%s Themen \u00b7 %s %s \u00b7 %s %s", nrow(topics),
      nrow(data$participants),
      if (identical(mode, "group_device")) "Gruppen" else "Teilnehmer",
      s$n_rounds[1], if (identical(mode, "hot_seat")) "Durchg\u00e4nge" else "Runden"
    ),
    if (identical(mode, "hot_seat")) {
      paste("Zeit je Person:", data$settings$turn_secs %||% 90, "Sekunden")
    } else {
      paste("Rundenl\u00e4nge:", s$round_secs[1], "Sekunden")
    }
  )
  kpis <- bw_kpis(entries, topics)
  tiles <- c(
    paste("Beitr\u00e4ge\n", bw_report_number(kpis$contributions, digits = 0)),
    paste("\u00d8 W\u00f6rter je Beitrag\n", bw_report_number(kpis$mean_words)),
    paste("Abgabequote\n", bw_report_number(kpis$submission_rate, percent = TRUE)),
    paste("Verschiedene Begriffe\n", bw_report_number(kpis$distinct_terms, digits = 0))
  )
  page1 <- patchwork::wrap_plots(
    bw_report_text(header, "Brainwriting 6-3-5 \u2014 Ergebnisbericht", size = 3.5),
    patchwork::wrap_plots(lapply(tiles, bw_report_text, size = 3), nrow = 1),
    bw_plot_contributions(entries, topics), bw_plot_terms(entries, topics),
    ncol = 1, heights = c(1.5, 0.55, 2.7, 4)
  )
  networks <- if (nrow(topics) > 0L && nrow(topics) <= 3L) {
    patchwork::wrap_plots(lapply(seq_len(nrow(topics)), function(i) {
      bw_plot_network(entries[entries$topic_id == topics$id[i], , drop = FALSE], topics) +
        ggplot2::labs(title = paste("Begriffsnetz:", substr(topics$title[i], 1, 30)))
    }), ncol = 1)
  } else {
    bw_plot_network(entries, topics)
  }
  footer <- c(
    "Abgabequote: Anteil abgegebener, nicht-leerer gespeicherter Beitr\u00e4ge.",
    "Ankn\u00fcpfung: mittlere Jaccard-Begriffsoverlappung aufeinanderfolgender Runden.",
    "Beschreibende Kennzahlen; kein Ma\u00df f\u00fcr Qualit\u00e4t oder kausale Wirkung.",
    paste("Erstellt:", stamp, "\u00b7 brainwritR", version)
  )
  page2 <- patchwork::wrap_plots(networks, bw_plot_wordcloud(entries, topics),
    bw_report_table(entries, topics), bw_report_text(footer, size = 2.6),
    ncol = 1, heights = c(4.5, 2.5, 2, 0.9)
  )
  page2 <- page2 + patchwork::plot_annotation(
    title = "Inhalte & Vernetzung",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(
      family = "sans", face = "bold", colour = bw_palette()[["ink"]]
    ))
  )
  grDevices::cairo_pdf(
    filename = file, width = 8.27, height = 11.69, family = "sans",
    onefile = TRUE
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(page1)
  # ggwordcloud reseeds while drawing; keep participant and group draws random.
  withr::with_preserve_seed(print(page2))
  if (has_votes(data$votes)) print(bw_report_weights(data))
  invisible(file)
}

#' Third report page with the plenum weights
#' @param data Snapshot with a votes table.
#' @return A patchwork page.
#' @keywords internal
#' @noRd
bw_report_weights <- function(data) {
  votes <- data$votes[data$votes$points > 0, , drop = FALSE]
  results <- plenum_results(data$entries, data$topics, votes)
  # With more than three topics, fewer and shorter labels keep one page legible.
  many <- nrow(data$topics) > 3L
  patchwork::wrap_plots(
    bw_report_text(bw_report_weights_lines(data), "Gewichtung im Plenum", size = 3),
    bw_plot_weights(results, data$topics, top_n = if (many) 4L else 6L, compact = many) +
      ggplot2::labs(title = NULL),
    ncol = 1, heights = c(1, 8)
  )
}

#' Explanatory lines of the report's weighting page
#' @keywords internal
#' @noRd
bw_report_weights_lines <- function(data) {
  voters <- length(unique(data$votes$pid[data$votes$points > 0]))
  shown <- if (nrow(data$topics) > 3L) "vier" else "sechs"
  who <- if (voters == 1L) {
    "1 Person hat gewichtet;"
  } else {
    sprintf("%d Personen haben gewichtet;", voters)
  }
  c(
    paste(who, "je Thema standen jeder Person 100 % zur Verf\u00fcgung."),
    paste("Anteil: Teil aller in einem Thema vergebenen Punkte; gezeigt sind bis zu",
          shown, "Beitr\u00e4ge."),
    "Die Gewichtung ist ein Meinungsbild der Gruppe, keine Bewertung der Ideenqualit\u00e4t."
  )
}
