#' Weighting sliders for one voter
#'
#' One card per topic. The browser keeps each topic within 100 percent and
#' reports every released slider; the server stores and, if needed, corrects it.
#' @param items Output of plenum_items() for the voter.
#' @param topics Raw topics table.
#' @param handover Whether a "done, pass on" button ends the turn (hot seat).
#' @keywords internal
#' @noRd
weights_ui <- function(items, topics, handover = FALSE) {
  cards <- lapply(topics$id, function(topic) {
    x <- items[items$topic_id == topic, , drop = FALSE]
    if (!nrow(x)) return(NULL)
    # Each slider is named by its question label and contribution text, and
    # described by the topic's remaining budget, so it is identifiable when focused.
    div(
      class = "bw-card bw-weights", `data-topic` = topic,
      role = "group", `aria-labelledby` = paste0("bw-topic-", topic),
      h4(id = paste0("bw-topic-", topic), translate = "no", topics$title[topics$id == topic]),
      div(id = paste0("bw-remaining-", topic), class = "bw-status bw-remaining",
          sprintf("Noch %d %% zu vergeben", 100L - sum(x$points))),
      lapply(seq_len(nrow(x)), function(i) {
        id <- x$id[i]
        div(
          class = "bw-weight-item",
          div(id = paste0("bw-q-", id), class = "who",
              question_heading(topics, topic, x$question[i])),
          div(id = paste0("bw-t-", id), class = "bw-weight-text", translate = "no", x$text[i]),
          div(
            class = "d-flex align-items-center gap-2",
            tags$input(
              type = "range", class = "form-range bw-weight", min = 0, max = 100, step = 5,
              value = x$points[i], `data-entry` = id,
              `aria-labelledby` = sprintf("bw-q-%d bw-t-%d", id, id),
              `aria-describedby` = paste0("bw-remaining-", topic),
              `aria-valuetext` = paste0(x$points[i], " %")
            ),
            tags$span(class = "bw-weight-value", paste0(x$points[i], " %"))
          )
        )
      })
    )
  })
  tagList(
    div(
      class = "bw-card",
      h3("Gewichtung"),
      p(class = "bw-status", paste(
        "Verteile je Thema 100 % auf die Beitr\u00e4ge, die dir am wichtigsten sind.",
        "Alle Beitr\u00e4ge sind anonym; gespeichert wird automatisch."
      ))
    ),
    cards,
    if (handover) {
      actionButton("plenum_done", "Fertig \u2014 weitergeben", class = "btn-primary w-100 btn-lg")
    }
  )
}

#' Weighting progress for the moderator
#' @param progress Output of plenum_progress().
#' @keywords internal
#' @noRd
plenum_progress_ui <- function(progress) {
  lapply(seq_len(nrow(progress)), function(i) {
    div(
      class = "mb-1",
      strong(translate = "no", progress$title[i]), " ",
      tags$span(class = "bw-status", progress_text(progress$complete[i], progress$eligible[i]))
    )
  })
}

#' Ranked contributions per topic
#' @param results Output of plenum_results().
#' @param topics Raw topics table.
#' @keywords internal
#' @noRd
plenum_results_ui <- function(results, topics) {
  lapply(topics$id, function(topic) {
    x <- results[results$topic_id == topic, , drop = FALSE]
    if (!nrow(x)) return(NULL)
    div(
      class = "bw-card",
      h4(translate = "no", topics$title[topics$id == topic]),
      lapply(seq_len(nrow(x)), function(i) {
        share <- if (is.na(x$share[i])) "\u2014" else paste0(round(100 * x$share[i]), " %")
        div(
          class = "bw-rank-row",
          div(class = "bw-rank", strong(paste0(x$rank[i], ".")), div(share)),
          div(
            div(class = "who", sprintf("R%d \u00b7 F%d \u00b7 %d Unterst\u00fctzende",
                                       x$round[i], x$question[i], x$supporters[i])),
            question_heading(topics, topic, x$question[i]),
            div(class = "bw-rank-text", translate = "no", x$text[i])
          )
        )
      })
    )
  })
}
