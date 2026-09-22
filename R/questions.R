#' Questions of one topic, including legacy two-question records
#' @keywords internal
#' @noRd
topic_questions <- function(topic) {
  if (!is.null(topic[["questions"]])) {
    return(unname(unlist(topic[["questions"]], use.names = FALSE)))
  }
  stored <- topic$questions_yaml
  if (length(stored) == 1L && !is.na(stored) && nzchar(stored)) {
    return(unname(unlist(yaml::yaml.load(stored, eval.expr = FALSE), use.names = FALSE)))
  }
  c(topic$q1, topic$q2)
}

#' Resolve the authored question for each contribution
#' @keywords internal
#' @noRd
question_text <- function(topics, topic_id, question) {
  vapply(seq_along(topic_id), function(i) {
    row <- match(topic_id[i], topics$id)
    if (is.na(row)) return("")
    questions <- topic_questions(topics[row, , drop = FALSE])
    q <- question[i]
    if (is.na(q) || q < 1L || q > length(questions)) return("")
    questions[q]
  }, character(1))
}

#' Authored question heading, protected from interface translation
#' @keywords internal
#' @noRd
question_heading <- function(topics, topic_id, question) {
  div(class = "bw-question-heading", tags$strong(sprintf("Frage %d", question)),
      " \u2014 ", tags$span(translate = "no", question_text(topics, topic_id, question)))
}

#' Validate an editable question count
#' @keywords internal
#' @noRd
valid_question_count <- function(n) {
  is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 1 &&
    n == floor(n) && n <= .Machine$integer.max
}
