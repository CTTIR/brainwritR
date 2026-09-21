#' Topic assigned to a group in a round
#'
#' Over `n_groups` consecutive rounds, each group visits every topic exactly
#' once. In each round the groups cover all topics. Arguments recycle using
#' ordinary R arithmetic; group and round indices are one-based.
#' @param grp Positive integer group index, at most `n_groups`.
#' @param round Positive integer round index.
#' @param n_groups Integer number of groups (2 to 6).
#' @return A numeric vector of one-based topic indices.
#' @export
#' @examples
#' topic_for(1:3, 2, 3)
#' topic_for(1, 1:3, 3)
topic_for <- function(grp, round, n_groups) {
  check_index(n_groups, "n_groups", 2, 6)
  check_index(grp, "grp", 1, n_groups)
  check_index(round, "round")
  ((grp - 1 + round - 1) %% n_groups) + 1
}

#' Sheet assigned to a participant within a group
#'
#' Sheet counts are frozen at session start. Modulo mapping permits several
#' participants to contribute to the same sheet in one round without replacing
#' one another's entries. A smaller arriving group can leave sheets untouched.
#' @param idx Positive integer within-group index.
#' @param n_sheets Positive integer number of sheets for the topic.
#' @return A numeric vector of one-based sheet indices.
#' @export
#' @examples
#' sheet_for(1:6, 5)
sheet_for <- function(idx, n_sheets) {
  check_index(idx, "idx")
  check_index(n_sheets, "n_sheets")
  ((idx - 1) %% n_sheets) + 1
}

#' Validate an integer-valued index
#' @keywords internal
#' @noRd
check_index <- function(x, name, min = 1, max = Inf) {
  if (!is.numeric(x) || !length(x) || anyNA(x) ||
        any(!is.finite(x) | x != floor(x) | x < min | x > max)) {
    stop(name, " must contain finite whole numbers in the documented range.", call. = FALSE)
  }
}
