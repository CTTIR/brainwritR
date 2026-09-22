#' Validate a portable session configuration
#'
#' @param x A named list of settings.
#' @return A `bw_settings` list, or all validation errors as a character vector.
#' @keywords internal
#' @noRd
validate_settings <- function(x) {
  if (!is.list(x) || is.data.frame(x) || is.null(names(x))) {
    return("Einstellungen: m\u00fcssen eine benannte Liste sein.")
  }
  errors <- character()
  allowed <- c("format", "mode", "rounds", "round_secs", "turn_secs", "topics",
               "groups", "participants", "expected_participants")
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) {
    warning(paste0("Unbekannte Einstellungen: ", paste(unknown, collapse = ", "), "."),
            call. = FALSE)
  }
  if (anyDuplicated(names(x))) {
    errors <- c(errors, "Einstellungen: Schl\u00fcsselnamen m\u00fcssen eindeutig sein.")
  }
  scalar_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(trimws(value))
  }
  if (!identical(x$format, "brainwriting635-settings/1")) {
    errors <- c(errors, "format: muss brainwriting635-settings/1 sein.")
  }
  if (!scalar_text(x$mode) ||
        !x$mode %in% c("individual", "group_device", "hot_seat")) {
    errors <- c(errors, "mode: muss individual, group_device oder hot_seat sein.")
  }
  ranges <- list(rounds = c(1, 12), round_secs = c(30, 1800), turn_secs = c(20, 600),
                 expected_participants = c(1, 500))
  for (field in names(ranges)) {
    # The planned number of participants is optional.
    if (field == "expected_participants" && is.null(x[[field]])) next
    value <- x[[field]]
    bounds <- ranges[[field]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
          !is.finite(value) || value != floor(value) ||
          value < bounds[1] || value > bounds[2]) {
      errors <- c(errors, sprintf("%s: muss zwischen %s und %s liegen (ganze Zahl).",
                                  field, bounds[1], bounds[2]))
    }
  }
  topics <- x$topics
  valid_topics <- is.list(topics) && !is.data.frame(topics)
  if (!valid_topics || length(topics) < 2L || length(topics) > 6L) {
    errors <- c(errors, "topics: muss 2 bis 6 Themen enthalten.")
  }
  if (valid_topics) {
    for (i in seq_along(topics)) {
      topic <- topics[[i]]
      if (!is.list(topic) || !scalar_text(topic$title)) {
        errors <- c(errors, sprintf("topics: Thema %s hat keinen Titel.", i))
      }
      if (is.list(topic) && !is.null(topic$questions)) {
        questions <- topic$questions
        if (!(is.character(questions) || is.list(questions)) || !length(questions) ||
              !all(vapply(as.list(questions), scalar_text, logical(1)))) {
          errors <- c(errors, sprintf(
            "topics: Thema %s braucht mindestens eine nicht-leere Frage.", i
          ))
        }
      } else {
        for (q in 1:2) {
          if (!is.list(topic) || !scalar_text(topic[[paste0("q", q)]])) {
            errors <- c(errors, sprintf("topics: Thema %s hat keine Frage %s.", i, q))
          }
        }
      }
    }
  }

  name_vector <- function(value, field) {
    if (is.null(value)) return(character())
    # read_yaml returns YAML sequences as lists; accept R character vectors too.
    if (is.list(value) && !is.data.frame(value) &&
          all(vapply(value, scalar_text, logical(1)))) {
      value <- as.character(unlist(value, use.names = FALSE))
    }
    if (!is.character(value) || anyNA(value) || any(!nzchar(trimws(value)))) {
      return(paste0(field,
                    ": Namen m\u00fcssen nicht-leere Texte sein."))
    }
    value <- trimws(value)
    if (anyDuplicated(value)) {
      return(paste0(field,
                    ": Namen m\u00fcssen nach dem Trimmen eindeutig sein."))
    }
    structure(value, valid = TRUE)
  }
  groups <- name_vector(x$groups, "groups")
  participants <- name_vector(x$participants, "participants")
  if (!is.null(x$groups) && !isTRUE(attr(groups, "valid"))) {
    errors <- c(errors, groups)
  }
  if (!is.null(x$groups) && length(x$groups) != length(topics)) {
    errors <- c(errors, "groups: Anzahl muss der Anzahl der Themen entsprechen.")
  }
  if (!is.null(x$participants) && !isTRUE(attr(participants, "valid"))) {
    errors <- c(errors, participants)
  }
  if (length(errors)) return(errors)
  config <- structure(list(
    format = "brainwriting635-settings/1", mode = x$mode,
    rounds = as.integer(x$rounds), round_secs = as.integer(x$round_secs),
    turn_secs = as.integer(x$turn_secs),
    topics = lapply(topics, function(topic) {
      if (!is.null(topic$questions)) {
        list(title = trimws(topic$title), questions = trimws(topic_questions(topic)))
      } else {
        lapply(topic[c("title", "q1", "q2")], trimws)
      }
    }),
    groups = if (is.null(x$groups)) paste("Gruppe", seq_along(topics)) else as.vector(groups),
    participants = as.vector(participants)
  ), class = c("bw_settings", "list"))
  if (!is.null(x$expected_participants)) {
    config$expected_participants <- as.integer(x$expected_participants)
  }
  config
}

#' Read and validate a YAML settings file
#'
#' @param path Path to a YAML file, at most 100 KiB.
#' @return A `bw_settings` list, or German validation errors.
#' @keywords internal
#' @noRd
parse_settings <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    return("Datei: Einstellungsdatei wurde nicht gefunden.")
  }
  info <- file.info(path)
  if (isTRUE(info$isdir) || is.na(info$size) || info$size > 100 * 1024) {
    return("Datei: muss eine YAML-Datei mit h\u00f6chstens 100 KB sein.")
  }
  parsed <- tryCatch(yaml::read_yaml(path, eval.expr = FALSE), error = function(e) e)
  if (inherits(parsed, "error")) {
    return("Datei: YAML konnte nicht gelesen werden; bitte Syntax pr\u00fcfen.")
  }
  validate_settings(parsed)
}

#' Write a validated portable settings file
#'
#' @param x A settings list.
#' @param path Destination path.
#' @return The destination path invisibly. Invalid settings cause an error.
#' @keywords internal
#' @noRd
write_settings <- function(x, path) {
  config <- validate_settings(x)
  if (!inherits(config, "bw_settings")) stop(paste(config, collapse = "\n"), call. = FALSE)
  yaml::write_yaml(unclass(config), path)
  invisible(path)
}
