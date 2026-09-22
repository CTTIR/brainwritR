#' Editable classroom demonstration topics
#'
#' The selected interface language is used when the preset is applied. Later
#' language changes do not translate or overwrite these editable topic values.
#' @param language Language code: de, en or fr. Other values use German.
#' @return Three topic lists, each containing title, q1 and q2.
#' @keywords internal
#' @noRd
example_topics <- function(language = "de") {
  if (length(language) != 1L || is.na(language) || !language %in% c("de", "en", "fr")) {
    language <- "de"
  }
  # 6-3-5 (Rohrbach): one open problem per sheet. Each round, write new ideas
  # (up to three), then take up an idea already on the sheet and develop it.
  build <- c(
    de = "Greife eine Idee von oben auf (oder eine eigene) und entwickle sie weiter.",
    en = "Pick up an idea from above (or one of your own) and develop it further.",
    fr = "Reprenez une id\u00e9e ci-dessus (ou l'une des v\u00f4tres) et d\u00e9veloppez-la."
  )
  sets <- list(
    de = list(
      c("Mehr Beteiligung in Gruppenarbeit", paste(
        "Wie k\u00f6nnten wir erreichen, dass sich alle aktiv an Gruppenarbeiten beteiligen?",
        "Notiere bis zu drei neue Ideen."
      )),
      c("Voneinander lernen", paste(
        "Wie k\u00f6nnten wir im Kurs mehr voneinander lernen?",
        "Notiere bis zu drei neue Ideen."
      )),
      c("Hilfreiches Feedback", paste(
        "Wie k\u00f6nnten wir einander Feedback geben, das wirklich weiterhilft?",
        "Notiere bis zu drei neue Ideen."
      ))
    ),
    en = list(
      c("More participation in group work", paste(
        "How might we get everyone to take an active part in group work?",
        "Note up to three new ideas."
      )),
      c("Learning from one another", paste(
        "How might we learn more from one another in class?",
        "Note up to three new ideas."
      )),
      c("Helpful feedback", paste(
        "How might we give one another feedback that really helps?",
        "Note up to three new ideas."
      ))
    ),
    fr = list(
      c("Plus de participation en groupe", paste(
        "Comment pourrions-nous amener chacun \u00e0 participer activement au travail de groupe ?",
        "Notez jusqu'\u00e0 trois nouvelles id\u00e9es."
      )),
      c("Apprendre les uns des autres", paste(
        "Comment pourrions-nous apprendre davantage les uns des autres en cours ?",
        "Notez jusqu'\u00e0 trois nouvelles id\u00e9es."
      )),
      c("Des retours utiles", paste(
        "Comment pourrions-nous nous faire des retours vraiment utiles ?",
        "Notez jusqu'\u00e0 trois nouvelles id\u00e9es."
      ))
    )
  )
  lapply(sets[[language]], function(topic) {
    stats::setNames(as.list(c(topic, build[[language]])), c("title", "q1", "q2"))
  })
}

#' Describe the configured format in plain sentences
#'
#' The classroom format is named after participants, questions per round and
#' minutes per round, like 6-3-5 and its course variant 15-2-5.
#' @param mode Device mode.
#' @param participants Planned participants, or NA when unknown.
#' @param k Number of topics and groups.
#' @param rounds Number of rounds or hot-seat passes.
#' @param round_secs Seconds per parallel round.
#' @param turn_secs Seconds per hot-seat turn.
#' @param questions Number of questions per topic.
#' @param roster Number of roster names in hot-seat mode.
#' @return German sentences, one per element.
#' @keywords internal
#' @noRd
format_summary <- function(mode, participants, k, rounds, round_secs, turn_secs = 90,
                           roster = 0, questions = 2L) {
  coverage <- if (rounds == k) {
    sprintf("Nach %d Runden hat jede Person jedes Thema 1\u00d7 bearbeitet.", k)
  } else if (rounds < k) {
    sprintf("Mit %d Runden bearbeitet jede Person %d von %d Themen.", rounds, rounds, k)
  } else {
    sprintf(paste("Nach %d Runden hat jede Person jedes Thema bearbeitet;",
                  "danach wiederholen sich die Themen."), k)
  }
  if (identical(mode, "hot_seat")) {
    return(c(sprintf("Reihum: %d Personen, %d Durchg\u00e4nge, %d s je Person.",
                     roster, rounds, turn_secs), coverage))
  }
  timing <- sprintf("%d Themen, %d Runden, %d s pro Runde.", k, rounds, round_secs)
  if (is.null(participants) || is.na(participants)) return(c(timing, coverage))
  minutes <- trimws(formatC(round_secs / 60, format = "fg", digits = 3, decimal.mark = ","))
  groups <- if (participants < k) {
    sprintf("F\u00fcr %d Themen sind mindestens %d Teilnehmende n\u00f6tig.", k, k)
  } else if (participants == k) {
    "Je Gruppe 1 Person."
  } else if (participants %% k == 0) {
    sprintf("Je Gruppe %d Personen.", participants %/% k)
  } else {
    sprintf("Je Gruppe %d\u2013%d Personen.", participants %/% k, participants %/% k + 1)
  }
  if (length(unique(questions)) == 1L) questions <- unique(questions)
  question_label <- if (length(questions) == 1L) {
    as.character(questions)
  } else {
    paste(range(questions), collapse = "\u2013")
  }
  c(paste0(sprintf("Format %d-%s-%s: ", participants,
                   question_label, minutes), timing), groups, coverage)
}

#' Count sentences with correct singular forms
#' @param n,done,total Counts.
#' @return German interface text, translated in the browser.
#' @keywords internal
#' @noRd
plenum_count_text <- function(n) {
  if (n == 1L) "1 Person hat gewichtet." else sprintf("%d Personen haben gewichtet.", n)
}

#' @rdname plenum_count_text
#' @keywords internal
#' @noRd
progress_text <- function(done, total) {
  sprintf(if (done == 1L) "%d von %d hat 100 %% vergeben" else "%d von %d haben 100 %% vergeben",
          done, total)
}

#' @rdname plenum_count_text
#' @keywords internal
#' @noRd
contributions_text <- function(n) {
  if (n == 1L) "1 Beitrag" else sprintf("%d Beitr\u00e4ge", n)
}
