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
