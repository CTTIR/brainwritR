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
  sets <- list(
    de = list(
      c("Gemeinsam lernen", "Was hilft uns, voneinander zu lernen?",
        "Welche Lernidee k\u00f6nnen wir im n\u00e4chsten Kurs ausprobieren?"),
      c("Zusammenarbeit", "Wie k\u00f6nnen wir alle aktiv in die Gruppenarbeit einbeziehen?",
        "Welche konkrete Vereinbarung w\u00fcrde unsere Zusammenarbeit verbessern?"),
      c("Hilfreiches Feedback", "Was macht Feedback hilfreich und respektvoll?",
        "Wie k\u00f6nnen wir Feedback regelm\u00e4\u00dfig in unseren Alltag einbauen?")
    ),
    en = list(
      c("Learning together", "What helps us learn from one another?",
        "Which learning idea could we try in our next class?"),
      c("Collaboration", "How can we involve everyone actively in group work?",
        "Which concrete agreement would improve our collaboration?"),
      c("Helpful feedback", "What makes feedback helpful and respectful?",
        "How can we make feedback a regular part of our everyday work?")
    ),
    fr = list(
      c("Apprendre ensemble", "Qu'est-ce qui nous aide \u00e0 apprendre les uns des autres ?",
        "Quelle id\u00e9e pour apprendre pourrions-nous essayer au prochain cours ?"),
      c("Collaboration", "Comment faire participer activement chacun au travail de groupe ?",
        "Quel accord concret pourrait am\u00e9liorer notre collaboration ?"),
      c("Un retour constructif", "Qu'est-ce qui rend un retour utile et respectueux ?",
        "Comment int\u00e9grer des retours r\u00e9guliers dans notre travail quotidien ?")
    )
  )
  lapply(sets[[language]], function(topic) {
    stats::setNames(as.list(topic), c("title", "q1", "q2"))
  })
}
