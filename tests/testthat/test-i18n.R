test_that("the language control offers three accessible positions and German default", {
  html <- as.character(language_switch_ui())
  expect_match(html, 'id="bw-language-slider"', fixed = TRUE)
  expect_match(html, 'type="range"', fixed = TRUE)
  expect_match(html, 'min="1"', fixed = TRUE)
  expect_match(html, 'max="3"', fixed = TRUE)
  expect_match(html, 'step="1"', fixed = TRUE)
  expect_match(html, 'value="1"', fixed = TRUE)
  expect_match(html, 'aria-valuetext="Deutsch"', fixed = TRUE)
  expect_match(html, 'translate="no"', fixed = TRUE)
})

test_that("translations preserve unknown content and cover all three languages", {
  dict <- bw_language_dictionary()
  expect_named(dict, c("de", "en", "fr"))
  expect_false(anyDuplicated(dict$de) > 0)
  expect_true(all(nzchar(as.matrix(dict))))
  expect_identical(bw_translate(c("Abgeben", "private contribution"), "en"),
                   c("Submit", "private contribution"))
  expect_identical(bw_translate("Abgeben", "fr"), "Envoyer")
  expect_identical(bw_translate(dict$de, "de"), dict$de)
  expect_identical(bw_translate(dict$de, "en"), dict$en)
  expect_identical(bw_translate(dict$de, "fr"), dict$fr)
})

test_that("client language script updates nodes in place and keeps identity separate", {
  js <- language_js()
  expect_match(js, "new WeakMap()", fixed = TRUE)
  expect_match(js, "MutationObserver", fixed = TRUE)
  expect_match(js, "bw_language", fixed = TRUE)
  expect_match(js, "ui_language", fixed = TRUE)
  expect_match(js, "[data-bw-user]", fixed = TRUE)
  expect_match(js, "characterData:true", fixed = TRUE)
  expect_false(grepl("innerHTML|outerHTML|location.reload|bw_store_pid", js))
})

test_that("session administration texts are translated into English and French", {
  dict <- bw_language_dictionary()
  texts <- c(
    "Standard-Session", "Alle Sessions", "Abmelden", "Session archiviert",
    "Session nicht gefunden", "Bitte QR-Code oder Link pr\u00fcfen.", "Zur Startseite",
    "Diese Session ist abgeschlossen. Ein Beitritt ist nicht mehr m\u00f6glich.",
    "Archiviert \u2014 nur Ansicht und Export.", "Zur\u00fccksetzen \u2026",
    "Session zur\u00fccksetzen?", "Einrichtung", "L\u00e4uft", "Beendet", "Datei fehlt",
    "Archiviert", "Basisadresse", "\u00d6ffnen", "QR-Code", "Neu starten", "Wiederherstellen",
    "Archivieren", "L\u00f6schen", "Zur\u00fccksetzen", "Sessions", "Neue Session", "Aktiv",
    "Archiv", "Jede Session hat eine eigene Adresse und einen eigenen QR-Code.",
    "Keine Sessions.", "Bezeichnung (optional)", "Erstellen", "QR-Code herunterladen (PNG)",
    "Schlie\u00dfen", "Diese Session ist noch nicht eingerichtet.",
    "Standard-Session archivieren?", "Standard-Session zur\u00fccksetzen?", "Session l\u00f6schen?",
    paste("Die Ergebnisse werden als archivierte Session gespeichert.",
          "Danach ist die Standard-Session f\u00fcr die n\u00e4chste Aktivit\u00e4t leer."),
    paste("Die Session wird mit allen Teilnehmern und Beitr\u00e4gen",
          "endg\u00fcltig gel\u00f6scht. Vorher exportieren!"),
    "Endg\u00fcltig l\u00f6schen", "Session nicht gefunden.",
    "Nur beendete Sessions k\u00f6nnen archiviert werden.",
    "Die Session-Datei konnte nicht gel\u00f6scht werden."
  )
  expect_identical(setdiff(texts, dict$de), character())
  expect_identical(bw_translate("Neue Session", "en"), "New session")
  expect_identical(bw_translate("Archivieren", "fr"), "Archiver")
  js <- language_js()
  for (pattern in c("Beitr\\u00e4ge$", "^Erstellt: ", "^Zu viele Fehlversuche")) {
    expect_match(js, pattern, fixed = TRUE)
  }
})
