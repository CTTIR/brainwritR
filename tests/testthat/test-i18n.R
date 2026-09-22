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

test_that("setup texts for group size, format and optional names are translated", {
  dict <- bw_language_dictionary()
  texts <- c(
    "Teilnehmende (geplant)", "Namen (optional, ein Name pro Zeile)",
    "Alex\nRobin\n\u2026",
    paste("Teilnehmende k\u00f6nnen ihren Namen auch beim Beitritt selbst eingeben",
          "\u2013 gerne ein Pseudonym."),
    "Die Namen legen die Reihenfolge fest \u2013 gerne Pseudonyme."
  )
  expect_identical(setdiff(texts, dict$de), character())
  expect_false(any(c("Im Einzelmodus optional") %in% dict$de))
  js <- language_js()
  for (pattern in c("^Format (\\d+)-2-", "^Je Gruppe (\\d+) Personen", "Teilnehmenden$",
                    "^Reihum: (\\d+) Personen", "danach wiederholen",
                    "muss zwischen")) {
    expect_match(js, pattern, fixed = TRUE)
  }
})

test_that("plenum weighting texts are translated into English and French", {
  dict <- bw_language_dictionary()
  texts <- c(
    "Gewichtung", "Gewichtung im Plenum", "Analyse & Plenum", "Danke!",
    paste("Verteile je Thema 100 % auf die Beitr\u00e4ge, die dir am wichtigsten sind.",
          "Alle Beitr\u00e4ge sind anonym; gespeichert wird automatisch."),
    "Die Gewichtung ist abgeschlossen. Die Ergebnisse besprechen wir jetzt im Plenum.",
    "Alle haben gewichtet.", "Die Moderation beendet die Gewichtung.",
    "Es gibt noch keine Beitr\u00e4ge zum Gewichten.", "Gewichtung starten",
    "Gewichtung l\u00e4uft.",
    "Gewichtung l\u00e4uft", "Gewichtung beenden", "Ergebnisse erscheinen nach dem Beenden.",
    "Gewichtung beendet.", "Wieder \u00f6ffnen",
    "Alle verteilen je Thema 100 % auf die anonym gezeigten Beitr\u00e4ge, auch auf eigene.",
    "Gewichtung (CSV)",
    "Die Gewichtung ist auch in XLSX, RDS, Markdown und im PDF-Bericht enthalten.",
    "Die Gewichtung ist erst nach der Schreibphase m\u00f6glich.",
    "Die Gewichtung l\u00e4uft nicht.",
    "Bitte zuerst die Gewichtung beenden."
  )
  expect_identical(setdiff(texts, dict$de), character())
  js <- language_js()
  for (pattern in c("^Noch (\\d+) % zu vergeben$", "haben 100 % vergeben",
                    "Personen haben gewichtet", "Unterst\\u00fctzende$")) {
    expect_match(js, pattern, fixed = TRUE)
  }
})

test_that("counts read correctly in the singular and English uses a decimal point", {
  expect_identical(format_summary("individual", 3, 3, 3, 300)[2], "Je Gruppe 1 Person.")
  expect_identical(plenum_count_text(1), "1 Person hat gewichtet.")
  expect_identical(plenum_count_text(4), "4 Personen haben gewichtet.")
  expect_identical(progress_text(1, 3), "1 von 3 hat 100 % vergeben")
  expect_identical(progress_text(2, 3), "2 von 3 haben 100 % vergeben")
  expect_identical(contributions_text(1), "1 Beitrag")
  expect_identical(contributions_text(2), "2 Beitr\u00e4ge")
  js <- language_js()
  for (pattern in c("^Je Gruppe 1 Person\\.$", "^1 Person hat gewichtet\\.$",
                    "hat 100 % vergeben$", "^(\\d+) Beitrag$", "\\u00b7 1 Unterst\\u00fctzende$")) {
    expect_match(js, pattern, fixed = TRUE)
  }
  # English minutes use a decimal point, French keeps the comma.
  expect_match(js, "minutes.replace(',', '.')", fixed = TRUE)
})

test_that("the roster placeholder shows one name per line", {
  path <- withr::local_tempfile()
  init_db(path)
  cfg <- app_config(path, "secret", "http://localhost:3838", poll_ms = 50)
  shiny::testServer(app_server(cfg), {
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_match(output$mod_view$html, 'placeholder="Alex&#10;Robin&#10;\u2026"', fixed = TRUE)
  })
  expect_true("Alex\nRobin\n\u2026" %in% bw_language_dictionary()$de)
})
