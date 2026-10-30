test_that("the demonstration PIN is limited to local startup before creating storage", {
  path <- file.path(withr::local_tempdir(), "uncreated", "session.sqlite")
  cfg <- app_config(path, "635", "http://localhost:3838")
  expect_identical(cfg$host, "127.0.0.1")
  expect_identical(app_config(path, "635", "http://[::1]:3838", host = "::1")$host, "::1")
  for (host in c("0.0.0.0", "::", "192.168.1.2")) {
    expect_error(run_app(path, "635", "http://localhost:3838", host = host), "private mod_pin")
  }
  expect_error(run_app(path, "635", "https://class.example.org"), "private mod_pin")
  expect_false(dir.exists(dirname(path)))
  cfg <- app_config(path, "classroom-private-pin", "https://class.example.org", host = "0.0.0.0")
  expect_identical(cfg$host, "0.0.0.0")
})

test_that("invitation URLs cannot change the intended session route", {
  path <- file.path(withr::local_tempdir(), "uncreated", "session.sqlite")
  invalid <- c(
    "http:///missing-host", "https://example.org/#section", "https://user:password@example.org",
    "https://example.org/?s=abc234", "https://example.org/?mod=1",
    "https://example.org/?view=sessions",
    "https://example.org/?%73=abc234", "https://example.org/?lang=de&%6dod=1",
    "https://example.org:99999", "https://example.org:0", "https://example.org:abc",
    "https://example.org/invalid%escape", "https://example.org\\other", "https://example..org"
  )
  for (url in invalid) expect_error(run_app(path, "private-pin", url), "base_url")
  expect_false(dir.exists(dirname(path)))
  for (url in c("https://class.example.org/path/?lang=de", "http://[::1]:3838/",
                "https://localhost")) {
    cfg <- app_config(path, "private-pin", url)
    invitation <- session_url(cfg$base_url, "abc234")
    query <- shiny::parseQueryString(sub("^[^?]*\\?", "", invitation))
    expect_identical(query$s, "abc234")
    expect_null(query$mod)
    expect_null(query$view)
  }
})

test_that("archive failures close every connection and preserve the source session", {
  for (stage in c("vacuum", "copy")) local({
    main <- catalog_fixture()
    finish_with_entry(main, "Preserved after failure")
    before <- collect_data(main)
    code <- "abc234"
    if (stage == "vacuum") {
      dir.create(sessions_dir(main), showWarnings = FALSE)
      writeLines("occupied", session_path(main, code))
    }
    handles <- list()
    real_db <- db
    real_execute <- dbExecute
    # Retain the handles so garbage collection cannot hide a leaked connection.
    withr::defer({
      for (con in handles) if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })
    local_mocked_bindings(
      unused_code = function(...) code,
      db = function(...) {
        con <- real_db(...)
        handles[[length(handles) + 1L]] <<- con
        con
      },
      dbExecute = function(conn, statement, ...) {
        if (stage == "copy" && grepl("DROP TABLE", statement, fixed = TRUE)) stop("copy failure")
        real_execute(conn, statement, ...)
      }
    )
    expect_error(archive_standard(main))
    expect_true(length(handles) > 0L)
    expect_false(any(vapply(handles, DBI::dbIsValid, logical(1))))
    after <- collect_data(main)
    expect_identical(after$entries, before$entries)
    expect_identical(after$participants, before$participants)
    expect_identical(after$session, before$session)
    expect_equal(nrow(catalog_rows(main)), 0)
  })
})
