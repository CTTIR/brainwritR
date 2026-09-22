test_that("wrong PINs lock logins with growing waits and a correct PIN resets", {
  auth <- moderator_auth()
  for (i in 1:4) {
    attempt <- auth_try_pin(auth, "falsch", "secret", time = 100)
    expect_false(attempt$ok)
    expect_equal(attempt$wait, 0)
  }
  expect_equal(auth_try_pin(auth, "falsch", "secret", time = 100)$wait, 15)
  locked <- auth_try_pin(auth, "secret", "secret", time = 110)
  expect_false(locked$ok)
  expect_equal(locked$wait, 5)
  expect_equal(auth_try_pin(auth, "falsch", "secret", time = 116)$wait, 30)
  success <- auth_try_pin(auth, "secret", "secret", time = 147)
  expect_true(success$ok)
  expect_match(success$token, "^[A-Za-z0-9]{32}$")
  expect_equal(auth_try_pin(auth, "falsch", "secret", time = 148)$wait, 0)
  expect_false(auth_try_pin(auth, NULL, "secret", time = 148)$ok)
})

test_that("login waits are capped and old failures are forgotten", {
  auth <- moderator_auth()
  time <- 0
  for (i in 1:12) {
    attempt <- auth_try_pin(auth, "falsch", "secret", time = time)
    time <- time + attempt$wait + 1
  }
  expect_equal(attempt$wait, 300)
  expect_equal(auth_try_pin(auth, "falsch", "secret", time = time + 901)$wait, 0)
})

test_that("moderator tokens expire and can be revoked", {
  auth <- moderator_auth(ttl = 60)
  token <- auth_try_pin(auth, "secret", "secret", time = 0)$token
  expect_true(auth_check_token(auth, token, time = 59))
  expect_false(auth_check_token(auth, token, time = 61))
  token <- auth_try_pin(auth, "secret", "secret", time = 100)$token
  expect_false(auth_check_token(auth, "erfunden", time = 100))
  expect_false(auth_check_token(auth, NULL, time = 100))
  auth_revoke(auth, token)
  expect_false(auth_check_token(auth, token, time = 100))
})

test_that("a PIN login hands the tab a token that survives reloads until logout", {
  path <- withr::local_tempfile()
  init_db(path)
  server <- app_server(app_config(path, "secret", "http://localhost:3838", poll_ms = 50))
  first <- capturing_session()
  shiny::testServer(server, session = first, {
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_true(is_mod())
  })
  token <- sent(first, "bw_store_mod_token")[[1]]$message
  expect_match(token, "^[A-Za-z0-9]{32}$")
  reload <- capturing_session()
  shiny::testServer(server, session = reload, {
    expect_false(is_mod())
    session$setInputs(mod_token = "erfunden")
    expect_false(is_mod())
    session$setInputs(mod_token = token)
    expect_true(is_mod())
    session$setInputs(mod_logout = 1)
    expect_false(is_mod())
  })
  expect_length(sent(reload, "bw_clear_mod_token"), 1L)
  shiny::testServer(server, {
    session$setInputs(mod_token = token)
    expect_false(is_mod())
  })
})

test_that("the server refuses a correct PIN while the login is locked", {
  path <- withr::local_tempfile()
  init_db(path)
  server <- app_server(app_config(path, "secret", "http://localhost:3838", poll_ms = 50))
  shiny::testServer(server, {
    for (i in 1:5) session$setInputs(pin = "falsch", pin_btn = i)
    session$setInputs(pin = "secret", pin_btn = 6)
    expect_false(is_mod())
  })
})

test_that("an open moderator connection ends when its login expires or is revoked", {
  path <- withr::local_tempfile()
  init_db(path)
  server <- app_server(app_config(path, "secret", "http://localhost:3838", poll_ms = 50))
  session <- capturing_session()
  shiny::testServer(server, session = session, {
    session$setInputs(pin = "secret", pin_btn = 1)
    expect_true(is_mod())
    session$elapse(61000)
    expect_true(is_mod())
    # Thirteen hours later every 12-hour login is past its lifetime. The mock is
    # limited to this one step so later tests keep a real clock.
    later <- as.numeric(Sys.time()) + 13 * 3600
    testthat::with_mocked_bindings(session$elapse(61000), now = function() later)
    expect_false(is_mod())
    session$setInputs(start_btn = 1)
  })
  expect_length(sent(session, "bw_clear_mod_token"), 1L)
  expect_identical(read_table(path, "session")$status, "setup")
})

test_that("the clock is real again after the expiry test", {
  before <- now()
  Sys.sleep(0.05)
  expect_gt(now(), before)
  expect_lt(abs(now() - as.numeric(Sys.time())), 5)
})
