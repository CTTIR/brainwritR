# Run the classroom brainwriting application

Starts one multilingual Shiny application backed by SQLite. Open
`base_url` for participants and append `/?mod=1` for the moderator.
Configuration is resolved at invocation, never at package load time.

## Usage

``` r
run_app(
  db_path = Sys.getenv("DB_PATH", "data/brainwriting.sqlite"),
  mod_pin = Sys.getenv("MOD_PIN", "635"),
  base_url = Sys.getenv("BASE_URL", "http://localhost:3838"),
  port = Sys.getenv("PORT", "3838"),
  host = "0.0.0.0",
  poll_ms = 2500
)
```

## Arguments

- db_path:

  SQLite file path. Defaults to `DB_PATH`, then
  `"data/brainwriting.sqlite"`.

- mod_pin:

  Moderator PIN. Defaults to `MOD_PIN`, then `"635"`. Set a private PIN
  before deployment.

- base_url:

  Public participant URL used in the QR code. Defaults to `BASE_URL`,
  then `"http://localhost:3838"`.

- port:

  Listening port. Defaults to `PORT`, then `3838`.

- host:

  Listening address, default `"0.0.0.0"`.

- poll_ms:

  Database polling interval in milliseconds, default `2500`.

## Value

Invisibly, the value returned by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) when the
app stops.

## Details

Use exactly one R process per database. No replicas or horizontal
scaling are supported. A round advances on the next connected client's
poll; with no clients connected it advances when someone reconnects.

## Examples

``` r
if (interactive()) {
  run_app()
}
```
