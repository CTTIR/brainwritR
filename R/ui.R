#' Build the application interface
#' @keywords internal
#' @noRd
app_ui <- function(cfg) {
  page_fluid(
    lang = "de",
    theme = bs_theme(version = 5, bg = "#eceff2", fg = "#22303c", primary = "#0e6e78"),
    useShinyjs(),
    tags$head(
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$link(rel = "icon", type = "image/svg+xml", href = app_icon_uri()),
      tags$style(HTML(app_css())),
      tags$script(HTML(app_js())),
      tags$title("Brainwriting 6-3-5")
    ),
    tags$main(
      class = "bw-wrap", id = "main",
      tags$header(
        class = "bw-header",
        div(
          class = "bw-brand",
          tags$img(src = app_icon_uri(), alt = "", width = 36, height = 42),
          "brainwritR"
        ),
        div(class = "bw-status", "Gemeinsam Ideen weiterdenken")
      ),
      uiOutput("page"),
      tags$footer(
        class = "bw-footer",
        "6-3-5 \u00b7 Kursvariante 15-2-5 \u00b7 Pseudonyme willkommen"
      )
    )
  )
}

#' Inline the packaged icon without an external resource request
#' @keywords internal
#' @noRd
app_icon_uri <- function() {
  path <- system.file("app/www/icon.svg", package = "brainwritR")
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE))
}
