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
      tags$script(HTML(paste(app_js(), language_js()))),
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
      div(class = "bw-controls", language_switch_ui(), theme_toggle_ui()),
      uiOutput("page"),
      tags$footer(
        class = "bw-footer",
        tags$a(
          href = "https://github.com/CTTIR/brainwritR", target = "_blank", rel = "noopener",
          class = "bw-footer-link", translate = "no",
          # Inline SVG: no icon webfont and no external request.
          HTML(fontawesome::fa("github", fill = "currentColor", height = "1.1em", a11y = "deco")),
          "CTTIR \u00b7 brainwritR"
        )
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

#' Button switching between the light and the dark colour theme
#'
#' The choice is applied in the browser and remembered there; without a stored
#' choice the device preference decides. Figures and QR codes stay light.
#' @keywords internal
#' @noRd
theme_toggle_ui <- function() {
  icon <- function(name) {
    HTML(fontawesome::fa(name, fill = "currentColor", height = "1.1em", a11y = "deco"))
  }
  tags$button(
    type = "button", id = "bw-theme-toggle", class = "btn btn-outline-secondary bw-theme-toggle",
    `aria-label` = "Dunkles Design", title = "Dunkles Design", `aria-pressed` = "false",
    tags$span(class = "bw-icon-moon", icon("moon")),
    tags$span(class = "bw-icon-sun", icon("sun"))
  )
}
