#' brainwritR: resilient classroom brainwriting
#'
#' A single-process, SQLite-backed multilingual Shiny application for
#' facilitated topic rotation. See [run_app()] and the classroom guide.
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbWithTransaction
#' @importFrom RSQLite SQLite
#' @importFrom graphics par plot
#' @importFrom utils write.csv
#' @importFrom stats setNames
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shinyjs useShinyjs
#' @importFrom qrcode qr_code
#' @importFrom shiny HTML actionButton debounce div downloadButton downloadHandler
#' @importFrom shiny h2 h3 h4 invalidateLater isolate modalButton modalDialog numericInput observe
#' @importFrom shiny observeEvent p parseQueryString passwordInput plotOutput reactive reactivePoll
#' @importFrom shiny reactiveVal removeModal renderPlot renderUI req showModal showNotification
#' @importFrom shiny strong tagList tags textAreaInput textInput uiOutput
#' @importFrom rlang .data
"_PACKAGE"
