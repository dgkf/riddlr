#'
#' @importFrom shiny addResourcePath singleton tags
#' @export
#'
riddlr_css <- function() {
  shiny::addResourcePath("riddlr", system.file("www", package = "riddlr"))
  shiny::singleton(shiny::tags$head(shiny::tags$link(
    id = "riddlr-css",
    rel = "stylesheet",
    type = "text/css",
    href = "riddlr/riddlr.css"
  )))
}
