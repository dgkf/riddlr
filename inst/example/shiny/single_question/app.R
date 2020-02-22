library(shiny)
library(riddlr)
library(markdown)
library(shinyAce)

# ensure interactive console width won't affect output
options(width = 80)

r <- parse_riddlr_rmd(system.file(
  "example",
  "questions",
  "q3.riddlr.Rmd",
  package = "riddlr"))

ui <- fluidPage(
  theme = shinytheme(theme = 'cosmo'),
  riddle_ui("riddle",
    question_ui = r$prompt,
    metadata = r$metadata))

server <- function(input, output, session) {
  observe(callModule(riddle, "riddle",
    solution = r$grader$solution,
    quoted = TRUE,
    test_inputs = r$grader$test_inputs,
    test_timeouts = r$grader$test_timeouts))
}

shinyApp(ui, server)
