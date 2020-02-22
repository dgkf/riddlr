#'
#' @importFrom shinyAce aceEditor
#' @export
#'
riddle_ui <- function(inputId, label = NULL, question_ui, metadata = list(),
    ace = metadata$ace) {

  title <- label %||% metadata$title %||% "Question"
  subtitle <- metadata$subtitle %||% sprintf("by %s", metadata$author) %||% NULL

  ns <- NS(inputId)

  aceEditor_hotkeys <- list(list(
    win = "Ctrl-R|Ctrl-Shift-Enter",
    mac = "CMD-ENTER|CMD-SHIFT-ENTER"))
  names(aceEditor_hotkeys) <- ns("ace_editor_run")

  aceEditor_defaults <- list(
    autoScrollEditorIntoView = TRUE,
    minLines = 15,
    maxLines = 25,
    autoComplete = "live",
    autoCompleters = "rlang")

  aceEditor_fixed <- list(
    outputId = ns("riddle_input"),
    mode = "r",
    hotkeys = aceEditor_hotkeys)

  aceEditor_args <- Reduce(modifyList, list(
    aceEditor_defaults,
    ace %||% list(),
    aceEditor_fixed))

  tagList(
    riddlr_css(),
    h2(title),
    if (!is.null(subtitle)) h4(subtitle) else list(),
    question_ui,
    h2("Solution"),
    do.call(shinyAce::aceEditor, aceEditor_args),
    div(
      class = "riddlr-run-btns",
      actionButton(ns("riddle_run"), "Run", icon = icon("angle-right")),
      modifyCssClasses(
        actionButton(ns("riddle_submit"), "Submit", icon = icon("check-square")),
        -btn-default, btn-primary)),
    uiOutput(ns("riddle_output")))
}



#'
#' @importFrom shinyAce aceAutocomplete aceAnnotate aceTooltip
#' @importFrom shiny is.reactive reactiveVal observeEvent renderUI tagList h2
#' @export
#'
riddle <- function(input, output, session, solution, test_inputs, test_details,
    test_timeouts, quoted = FALSE, clear = reactive(TRUE)) {

  ns <- session$ns

  ace_completer <- shinyAce::aceAutocomplete("riddle_input")
  ace_annotater <- shinyAce::aceAnnotate("riddle_input")
  ace_tooltip   <- shinyAce::aceTooltip("riddle_input")

  if (!shiny::is.reactive(clear)) clear <- reactive(clear)

  riddle_result <- shiny::reactiveVal()
  shiny::observeEvent(clear(), riddle_result(NULL))

  shiny::observeEvent(input$riddle_submit, {
    riddle_result(riddle_response_html(grade_riddle(
      input$riddle_input,
      test_timeouts = test_timeouts,
      test_inputs = test_inputs,
      solution = solution,
      quoted = quoted
    )))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observeEvent({ input$ace_editor_run; input$riddle_run }, {
    riddle_result(riddle_response_html(run_riddle(
      input$riddle_input,
      timeout = test_timeouts[[1]] * 10,
      envir = test_inputs[[1]])))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$riddle_output <- shiny::renderUI({
    if (is.null(riddle_result())) return()
    shiny::tagList(
      shiny::h2("Output"),
      riddle_result())
  })

  NULL
}
