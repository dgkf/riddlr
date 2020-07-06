#' function for generating an exercise checker for a puzzlr challenge
#'
#' @param envs environments in which solution should be evaluated. these should
#'   contain the necessary input variables for the challenge.
#' @param solution solution code
#'
#' @return A shiny ui element displaying output from grading the question
#'   solution.
#'
#' @importFrom shiny withProgress setProgress
#' @export
#'
grade_riddle <- function(user_code, solution, test_inputs, test_timeouts = Inf,
    grace_timeouts = test_timeouts * 5,
    test_details = rep(list(NULL), length(test_inputs)),
    quoted = FALSE) {

  n <- length(test_inputs)

  # validate test inputs and ensure they have initialized value
  if (!length(test_timeouts)) test_timeouts <- 5
  if (!length(grace_timeouts)) grace_timeouts <- test_timeouts * 5
  if (!length(test_details)) test_details <- rep(list(NULL), length(test_inputs))

  shiny::withProgress(
    message = 'parsing input...',
    min = -1,
    max = n,
    value = -1, {

    .exit_response_type <- "riddlr_error"

    test_timeouts  <- rep_len(test_timeouts, n)
    grace_timeouts <- rep_len(grace_timeouts, n)
    test_details   <- rep_len(test_details,  n)
    if (!quoted) solution <- as.list(match.call())$solution

    .total_duration <- 0L

    # call submission callback whenever function completes
    on.exit({
      opts$get("onSubmit")(user_code, .total_duration, .exit_response_type)
    })

    # attempt to parse user code, display errors/warnings if necessary
    user_expr <- parse_safe(text = user_code)

    if (is_error(user_expr)) {
      .exit_response_type <- "syntax"
      return(parse_error_response(user_expr))
    }

    # test against test cases
    .total_duration <- 0
    for (i in seq_along(test_inputs)) {
      setProgress(i - 1,
        message = sprintf("running test case %d...", i),
        detail = test_details[[i]])

      envir <- as.environment(test_inputs[[i]])
      parent.env(envir) <- riddlr_base_env

      user_soln <- eval_with_timeout(user_expr,
        timeout = grace_timeouts[[i]],
        envir = new.env(parent = envir))

      if (is_timeout_error(user_soln)) {
        .exit_response_type <- "timeout"
        return(timeout_response(user_soln, NULL, test_inputs[[i]], i, n))
      }

      if (is_error(user_soln)) {
        .exit_response_type <- "error"
        return(error_response(user_soln, NULL, test_inputs[[i]], i, n))
      }

      soln <- eval_with_timeout(solution, envir = new.env(parent = envir))

      if (user_soln != soln) {
        .exit_response_type <- "incorrect"
        return(incorrect_solution_response(user_soln, soln, test_inputs[[i]], i, n))
      }

      if (attr(user_soln, "duration")["elapsed"] > test_timeouts[[i]]) {
        .exit_response_type <- "grace_timeouts"
        return(grace_timeouts_response(user_soln, soln, test_inputs[[i]], i, n,
            attr(user_soln, "duration")["elapsed"] / test_timeouts[[i]]))
      }

      .total_duration <- .total_duration + attr(user_soln, "duration")["elapsed"]
    }

    shiny::setProgress(n, message = "done running tests...")

    # success!
    .exit_response_type <- "success"
    success_response(hash = solution)
  })
}



#' Format test results as html for updating progress text
#'
#' @importFrom shiny tagList
#'
update_test_progress_html <- function(test_results, reactive_output) {
  if (is.null(reactive_output)) return(tagList())
  reactive_output(do.call(shiny::tagList, Map(
    test_results,
    seq_along(test_results),
    f = function(t, i) {
      if (isTRUE(t))
        tags$span(icon("check"), sprintf("Test %d", i))
      else if (is.na(t))
        tags$span(icon("hourglass-half"), sprintf("Test %d", i))
      else
        tags$span(icon("times"), sprintf("Test %d", i))
    })))
}



#' Parsing with error handling
#'
parse_safe <- function(...) {
  riddlr_result(tryCatch(parse(...), error = function(e) e))
}



#' Evaluate and memoise solution execution
#'
#' @importFrom memoise memoise
#'
eval_with_timeout <- memoise::memoise(
  function(x, timeout = Inf, envir = parent.frame()) {

  .duration <- NULL

  # break down code into top level expressions
  xs <- if (is.expression(x) && length(x)) as.list(x) else list(x)

  # itrate through expressions, collecting console output
  .console <- rep(list(""), length(xs))
  .w <- list()
  .e <- withCallingHandlers(
    tryCatch({
      setTimeLimit(timeout, timeout, TRUE)
      with_env_cleanup({
        .duration <- system.time(for (i in seq_along(xs)) {
          .console[[i]] <- paste(capture.output(
            u <- eval(bquote(with(envir, .(xs[[i]]))))),
            collapse = "\n")
          })
      })
      setTimeLimit(Inf, Inf)
      TRUE
    },
    error = function(e) e),
    warning = function(w) {
      .w <<- append(.w, list(w))
      invokeRestart('muffleWarning')
    })
  setTimeLimit(Inf, Inf)

  if (inherits(.e, "error"))
    riddlr_result(.e, expr = xs, console = .console)
  else
    riddlr_result(u, expr = xs, duration = .duration, console = .console,
      warnings = .w %||% NULL)
})



#' Format a riddlr response as HTML output
#'
#' @importFrom shiny div HTML
#' @importFrom markdown markdownToHTML
#' @export
#'
riddle_response_html <- function(x) {
  class <- c(x$class, "riddlr-alert")
  class_inner <- setdiff(class, "alert")

  class <- paste(class, collapse = " ")
  class_inner <- paste(class_inner, collapse = " ")

  shiny::div(
    class = class,
    shiny::HTML(
      # silly hack to inherit alert stylings
      gsub("<(code|pre)>", sprintf("<\\1 class='%s'>", class_inner),
        markdown::markdownToHTML(
          text = x$message,
          fragment.only = TRUE,
          stylesheet = ""))))
}



#' Response for an incorrect solution
#'
#' @importFrom shiny HTML
#' @importFrom markdown markdownToHTML
#'
incorrect_solution_response <- function(user_output, solution_output,
    input_env, test_i, test_n, err) {

  list(
    message = sprintf(paste0(
        "Sorry, your code failed on one of the test cases (%s of %s). ",
        "  \n  \n",
        "**Your Solution:**  \n```  \n%s  \n```",
        "  \n  \n",
        "**Correct Solution:**  \n```  \n%s  \n```",
        "  \n  \n",
        "**Input:**  \n```  \n%s  \n```"),
      test_i, test_n,
      paste(capture.output(user_output), collapse = '  \n'),
      paste(capture.output(solution_output), collapse = '  \n'),
      paste(capture.output(as.list(input_env)), collapse = '  \n')),
    class = c("alert", "alert-warning"))
}




#' Response for a solution that times out
#'
#' @importFrom shiny HTML
#' @importFrom markdown markdownToHTML
#'
#' @export
#'
timeout_response <- function(x, solution_output,
    input_env, test_i, test_n) {

  list(
    message = sprintf(paste0(
        "Sorry, your code was terminated prematurely for taking ",
        "too long to execute while running test case (%s of %s). ",
        "  \n  \n**Input:**  \n```  \n%s  \n```"),
      test_i, test_n,
      paste(capture.output(as.list(input_env)), collapse = '  \n')),
    class = c("alert", "alert-warning"))
}



#' Response for a solutions that produce errors
#'
#' @importFrom shiny HTML
#' @importFrom markdown markdownToHTML
#'
#' @export
#'
parse_error_response <- function(x, ...) {
  list(
    message = sprintf(paste0(
      "**Error:**  \n```  \n%s  \n```"),
      paste(capture.output(cat(x$message)), collapse = '  \n')),
    class = c("alert", "alert-danger"))
}



#' Response for a solutions that produce errors
#'
#' @importFrom shiny HTML
#' @importFrom markdown markdownToHTML
#'
#' @export
#'
error_response <- function(x, solution_output,
    input_env, test_i, test_n) {

  list(
    message = sprintf(paste0(
        "Sorry, your code encountered an error while running one of ",
        "the test cases (%s of %s). ",
        "  \n  \n**Error:**  \n```  \n%s  \n```"),
      test_i, test_n,
      paste(capture.output(cat(x$message)), collapse = '  \n')),
    class = c("alert", "alert-danger"))
}



#' Response for a solution that times out only a grace period
#'
#' @importFrom shiny HTML
#' @importFrom markdown markdownToHTML
#'
#' @export
#'
grace_timeouts_response <- function(user_output, solution_output, input_env,
    test_i, test_n, time_frac) {

  list(
    message = sprintf(paste0(
        "Sorry, although your solution was correct, your code ",
        "took too long to run on one of ",
        "the test cases (%s of %s). Your code took **%.2fx** the ",
        "allowed time and was terminated before evaluating further ",
        "test cases."),
      test_i, test_n, time_frac),
    class = c("alert", "alert-warning"))
}



#' Response for successful solution
#'
#' @importFrom shiny HTML
#' @importFrom markdown markdownToHTML
#'
#' @export
#'
success_response <- function(hash = runif(1)) {
  congrats <- list(
    paste("Nice job", icon("thumbs-up")),
    paste("Cheers", icon("beer")),
    paste("That went swimmingly", icon("swimmer")),
    paste("Code cracked", icon("code")),
    paste("Top notch", icon("award")),
    # paste("Like taking candy from a baby", icon("baby-carriage")),
    paste("Brainiac", icon("brain")),
    paste("Moving on up", icon("chart-line")),
    paste("The magic touch", icon("fingerprint")),
    paste("Touchdown", icon("football-ball")),
    # paste("That's the spirit", icon("ghost")),
    paste("Goooooooal", icon("futbol")),
    paste("Wizkid", icon("graduation-cap")),
    # paste("Smarty cat", icon("cat")),
    paste("Briliant", icon("lightbulb")),
    paste("Award winning", icon("medal")),
    # paste("You pieced it together", icon("puzzle-piece")),
    # paste("One solution to rule them all", icon("ring")),
    paste("Full throttle", icon("tachometer-alt")),
    paste("That's the ticket", icon("ticket-alt")),
    paste("Have a trophy", icon("trophy")),
    paste("A winning formula", icon("vial")),
    paste("Certified awesome", icon("certificate")),
    paste("Eureka", icon("apple-alt")))

  # poor man's hash
  i <- sum(utf8ToInt(capture.output(hash))) %% length(congrats) + 1

  list(
    message = paste0(
      "## ", congrats[[i]], "  \n",
      "Your solution passes all the test cases!"),
    class = c("alert", "alert-success"))
}
