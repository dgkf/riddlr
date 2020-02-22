#' @export
run_riddle <- function(user_code, timeout = Inf, envir = parent.frame()) {
  envir <- as.environment(envir)
  parent.env(envir) <- riddlr_base_env

  user_expr <- parse_safe(text = user_code)

  if (is_error(user_expr))
    return(parse_error_response(user_expr))

  user_result <- eval_with_timeout(user_expr,
    timeout = timeout,
    envir = new.env(parent = envir))

  if (is_timeout_error(user_result))
    return(simple_timeout_response(user_result, timeout))

  if (is_error(user_result))
    return(simple_error_response(user_result))

  if (is_warning(user_result))
    return(simple_warning_response(user_result))

  simple_console_response(user_result)
}



build_console_out <- function(exprs, console) {
  if (length(exprs) == length(console)) {
    gsub("(^\\n+|\\n+$)", "", paste0(
      paste0("> ", as.character(exprs), "  \n"),
      ifelse(sapply(console, length) > 0 & sapply(console, nchar) > 0,
        paste0(console, "  \n"), ""),
      collapse = ""))
  } else if (!is.null(console)) {
    paste(console, collapse = "  \n")
  } else {
    ""
  }
}



simple_timeout_response <- function(x, timeout = Inf) {
  list(message = paste0(
      "**Timeout**: Your code timed out ",
      if (is.finite(timeout))
        sprintf("after running for %ds", ceiling(timeout)),
      ".",
      if (!is.null(console <- attr(x, "console")))
        sprintf("  \n  \n```   \n%s   \n```",
          paste(console, collapse = "  \n"))),
    class = c("alert", "alert-warning"))
}



simple_error_response <- function(x) {
  console.out <- build_console_out(attr(x, "expr"), attr(x, "console"))

  list(message = paste0(
      if (nchar(console.out))
        sprintf("**Console Output**:  \n```   \n%s   \n```  \n", console.out),
      sprintf("**Error**:  \n```  \n%s  \n```", x$message)),
    class = c("alert", "alert-danger"))
}



simple_warning_response <- function(x) {
  print('attempt to build simple warning')
  console.out <- build_console_out(attr(x, "expr"), attr(x, "console"))

  list(message = paste0(
    if (nchar(console.out))
      sprintf("**Console Output**:  \n```   \n%s   \n```  \n", console.out),
    sprintf("**Warning**:  \n```  \n%s  \n```",
      paste(lapply(attr(x, "warnings"), "[[", "message"), collapse = "  \n"))),
    class = c("alert", "alert-warning"))
}



simple_console_response <- function(x) {
  console.out <- build_console_out(attr(x, "expr"), attr(x, "console"))

  list(message = paste0(
    if (nchar(console.out))
      sprintf("**Console Output**:  \n```   \n%s   \n```  \n", console.out),
    sprintf("```  \n%s  \n```", paste(capture.output(x), collapse = "  \n"))),
    class = c("alert", "alert-info"))
}
