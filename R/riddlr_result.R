riddlr_result <- function(result, expr = NULL, duration = NULL,
    console = NULL, warnings = NULL) {

  r_class <- c("riddlr_result", class(result))
  if (!is.null(warnings)) r_class <- c(r_class, "warning")

  structure(result,
    expr = expr,
    warnings = warnings,
    duration = duration,
    console = console,
    class = r_class)
}



unclass_riddlr_result <- function(x) {
  x <- unclass(x)
  attributes(x) <- NULL
  x
}



#' @export
print.riddlr_result <- function(x, ...) {
  print(unclass_riddlr_result(x), ...)
}



format.riddlr_result <- function(x, ...) {
  format(unclass_riddlr_result(x), ...)
}



Ops.riddlr_result <- function(e1, e2) {
  if (.Generic == "==") {
    e1 <- unclass_riddlr_result(e1)
    e2 <- unclass_riddlr_result(e2)
    return(identical(e1, e2))
  } else if (.Generic == "!=") {
    return(!(e1 == e2))
  }
}
