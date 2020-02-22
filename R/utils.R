`%||%` <- function(lhs, rhs) if (is.null(lhs) || !length(lhs)) rhs else lhs



#' Compare an error against a timeout error from setTimeLimit
#'
#' @param e an error to compare
#'
#' @return a logical indicating whether e is a setTimeLimit timeout error
#' @export
is_timeout_error <- function(e) {
  if (!"error" %in% class(e)) return(FALSE)
  (identical(attributes(e), attributes(timeout_error_cpu)) ||
  identical(attributes(e), attributes(timeout_error_elapsed))) &&
  (identical(e$message, timeout_error_cpu$message) ||
  identical(e$message, timeout_error_elapsed$message))
}

#' compute a timeout error from setTimeLimit
timeout_error_cpu <- tryCatch({
  setTimeLimit(cpu = 0.0001, elapsed = Inf, transient = TRUE)
  Sys.sleep(0.001)
}, error = function(e) e)

timeout_error_elapsed <- tryCatch({
  setTimeLimit(cpu = Inf, elapsed = 0.0001, transient = TRUE)
  Sys.sleep(0.001)
}, error = function(e) e)



is_error <- function(x) return(inherits(x, "error"))
is_warning <- function(x) return(inherits(x, "warning"))



#' @export
modifyCssClasses <- function(x, ...) {
  dots <- as.list(match.call())[-1]
  dots <- gsub("\\s+", "", as.character(dots[names(dots) == ""]))

  classes <- strsplit(x$attribs$class, " ")[[1]]
  classes <- setdiff(classes, gsub("^-", "", dots[grepl("^-", dots)]))
  classes <- sort(c(classes, dots[grepl("^[^-]", dots)]))

  x$attribs$class <- paste(classes, collapse = " ")
  x
}



#' @export
bootstrapify <- function(x) {
  gsub("<tr>", "<tr class='header'>",
  gsub("<table>", "<table class='table table-condensed'",
    x))
}
