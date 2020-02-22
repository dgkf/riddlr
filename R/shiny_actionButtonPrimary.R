modifyCssClasses <- function(x, ...) {
  dots <- as.list(match.call())[-1]
  dots <- gsub("\\s+", "", as.character(dots[names(dots) == ""]))

  classes <- strsplit(x$attribs$class, " ")[[1]]
  classes <- setdiff(classes, gsub("^-", "", dots[grepl("^-", dots)]))
  classes <- sort(c(classes, dots[grepl("^[^-]", dots)]))

  x$attribs$class <- paste(classes, collapse = " ")
  x
}
