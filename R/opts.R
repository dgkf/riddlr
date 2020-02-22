#' Getters and setters for riddlr options
#'
#' \describe{
#'   \item{onSubmit}{
#'     Expects a function expecting arguments \code{code}, \code{duration} and
#'     \code{response}. This function is called after a question solution is
#'     submitted and graded and can be used as a callback for additional
#'     functionality or logging.
#'   }
#' }
#'
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' opts$set(onSubmit = function(code, duration, response) message("submitted"))
#'
opts <- list(
  get = function(name) riddlr:::.opts[[name]],
  set = function(...) {
    assignInMyNamespace(".opts", utils::modifyList(riddlr:::.opts, list(...)))
  })



.opts <- list()
.opts$onSubmit <- function(code, duration, response) { }
