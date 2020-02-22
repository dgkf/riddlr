#' Build a base environment for solutions to be evaluated within
riddlr_base_env <- Reduce(function(l, r) { parent.env(l) <- r; l },
  lapply(c("methods", "datasets", "utils", "tools", "stats"), getNamespace),
  init = new.env(parent = getNamespace("base")))



#' Helper to ensure that any library calls get undone by the end of evaluation
with_env_cleanup <- function(expr, envir = parent.frame()) {
  start_env_pos <- length(search())
  on.exit({
    while (length(search()) > start_env_pos)
      detach(attr(pos.to.env(2L), "name"),
        unload = TRUE,
        character.only = TRUE)
  })
  eval(expr, envir = envir)
}
