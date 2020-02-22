#'
#' @importFrom rmarkdown yaml_front_matter
#' @export
#'
parse_riddlr_dir_headers <- function(path, pattern = ".riddlr.Rmd") {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  names(files) <- files
  unname(Map(function(filepath) {
    i <- rmarkdown::yaml_front_matter(filepath)
    i$filepath <- filepath
    i
  }, files))
}



#'
#' @importFrom rmarkdown render html_fragment
#' @export
#'
parse_riddlr_rmd <- function(file,
    text = paste0(readLines(file, warn = FALSE), collapse = "\n")) {

  chunks <- split_header(text)
  header <- chunks[,"header"]
  chunks <- split_chunks(chunks[,"body"])

  grader <- tryCatch(
    eval(parse(text = chunks$grader), envir = new.env(parent = topenv())),
    error = function(e) {
      message("An error was encountered while trying to evaluate ",
        "chunk \"grader\"")
      stop(e)
    })

  starter <- trimws(chunks$starter)
  chunks <- chunks[-which(names(chunks) %in% c("grader", "starter"))]
  chunks <- trimws(paste(chunks, collapse = "\n"))

  tryCatch({
    tmp_in <- tempfile(fileext = ".Rmd")
    tmp_out <- tempfile(fileext = ".html")
    cat(chunks, file = tmp_in)
    rmarkdown::render(tmp_in,
      output_format = rmarkdown::html_fragment(),
      output_file = tmp_out,
      quiet = TRUE)
    }, error = function(e) {
      message("An error was encountered while attempting to render remaining ",
        "chunks to markdown")
      stop(e)
    })

  metadata <- yaml::read_yaml(text = header)
  metadata$ace$value <- starter

  list(
    metadata = metadata,
    prompt = HTML(paste(readLines(tmp_out), collapse = "\n")),
    grader = grader)
}


split_header <- function(x) {
  re_match <- gregexpr("(?s)---(?<header>.*)---(?<body>.*)", x, perl = TRUE)[[1]]
  re_capture <- substring(x,
    cs <- attr(re_match, "capture.start"),
    cs + attr(re_match, "capture.length") - 1)
  dim(re_capture) <- dim(attr(re_match, "capture.start"))
  colnames(re_capture) <- attr(re_match, "capture.names")
  re_capture
}


split_chunks <- function(x) {
  chunk_re <- paste0(
    "(?s)```\\{\\s*",           # start of chunk header
    "(?<engine>\\w+)",          # engine name
    "\\s*",                     # possible spaces
    "(?<name>[^,}]*)",          # chunk name
    "(?<args>,[^,}]*)*",        # additional header args
    "\\}", # chunk start        # to end of chunk header
    "(?<body>(?:.(?<!```))*)",  # chunk body
    "```")                      # chunk end

  re_match <- gregexpr(chunk_re, x, perl = TRUE)[[1]]
  re_capture <- substring(x,
    cs <- attr(re_match, "capture.start"),
    cs + attr(re_match, "capture.length") - 1)
  dim(re_capture) <- dim(attr(re_match, "capture.start"))
  colnames(re_capture) <- attr(re_match, "capture.names")

  splits <- sort(unique(c(
    1,
    re_match - 1,
    re_match + attr(re_match, "match.length"),
    nchar(x) + 1)))

  captured_splits <- which(splits %in% (re_match - 1))
  splits <- substring(x, head(splits, -1), tail(splits, -1))
  names(splits) <- rep("", length(splits))
  names(splits)[captured_splits] <- re_capture[,"name"]
  splits[captured_splits] <- re_capture[,"body"]

  as.list(splits)
}

