`substr2<-` <- function(x, start, stop, value){
  paste0(substr(x, 1, start-1), value, substr(x, stop+1, nchar(x)))
}



#' unglue_sub
#'
#' substitute substrings using strings or replacement functions
#'
#' @inheritParams unglue
#' @param x character vector
#' @param repl function to apply on matched substrings, formula (if package rlang
#'   is installed), substring, or named list of such.
#'
#' @export
#'
#' @examples
#' unglue_sub(
#'   c("a and b", "foo or bar"),
#'   c("{x} and {y}", "{x} or {z}"),
#'   "XXX")
#'
#' unglue_sub(
#'   c("a and b", "foo or bar"),
#'   c("{x} and {y}", "{x} or {z}"),
#'   toupper)
#'
#' unglue_sub(
#'   c("a and b", "foo or BAR"),
#'   c("{x} and {y}", "{x} or {z}"),
#'   list(x= "XXX", y = toupper, z = tolower))
#'
unglue_sub <- function(
  x, patterns, repl, open = "{", close = "}"){
  patterns_regex <- unglue_regex(
    patterns, open = open, close = close, use_multiple = FALSE,
    named_capture = FALSE, attributes = TRUE)
  # get numeric index of relevant pattern for each element of x
  pattern_indices <- pattern_match(x, patterns_regex)

  if (is.character(repl)) {
    repl <- as.function(c(alist(x=), repl))
  } else if (inherits(repl, "formula")) {
    if(!requireNamespace("rlang"))
      stop("rlang package must be installed to use formula notation in `repl` argument of unglue functions")
    repl <- rlang::as_function(repl)
  } else if (is.list(repl)) {
    repl <- lapply(repl, function(x){
      if (is.character(x)) {
        as.function(c(alist(x=), x))
      } else if (inherits(x, "formula")) {
        if(!requireNamespace("rlang"))
          stop("rlang package must be installed to use formula notation in `repl` argument of unglue functions")
        rlang::as_function(x)
      } else x
    })
  }

  # assign a tibble of matches to each result from each pattern
  for(i in seq_along(patterns_regex)){
    # get numeric index of elements of x that are matched by i-th pattern
    subset_ind <- which(pattern_indices == i)
    x_subset <- x[subset_ind]
    matched <- gregexpr(patterns_regex[i], x_subset, perl = T)
    # `groups` are indices or relevant named captured groups
    groups  <- attr(patterns_regex, "groups")[[i]]
    nms     <- rep(names(groups),lengths(groups))
    if (is.function(repl)){
      funs <- replicate(length(nms), repl)
      funs <- setNames(funs, nms)
    } else {
      funs <- repl[nms]
    }

    x[subset_ind] <- mapply(function(x,y) {
      for (i in rev(seq_along(funs))){
        start <- attr(y, "capture.start")[[i]]
        end  <- start + attr(y, "capture.length")[[i]] - 1
        substr2(x, start, end) <- funs[[i]](substr(x, start, end))
      }
      x},
      x[subset_ind], matched)
  }

  x
}
