
# escape all characters that need escaping
regex_escape <- function(string,n = 1) {
  for(i in seq_len(n)){
    string <- gsub("([][{}()+*^$|\\\\?])", "\\\\\\1", string)
  }
  string
}

pattern_match <- function(x,patterns){
  # lapply + do.call + cbind guarantee that we get a matrix output
  m <- do.call(cbind,lapply(patterns, grepl, x))
  # get indices of 1st relevant pattern
  i <- apply(m, 1, which.max)
  # but which.max returns 1 something on a vector of FALSE so need to adjust
  i[!rowSums(m)] <- NA
  # return pattern indices
  i
}

#' unglue
#'
#' `unglue()` extracts matched substrings using a syntax inspired from `glue::glue()`
#' into a list of data frames. `unglue_data()` returns a data frame from a vector,
#' just as `glue::glue_data()` starts from a data frame to return a vector.
#'
#' Simple cases don't require regex knowledge at all.
#'
#' To build the relevant regex pattern special characters will be escaped in the
#' input pattern and the subpatterns will be replaced with `(.*?)` if in standard
#' `"{foo}"` form. An alternate regular expression can be provided after `=` so that
#' `"{foo=\\d}` will be translated into `"(\\d)"`.
#'
#' Sometimes we might want to use regex to match a part of the text that won't
#' be extracted, in these cases we just need to omit the name as in `"{=\\d}`.
#'
#' @param x a character vector to unglue
#' @param patterns a character vector or a list of character vectors, if a list,
#'   items will be with a `""` separator.
#' @param open The opening delimiter.
#' @param close The closing delimiter.
#' @param convert convert columns of output using `utils::type.convert()`
#'
#' @return `unglue_data()` returns a data frame, `unglue()` returns a list of
#' 1 row data frames.
#' @seealso unglue_unnest
#' @export
#'
#' @examples
#' # using an awample from ?glue::glue
#' \dontrun{
#' library(magrittr)
#' library(glue)
#' glued_data <- mtcars %>% glue_data("{rownames(.)} has {hp} hp")
#' unglue_data(glued_data, "{rownames(.)} has {hp} hp")
#' }
#'
#' facts <- c("Antarctica is the largest desert in the world!",
#' "The largest country in Europe is Russia!",
#' "The smallest country in Europe is Vatican!",
#' "Disneyland is the most visited place in Europe! Disneyland is in Paris!",
#' "The largest island in the world is Green Land!")
#' facts_df <- data.frame(id = 1:5, facts)
#'
#' patterns <- c("The {adjective} {place_type} in {bigger_place} is {place}!",
#'             "{place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*}")
#' unglue_data(facts, patterns)
#'
#'\dontrun{
#' # unglue() is more suitable than unglue_data() in pipe chains:
#' library(tidyverse)
#' facts_df %>%
#'   mutate(unglued = unglue(facts, patterns)) %>%
#'   unnest()
#' # though in these cases it can be more convenient to use the more compact
#' # and dependence free `unglue_unnest`
#' }
#'
#' sentences <- c("666 is [a number]", "foo is [a word]",
#'               "42 is [the answer]", "Area 51 is [unmatched]")
#' patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
#' unglue_data(sentences, patterns)
#'
#'
unglue <- function(x, patterns, open = "{", close = "}", convert = TRUE){
  # going through unglue_data is a way to have identical names for all items
  # and to apply convert
  ud <- unglue_data(x, patterns, open = open, close = close, convert = convert)
  split(ud, seq_len(nrow(ud)))
}

#' @rdname unglue
#' @export
unglue_data <- function(
  x, patterns, open = "{", close = "}", convert = TRUE){
  if(!isTRUE(all(nchar(c(open, close)) == 1)))
    stop("open and close must be a single character")
  if(open == close)
    stop("open and close can't be the same character")
  # collapse patterns
  patterns <- sapply(patterns, paste, collapse = "")
  # escape variable delimiters
  open1 <- regex_escape(open)
  close1 <- regex_escape(close)
  # extract bracket content
  bracket_pattern <- paste0(open1,"(?>[^",open,close,"]|(?R))*", close1)
  matched <- gregexpr(bracket_pattern, patterns, perl = T)
  bracket_content <-
    Map(function(x,y) substring(x, y+1, y + attr(y, "match.length") - 2),
           patterns, matched)

  # or regmatches(patterns, matched) but removing first and last
  L <- lapply(bracket_content, function(x){
    eq <- grepl("=", x)
    nms <- x
    nms[eq] <- gsub("^(.*?)\\=.*","\\1", nms[eq])
    subpat <- rep.int("(.*?)", length(x))
    subpat[eq & nms !=""] <- gsub("^.*?\\=(.*)","(\\1)", x[eq & nms!=""])
    subpat[eq & nms ==""] <- gsub("^.*?\\=(.*)",  "\\1", x[eq & nms==""])
    nms <- nms[nms!=""]
    list(nms, subpat)
  })
  nms <- lapply(L, `[[`, 1)
  subpat <- lapply(L, `[[`, 2)

  # simplify patterns now that we've extracted the relevant content
  # it changes all "{foo}" to "\\{\\}", including cases with nested "{"

  regmatches(patterns, matched) <- paste0(open1,close1)
  # escaping is necessary to make the following replacements and have an unambiguous pattern
  patterns <- gsub(strrep(open1,2),open, patterns)
  patterns <- gsub(strrep(close1,2),close, patterns)
  open2  <- regex_escape(open1,2)
  close2 <- regex_escape(close1,2)

  # we escape all the relevant characters in the patterns, which means
  # contained open and close will be escaped too, so to match them we need to
  # escape them 2 times
  patterns_regex <- regex_escape(patterns)
  matched <- gregexpr(paste0(open2,close2), patterns_regex, perl = T)
  regmatches(patterns_regex, matched) <- subpat

  # complete the pattern with start and end of string
  patterns_regex <- paste0("^",patterns_regex,"$")
  # assign a pattern to each element
  pattern_indices <- pattern_match(x, patterns_regex)
  # initiate a list of results
  res <- as.list(rep.int(NA,length(x)))
  # assign a tibble of matches to each result from each pattern
  for(i in seq_along(patterns)){
    subset_ind <- which(pattern_indices == i)
    matched <- gregexpr(patterns_regex[i], x[subset_ind], perl = T)
    res_i <- Map(function(x,y) substring(x, attr(y, "capture.start") , attr(y, "capture.start") + attr(y, "capture.length") - 1),
                     x[subset_ind], matched)
    res_i <- lapply(res_i, function(x) as.data.frame(setNames(as.list(x), nms[[i]]), stringsAsFactors = FALSE))
    res[subset_ind] <- res_i
  }
  # replace NA elements by an empty single row tibble
  na_indices <- is.na(pattern_indices)
  res[na_indices] <- replicate(sum(na_indices), data.frame(row.names = 1))
  # bind everything
  res <- bind_rows2(res)
  rownames(res) <- NULL
  # convert if relevant
  if (convert) res <- utils::type.convert(res, as.is = TRUE)
  res
}

# simulate bind_rows to spare a dependency
# bind_rows2(list(mtcars[1:2, 1:3], mtcars[1:2, 2:4]))
bind_rows2 <- function(x){
  nms <- unique(unlist(sapply(x, names)))
  x[] <- lapply(x,function(x) {x[nms[!nms %in% names(x)]] <- NA; x})
  do.call(rbind,x)
}

setNames <-function (object = nm, nm) {
  names(object) <- nm
  object
}


#' unglue a column and unnest it
#'
#' `unglue_unnest()` extracts variables as new columns of the input data frame.
#'
#'  `unnest(data, var, patterns)` is similar to
#'  `dplyr::mutate(data, unglued = unglue(var, patterns)) %>% tidyr::unnest()`
#'  using tidyverse packages but is more compact and dependence free
#'
#' @inheritParams unglue
#' @param data a data frame
#' @param var the unquoted name of the column to unglue and unnest
#' @param keep wether to keep the original text column
#'
#' @return a data frame with the same attributes as the input (apart from names)
#' @seealso unglue, unglue_data
#' @export
#' @examples
#' facts <- c("Antarctica is the largest desert in the world!",
#'   "The largest country in Europe is Russia!",
#'   "The smallest country in Europe is Vatican!",
#'   "Disneyland is the most visited place in Europe! Disneyland is in Paris!",
#'   "The largest island in the world is Green Land!")
#' facts_df <- data.frame(id = 1:5, facts)
#' patterns <- c("The {adjective} {place_type} in {bigger_place} is {place}!",
#'             "{place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*}")
#' unglue_unnest(facts_df, facts, patterns)
#' unglue_unnest(facts_df, facts, patterns, keep = FALSE)
unglue_unnest <- function(data, var, patterns, open = "{", close = "}", convert = TRUE, keep = TRUE){
  attr_bkp <- attributes(data)
  attr_bkp$names <- NULL
  var <- deparse(substitute(var))
  ud  <- unglue_data(data[[var]], patterns, open = open, close = close, convert = convert)
  if(!keep) data[[var]] <- NULL
  res <- cbind(data, ud)
  names(res) <- make.unique(names(res))
  attributes(res) <- c(attr_bkp, list(names = names(res)))
  res
}
