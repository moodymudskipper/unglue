#' Detect if strings are matched by a set of unglue patterns
#'
#' Returns a logical indicating whether input strings were matched by one or more patterns
#' @inheritParams  unglue
#' @return a vector of logical.
#' @export
#' @examples
#' sentences <- c("666 is [a number]", "foo is [a word]",
#'                "42 is [the answer]", "Area 51 is [unmatched]")
#' patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
#' unglue_detect(sentences, patterns)
unglue_detect  <- function(
  x, patterns, open = "{", close = "}", convert = FALSE, multiple = NULL){
  patterns_regex <- unglue_regex(
    patterns, open = open, close = close, use_multiple = !is.null(multiple),
    named_capture = FALSE, attributes = TRUE)
  unglue_data0(x, patterns_regex, convert, multiple, output = "logical")
}

#' unglue
#'
#' The functions `unglue_data()`, `unglue()`, `unglue_vec()` and `unglue_unnest()`
#' extract matched substrings using a syntax inspired from `glue::glue()`.
#' Simple cases don't require regex knowledge at all.
#'
#' Depending on the task you might want:
#' * `unglue_data()` to return a data frame from a character vector,
#'   just as `glue::glue_data()` does in reverse
#' * `unglue()` to return a list of data frames containing the matches
#' * `unglue_vec()` to extract one value by element of `x`, chosen by index or by
#'   name.
#' * `unglue_unnest()` to extract value from a column of a data frame to new columns
#'
#' To build the relevant regex pattern special characters will be escaped in the
#' input pattern and the subpatterns will be replaced with `(.*?)` if in standard
#' `"{foo}"` form. An alternate regular expression can be provided after `=` so that
#' `"{foo=\\d}"` will be translated into `"(\\d)"`.
#'
#' Sometimes we might want to use regex to match a part of the text that won't
#' be extracted, in these cases we just need to omit the name as in `"{=\\d}"`.
#'
#' `unglue_unnest()`'s name is a tribute to `tidyr::unnest()` because
#'  `unglue_unnest(data, col, patterns)` returns a similar output as
#'  `dplyr::mutate(data, unglued = unglue(col, patterns)) %>% tidyr::unnest()`
#'  (without requiring any extra package).
#'  It is also very close to `tidyr::extract()` and efforts were made to make
#'  the syntax consistent with the latter.
#'
#' @param x a character vector to unglue.
#' @param data a data frame.
#' @param patterns a character vector or a list of character vectors, if a list,
#'   items will be pasted using an empty separator (`""`).
#' @param open The opening delimiter.
#' @param close The closing delimiter.
#' @param convert If `TRUE`, will convert columns of output using
#'   `utils::type.convert()` with parameter `as.is = TRUE`, alternatively, can
#'   be a converting function, such as `readr::type_convert`. Formula notation
#'   is supported if the package `rlang` is installed, so things like
#'   `convert = ~type_convert(., numerals = "warn.loss")` are possible.
#' @param multiple The aggregation function to use if several subpatterns are
#'   named the same, by default no function is used and subpatterns named the
#'   same will match the same value. If a function is provided it will be fed
#'   the conflicting values as separate arguments. Formula notation
#'   is supported if the package `rlang` is installed.
#' @param col column containing the character vector to extract values from.
#' @param remove whether to remove the column `col` once extraction is performed
#' @param var for `unglue_vec()`, the numeric index or the name of the subpattern to extract from
#' @param na string to use when there is no match
#' @return For `unglue()`a list of one row data frames, for `unglue_data` a
#'   data frame, for `unglue_unnest` the data frame input with additional columns
#'   built from extracted values, for `unglue_vec` an atomic vector.
#' @export
#'
#' @examples
#' # using an awample from ?glue::glue
#' if(require(magrittr) && require(glue)) {
#'   glued_data <- mtcars %>% glue_data("{rownames(.)} has {hp} hp")
#'   unglue_data(glued_data, "{rownames(.)} has {hp} hp")
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
#' sentences <- c("666 is [a number]", "foo is [a word]",
#'               "42 is [the answer]", "Area 51 is [unmatched]")
#' patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
#' unglue_data(sentences, patterns)
#'
#' unglue_unnest(facts_df, facts, patterns)
#' unglue_unnest(facts_df, facts, patterns, remove = FALSE)
unglue  <- function(
  x, patterns, open = "{", close = "}", convert = FALSE, multiple = NULL){
  patterns_regex <- unglue_regex(
    patterns, open = open, close = close, use_multiple = !is.null(multiple),
    named_capture = FALSE, attributes = TRUE)
  unglue_data0(x, patterns_regex, convert, multiple, output = "list")
}

#' @rdname unglue
#' @export
#'
unglue_data  <- function(
  x, patterns, open = "{", close = "}", convert = FALSE, multiple = NULL,
  na = NA_character_){
  patterns_regex <- unglue_regex(
    patterns, open = open, close = close, use_multiple = !is.null(multiple),
    named_capture = FALSE, attributes = TRUE)
  unglue_data0(x, patterns_regex, convert, multiple, output = "data.frame", na = na)
}



#' @rdname unglue
#' @export
unglue_vec  <- function(
  x, patterns, var = 1, open = "{", close = "}", convert = FALSE, multiple = NULL, na = NA_character_){
  patterns_regex <- unglue_regex(
    patterns, open = open, close = close, use_multiple = !is.null(multiple),
    named_capture = FALSE, attributes = TRUE)
  if((!is.character(var) && !is.numeric(var)) || length(var) != 1){
    stop("var should be a character or numeric of length 1")
  }
  unglue_vec0(x, patterns_regex, var, convert = convert, multiple = multiple, na = na)
}


#' @rdname unglue
#' @export
unglue_unnest <- function(data, col, patterns, open = "{", close = "}",remove = TRUE, convert = FALSE, multiple = NULL, na = NA_character_){
  # save the attributes of our data.frame, except for the names
  attr_bkp <- attributes(data)
  attr_bkp$names <- NULL
  # use unglue_data on the column whose raw name is fed to `var`
  col <- deparse(substitute(col))
  ud  <- unglue_data(data[[col]], patterns, open = open, close = close, convert = convert, multiple = multiple, na = na)
  # remove this column if relevant
  if(remove) data[[col]] <- NULL
  # bind the source and built datasets together
  res <- cbind(data, ud)
  # make unique names in case of duplicated
  names(res) <- make.unique(names(res))
  # restore the attributes
  attributes(res) <- c(attr_bkp, list(names = names(res)))
  res
}
