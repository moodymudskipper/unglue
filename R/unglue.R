#' unglue
#'
#' The functions from the package *unglue* extract matched substrings using a
#' syntax inspired from `glue::glue()`.
#' Simple cases don't require regex knowledge at all.
#'
#' Depending on the task you might want:
#' * `unglue_data()` to return a data frame from a character vector,
#'   just as `glue::glue_data()` does in reverse
#' * `unglue()` to return a list of data frames containing the matches
#' * `unglue_vec()` to extract one value by element of `x`, chosen by indice or by
#'   name.
#' * `unglue_unnest()` to extract value from a column of a data frame to new columns
#' * `unglue_to_regex()` to transform a vector of patterns given in the unglue
#'   format to a vector of proper  regex (PCRE) patterns (so they can for instance
#'   be used with functions from other packages).
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
#' @param remove wether to remove the column `col` once extraction is performed
#' @param var the numeric index or the name of the subpattern to extract from
#' @param named_capture wether to incorporate the names of the groups in the
#'   ouput regex
#' @param attributes wether to give group attributes to the output
#'
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
#' sentences <- c("666 is [a number]", "foo is [a word]",
#'               "42 is [the answer]", "Area 51 is [unmatched]")
#' patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
#' unglue_data(sentences, patterns)
#'
#' unglue_unnest(facts_df, facts, patterns)
#' unglue_unnest(facts_df, facts, patterns, remove = FALSE)
unglue  <- function(
  x, patterns, open = "{", close = "}", convert = FALSE, multiple = NULL){
  patterns_regex <- unglue_to_regex(
    patterns, open = open, close = close, multiple = multiple,
    named_capture = FALSE, attributes = TRUE)
  unglue_data0(x, patterns_regex, convert, multiple, output = "list")
}

#' @rdname unglue
#' @export
#'
unglue_data  <- function(
  x, patterns, open = "{", close = "}", convert = FALSE, multiple = NULL){
  patterns_regex <- unglue_to_regex(
    patterns, open = open, close = close, multiple = multiple,
    named_capture = FALSE, attributes = TRUE)
  unglue_data0(x, patterns_regex, convert, multiple, output = "data.frame")
}

#' @rdname unglue
#' @export
unglue_vec  <- function(
  x, patterns, var = 1, open = "{", close = "}", convert = FALSE, multiple = NULL){
  patterns_regex <- unglue_to_regex(
    patterns, open = open, close = close, multiple = multiple,
    named_capture = FALSE, attributes = TRUE)
  if((!is.character(var) && !is.numeric(var)) || length(var) != 1){
    stop("var should be a character or numeric of length 1")
  }
  unglue_vec0(x, patterns_regex, var, convert = convert)
}

#' @rdname unglue
#' @export
unglue_to_regex <- function(
  patterns, open = "{", close = "}", multiple = NULL,
  named_capture = FALSE, attributes = FALSE){
  if(!is.null(multiple) && named_capture){
    stop("named_capture can be TRUE only when used with default multiple = NULL")
  }
  if(!isTRUE(all(nchar(c(open, close)) == 1)))
    stop("open and close must be a single character")
  if(open == close)
    stop("open and close can't be the same character")
  # collapse patterns into list of strings (or single string)
  patterns <- sapply(patterns, paste, collapse = "")
  # escape variable delimiters
  open1 <- regex_escape(open)
  close1 <- regex_escape(close)
  # define pattern which will help extract the content of our brackets
  bracket_pattern <- paste0(open1,"(?>[^",open,close,"]|(?R))*", close1)
  # matched will be a list containing for each pattern
  # the starting position of matches (in a vector) and the matches length (as attributes)
  matched <- gregexpr(bracket_pattern, patterns, perl = TRUE)
  # extract from patterns : subpatterns, names and group indices
  L <- parse_brackets(patterns, matched, multiple, named_capture = named_capture)
  subpat <- lapply(L, `[[`, "subpatterns")

  patterns_regex <- patterns
  # clean up patterns now that we've extracted the relevant content
  # it changes all "{...}" to "\\{\\}" placeholders, including cases with nested "{"
  regmatches(patterns_regex, matched) <- paste0(open1,close1)

  # escape double delimiters to consider them as one, the actual delimiters have
  # been escaped through previous step so are safe
  patterns_regex <- gsub(strrep(open1,2) ,open, patterns_regex)
  patterns_regex <- gsub(strrep(close1,2),close, patterns_regex)

  # To build our full regex pattern we need to escape all the relevant characters
  # in the patterns, which means contained open and close will be escaped too,
  # so to match them we need to escape them 2 further times
  open2  <- regex_escape(open1,2)
  close2 <- regex_escape(close1,2)
  patterns_regex <- regex_escape(patterns_regex)
  # matched will be a list containing for each pattern
  # the starting position of matches (in a vector) and the matches length (as attributes)
  matched <- gregexpr(paste0(open2,close2), patterns_regex, perl = T)

  # replace the placeholder with the actual regex
  regmatches(patterns_regex, matched) <- subpat
  # complete the pattern with start and end of string
  patterns_regex <- paste0("^",patterns_regex,"$")

  patterns_regex <- setNames(patterns_regex, patterns)
  if(named_capture) return(patterns_regex)
  if(attributes) {
    attr(patterns_regex, "groups") <-
      lapply(L, function(x) {
        tapply(x$group_indices, x$names, identity)[unique(x$names)]
      })
  }
  patterns_regex
}

#' @rdname unglue
#' @export
unglue_unnest <- function(data, col, patterns, open = "{", close = "}",remove = TRUE, convert = FALSE){
  # save the attributes of our data.frame, except for the names
  attr_bkp <- attributes(data)
  attr_bkp$names <- NULL
  # use unglue_data on the column whose raw name is fed to `var`
  col <- deparse(substitute(col))
  ud  <- unglue_data(data[[col]], patterns, open = open, close = close, convert = convert)
  # remove this column if relevant
  if(remove) data[[col]] <- NULL
  # bind the source and built datasets together
  res <- cbind(data, ud)
  # make unique names in case of duplicated
  names(res) <- make.unique(names(res))
  # restore the attibutes
  attributes(res) <- c(attr_bkp, list(names = names(res)))
  res
}
