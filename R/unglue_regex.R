#' Converts unglue pattern to regular regex pattern
#'
#' Transforms a vector of patterns given in the unglue
#' format to a vector of proper  regex (PCRE) patterns (so they can for instance
#' be used with functions from other packages).
#'
#' @inheritParams  unglue
#' @param use_multiple wether we should consider that duplicate labels can match
#'   different substrings.
#' @param named_capture wether to incorporate the names of the groups in the
#'   ouput regex
#' @param attributes wether to give group attributes to the output
#'
#' @export
#' @return a character vector.
#' @examples
#' patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
#' unglue_regex(patterns)
unglue_regex <- function(
  patterns, open = "{", close = "}", use_multiple = FALSE,
  named_capture = FALSE, attributes = FALSE){
  if(use_multiple && named_capture){
    stop("named_capture can be TRUE only when used with default use_multiple = FALSE")
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
  # we need to order the brackets in the character class to sort out the special
  # case of the `]` bracket
  open_close <- paste(c(open,close)[order(c(open,close) != "]")], collapse="")
  bracket_pattern <- paste0(open1,"(?>[^",open_close,"]|(?R))*", close1)
  # matched will be a list containing for each pattern
  # the starting position of matches (in a vector) and the matches length (as attributes)
  matched <- gregexpr(bracket_pattern, patterns, perl = TRUE)
  # extract from patterns : subpatterns, names and group indices
  L <- parse_brackets(patterns, matched, use_multiple, named_capture = named_capture)
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
