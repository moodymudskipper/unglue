
# escape all characters that need escaping
regex_escape <- function(string,n = 1) {
  for(i in seq_len(n)){
    string <- gsub("([][{}()+*^$|\\\\?])", "\\\\\\1", string)
  }
  string
}

pattern_match <- function(x,patterns, type = "chr"){
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
#' `unglue()` wraps `stringr::str_match_all()` and `stringr::str_replace_all()`
#' to extract matched substrings using a syntax inspired from `glue::glue()`.
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
#' }
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
  # collapse patterns
  patterns <- lapply(patterns, paste, collapse = "")
  # escape variable delimiters
  open <- regex_escape(open)
  close <- regex_escape(close)
  # extract bracket content
  bracket_content <- stringr::str_match_all(patterns, paste0(open,"(.*?)",close))
  bracket_content <- lapply(bracket_content, function(x) x[,2])
  # extract variable names
  nms <- lapply(bracket_content, function(x){
    eq <- grepl("=", x)
    x[eq] <- gsub("^(.*?)\\=.*","\\1", x[eq])
    x[x!=""]
  })

  # escape delimiters another time
  open  <- regex_escape(open)
  close <- regex_escape(close)
  bracket_pattern <- paste0(open, "(.*?)",close)
  bracket_to_subppatern <-  function(x) {
    # unescape
    x <- gsub("\\\\([][{}()+*^$|\\\\?])", "\\1", x)
    if(!grepl("=", x)) "(.*?)" else
      # need to be adapted to other brackets
      if(grepl("\\{=", x)) gsub("\\{.*?=(.*?)\\}", "\\1", x) else
        paste0("(", gsub("\\{.*?=(.*?)\\}", "\\1", x), ")")
  }

  patterns_regex <- stringr::str_replace_all(
    regex_escape(patterns), bracket_pattern, bracket_to_subppatern)

  # complete the pattern with start and end of string
  patterns_regex <- paste0("^",patterns_regex,"$")
  # assign a pattern to each element
  pattern_indices <- pattern_match(x, patterns_regex , "int")
  # initiate a list of results
  res <- as.list(rep.int(NA,length(x)))
  # assign a tibble of matches to each result from each pattern
  for(i in seq_along(patterns)){
    subset_lgl <- pattern_indices == i
    subset_lgl <- subset_lgl[!is.na(subset_lgl)]
    res_i <- stringr::str_match_all(x[subset_lgl], patterns_regex[i])
    res[subset_lgl] <- lapply(res_i, function(x) as.data.frame(`colnames<-`(x[,-1, drop = FALSE],nms[[i]]),stringsAsFactors = FALSE))
  }
  # replace NA elements by an empty single row tibble
  na_indices <- is.na(pattern_indices)
  res[na_indices] <- replicate(sum(na_indices), data.frame(row.names = 1))
  # bind everything
  res <- bind_rows2(res)
  # convert if relevant
  if (convert) res <- utils::type.convert(res)
  res
}

# simulate bind_rows to spare a dependency
# bind_rows2(list(mtcars[1:2, 1:3], mtcars[1:2, 2:4]))
bind_rows2 <- function(x){
  nms <- unique(unlist(sapply(x, names)))
  x[] <- lapply(x,function(x) {x[nms[!nms %in% names(x)]] <- NA; x})
  do.call(rbind,x)
}
