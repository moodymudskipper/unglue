% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unglue.R
\name{unglue}
\alias{unglue}
\alias{unglue_data}
\alias{unglue_vec}
\alias{unglue_unnest}
\title{unglue}
\usage{
unglue(x, patterns, open = "{", close = "}", convert = FALSE, multiple = NULL)

unglue_data(
  x,
  patterns,
  open = "{",
  close = "}",
  convert = FALSE,
  multiple = NULL,
  na = NA_character_
)

unglue_vec(
  x,
  patterns,
  var = 1,
  open = "{",
  close = "}",
  convert = FALSE,
  multiple = NULL,
  na = NA_character_
)

unglue_unnest(
  data,
  col,
  patterns,
  open = "{",
  close = "}",
  remove = TRUE,
  convert = FALSE,
  multiple = NULL,
  na = NA_character_
)
}
\arguments{
\item{x}{a character vector to unglue.}

\item{patterns}{a character vector or a list of character vectors, if a list,
items will be pasted using an empty separator (\code{""}).}

\item{open}{The opening delimiter.}

\item{close}{The closing delimiter.}

\item{convert}{If \code{TRUE}, will convert columns of output using
\code{utils::type.convert()} with parameter \code{as.is = TRUE}, alternatively, can
be a converting function, such as \code{readr::type_convert}. Formula notation
is supported if the package \code{rlang} is installed, so things like
\code{convert = ~type_convert(., numerals = "warn.loss")} are possible.}

\item{multiple}{The aggregation function to use if several subpatterns are
named the same, by default no function is used and subpatterns named the
same will match the same value. If a function is provided it will be fed
the conflicting values as separate arguments. Formula notation
is supported if the package \code{rlang} is installed.}

\item{na}{string to use when there is no match}

\item{var}{for \code{unglue_vec()}, the numeric index or the name of the subpattern to extract from}

\item{data}{a data frame.}

\item{col}{column containing the character vector to extract values from.}

\item{remove}{whether to remove the column \code{col} once extraction is performed}
}
\value{
For \code{unglue()}a list of one row data frames, for \code{unglue_data} a
data frame, for \code{unglue_unnest} the data frame input with additional columns
built from extracted values, for \code{unglue_vec} an atomic vector.
}
\description{
The functions \code{unglue_data()}, \code{unglue()}, \code{unglue_vec()} and \code{unglue_unnest()}
extract matched substrings using a syntax inspired from \code{glue::glue()}.
Simple cases don't require regex knowledge at all.
}
\details{
Depending on the task you might want:
\itemize{
\item \code{unglue_data()} to return a data frame from a character vector,
just as \code{glue::glue_data()} does in reverse
\item \code{unglue()} to return a list of data frames containing the matches
\item \code{unglue_vec()} to extract one value by element of \code{x}, chosen by index or by
name.
\item \code{unglue_unnest()} to extract value from a column of a data frame to new columns
}

To build the relevant regex pattern special characters will be escaped in the
input pattern and the subpatterns will be replaced with \verb{(.*?)} if in standard
\code{"{foo}"} form. An alternate regular expression can be provided after \code{=} so that
\code{"{foo=\\\\d}"} will be translated into \code{"(\\\\d)"}.

Sometimes we might want to use regex to match a part of the text that won't
be extracted, in these cases we just need to omit the name as in \code{"{=\\\\d}"}.

\code{unglue_unnest()}'s name is a tribute to \code{tidyr::unnest()} because
\code{unglue_unnest(data, col, patterns)} returns a similar output as
\code{dplyr::mutate(data, unglued = unglue(col, patterns)) \%>\% tidyr::unnest()}
(without requiring any extra package).
It is also very close to \code{tidyr::extract()} and efforts were made to make
the syntax consistent with the latter.
}
\examples{
# using an awample from ?glue::glue
if(require(magrittr) && require(glue)) {
  glued_data <- mtcars \%>\% glue_data("{rownames(.)} has {hp} hp")
  unglue_data(glued_data, "{rownames(.)} has {hp} hp")
}

facts <- c("Antarctica is the largest desert in the world!",
"The largest country in Europe is Russia!",
"The smallest country in Europe is Vatican!",
"Disneyland is the most visited place in Europe! Disneyland is in Paris!",
"The largest island in the world is Green Land!")
facts_df <- data.frame(id = 1:5, facts)

patterns <- c("The {adjective} {place_type} in {bigger_place} is {place}!",
            "{place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*}")
unglue_data(facts, patterns)

sentences <- c("666 is [a number]", "foo is [a word]",
              "42 is [the answer]", "Area 51 is [unmatched]")
patterns <- c("{number=\\\\d+} is [{what}]", "{word=\\\\D+} is [{what}]")
unglue_data(sentences, patterns)

unglue_unnest(facts_df, facts, patterns)
unglue_unnest(facts_df, facts, patterns, remove = FALSE)
}
