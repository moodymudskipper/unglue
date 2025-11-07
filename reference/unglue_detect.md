# Detect if strings are matched by a set of unglue patterns

Returns a logical indicating whether input strings were matched by one
or more patterns

## Usage

``` r
unglue_detect(
  x,
  patterns,
  open = "{",
  close = "}",
  convert = FALSE,
  multiple = NULL
)
```

## Arguments

- x:

  a character vector to unglue.

- patterns:

  a character vector or a list of character vectors, if a list, items
  will be pasted using an empty separator (`""`).

- open:

  The opening delimiter.

- close:

  The closing delimiter.

- convert:

  If `TRUE`, will convert columns of output using
  [`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html)
  with parameter `as.is = TRUE`, alternatively, can be a converting
  function, such as
  [`readr::type_convert`](https://readr.tidyverse.org/reference/type_convert.html).
  Formula notation is supported if the package `rlang` is installed, so
  things like `convert = ~type_convert(., numerals = "warn.loss")` are
  possible.

- multiple:

  The aggregation function to use if several subpatterns are named the
  same, by default no function is used and subpatterns named the same
  will match the same value. If a function is provided it will be fed
  the conflicting values as separate arguments. Formula notation is
  supported if the package `rlang` is installed.

## Value

a vector of logical.

## Examples

``` r
sentences <- c("666 is [a number]", "foo is [a word]",
               "42 is [the answer]", "Area 51 is [unmatched]")
patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
unglue_detect(sentences, patterns)
#> [1]  TRUE  TRUE  TRUE FALSE
```
