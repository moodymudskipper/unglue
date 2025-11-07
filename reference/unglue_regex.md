# Converts unglue pattern to regular regex pattern

Transforms a vector of patterns given in the unglue format to a vector
of proper regex (PCRE) patterns (so they can for instance be used with
functions from other packages).

## Usage

``` r
unglue_regex(
  patterns,
  open = "{",
  close = "}",
  use_multiple = FALSE,
  named_capture = FALSE,
  attributes = FALSE
)
```

## Arguments

- patterns:

  a character vector or a list of character vectors, if a list, items
  will be pasted using an empty separator (`""`).

- open:

  The opening delimiter.

- close:

  The closing delimiter.

- use_multiple:

  whether we should consider that duplicate labels can match different
  substrings.

- named_capture:

  whether to incorporate the names of the groups in the output regex

- attributes:

  whether to give group attributes to the output

## Value

a character vector.

## Examples

``` r
patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
unglue_regex(patterns)
#> {number=\\d+} is [{what}]   {word=\\D+} is [{what}] 
#> "^(\\d+) is \\[(.*?)\\]$" "^(\\D+) is \\[(.*?)\\]$" 
```
