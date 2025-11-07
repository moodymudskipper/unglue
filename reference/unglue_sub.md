# unglue_sub

substitute substrings using strings or replacement functions

## Usage

``` r
unglue_sub(x, patterns, repl, open = "{", close = "}")
```

## Arguments

- x:

  character vector

- patterns:

  a character vector or a list of character vectors, if a list, items
  will be pasted using an empty separator (`""`).

- repl:

  function to apply on matched substrings, formula (if package rlang is
  installed), substring, or named list of such.

- open:

  The opening delimiter.

- close:

  The closing delimiter.

## Examples

``` r
unglue_sub(
  c("a and b", "foo or bar"),
  c("{x} and {y}", "{x} or {z}"),
  "XXX")
#> [1] "XXX and XXX" "XXX or XXX" 

unglue_sub(
  c("a and b", "foo or bar"),
  c("{x} and {y}", "{x} or {z}"),
  toupper)
#> [1] "A and B"    "FOO or BAR"

unglue_sub(
  c("a and b", "foo or BAR"),
  c("{x} and {y}", "{x} or {z}"),
  list(x= "XXX", y = toupper, z = tolower))
#> [1] "XXX and B"  "XXX or bar"
```
