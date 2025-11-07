# unglue

The functions `unglue_data()`, `unglue()`, `unglue_vec()` and
`unglue_unnest()` extract matched substrings using a syntax inspired
from [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).
Simple cases don't require regex knowledge at all.

## Usage

``` r
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

- na:

  string to use when there is no match

- var:

  for `unglue_vec()`, the numeric index or the name of the subpattern to
  extract from

- data:

  a data frame.

- col:

  column containing the character vector to extract values from.

- remove:

  whether to remove the column `col` once extraction is performed

## Value

For `unglue()`a list of one row data frames, for `unglue_data` a data
frame, for `unglue_unnest` the data frame input with additional columns
built from extracted values, for `unglue_vec` an atomic vector.

## Details

Depending on the task you might want:

- `unglue_data()` to return a data frame from a character vector, just
  as
  [`glue::glue_data()`](https://glue.tidyverse.org/reference/glue.html)
  does in reverse

- `unglue()` to return a list of data frames containing the matches

- `unglue_vec()` to extract one value by element of `x`, chosen by index
  or by name.

- `unglue_unnest()` to extract value from a column of a data frame to
  new columns

To build the relevant regex pattern special characters will be escaped
in the input pattern and the subpatterns will be replaced with `(.*?)`
if in standard `"{foo}"` form. An alternate regular expression can be
provided after `=` so that `"{foo=\\d}"` will be translated into
`"(\\d)"`.

Sometimes we might want to use regex to match a part of the text that
won't be extracted, in these cases we just need to omit the name as in
`"{=\\d}"`.

`unglue_unnest()`'s name is a tribute to
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
because `unglue_unnest(data, col, patterns)` returns a similar output as
`dplyr::mutate(data, unglued = unglue(col, patterns)) %>% tidyr::unnest()`
(without requiring any extra package). It is also very close to
[`tidyr::extract()`](https://tidyr.tidyverse.org/reference/extract.html)
and efforts were made to make the syntax consistent with the latter.

## Examples

``` r
# using an awample from ?glue::glue
if(require(magrittr) && require(glue)) {
  glued_data <- mtcars %>% glue_data("{rownames(.)} has {hp} hp")
  unglue_data(glued_data, "{rownames(.)} has {hp} hp")
}
#> Loading required package: magrittr
#> Loading required package: glue
#>            rownames...  hp
#> 1            Mazda RX4 110
#> 2        Mazda RX4 Wag 110
#> 3           Datsun 710  93
#> 4       Hornet 4 Drive 110
#> 5    Hornet Sportabout 175
#> 6              Valiant 105
#> 7           Duster 360 245
#> 8            Merc 240D  62
#> 9             Merc 230  95
#> 10            Merc 280 123
#> 11           Merc 280C 123
#> 12          Merc 450SE 180
#> 13          Merc 450SL 180
#> 14         Merc 450SLC 180
#> 15  Cadillac Fleetwood 205
#> 16 Lincoln Continental 215
#> 17   Chrysler Imperial 230
#> 18            Fiat 128  66
#> 19         Honda Civic  52
#> 20      Toyota Corolla  65
#> 21       Toyota Corona  97
#> 22    Dodge Challenger 150
#> 23         AMC Javelin 150
#> 24          Camaro Z28 245
#> 25    Pontiac Firebird 175
#> 26           Fiat X1-9  66
#> 27       Porsche 914-2  91
#> 28        Lotus Europa 113
#> 29      Ford Pantera L 264
#> 30        Ferrari Dino 175
#> 31       Maserati Bora 335
#> 32          Volvo 142E 109

facts <- c("Antarctica is the largest desert in the world!",
"The largest country in Europe is Russia!",
"The smallest country in Europe is Vatican!",
"Disneyland is the most visited place in Europe! Disneyland is in Paris!",
"The largest island in the world is Green Land!")
facts_df <- data.frame(id = 1:5, facts)

patterns <- c("The {adjective} {place_type} in {bigger_place} is {place}!",
            "{place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*}")
unglue_data(facts, patterns)
#>        place    adjective place_type bigger_place
#> 1 Antarctica      largest     desert    the world
#> 2     Russia      largest    country       Europe
#> 3    Vatican     smallest    country       Europe
#> 4 Disneyland most visited      place       Europe
#> 5 Green Land      largest     island    the world

sentences <- c("666 is [a number]", "foo is [a word]",
              "42 is [the answer]", "Area 51 is [unmatched]")
patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
unglue_data(sentences, patterns)
#>   number       what word
#> 1    666   a number <NA>
#> 2   <NA>     a word  foo
#> 3     42 the answer <NA>
#> 4   <NA>       <NA> <NA>

unglue_unnest(facts_df, facts, patterns)
#>   id
#> 1  1
#> 2  2
#> 3  3
#> 4  4
#> 5  5
unglue_unnest(facts_df, facts, patterns, remove = FALSE)
#>   id                                                                   facts
#> 1  1                          Antarctica is the largest desert in the world!
#> 2  2                                The largest country in Europe is Russia!
#> 3  3                              The smallest country in Europe is Vatican!
#> 4  4 Disneyland is the most visited place in Europe! Disneyland is in Paris!
#> 5  5                          The largest island in the world is Green Land!
```
