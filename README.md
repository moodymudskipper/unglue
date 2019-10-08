
<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/moodymudskipper/unglue.svg?branch=master)](https://travis-ci.org/moodymudskipper/unglue)
[![Coverage
Status](https://img.shields.io/codecov/c/github/moodymudskipper/unglue/master.svg)](https://codecov.io/github/tidyverse/glue?branch=master)

# unglue

The package *unglue* features functions such as `unglue()`,
`unglue_data()` and `unglue_unnest()` which provide in many cases a more
readable alternative to base regex functions. Simple cases indeed don’t
require regex knowledge at all.

It uses a syntax inspired from the functions of Jim Hester’s *glue*
package to extract matched substrings using a pattern, but is not
endorsed by the authors of *glue* nor *tidyverse* packages.

It is completely dependency free, though formula notation of functions
is supported if *rlang* is installed.

## Installation:

``` r
remotes::install_github("moodymudskipper/unglue")
```

### using an example from `?glue::glue` backwards

``` r
library(unglue)
library(glue)
library(magrittr)
glued_data <- head(mtcars) %>% glue_data("{rownames(.)} has {hp} hp")
glued_data
#> Mazda RX4 has 110 hp
#> Mazda RX4 Wag has 110 hp
#> Datsun 710 has 93 hp
#> Hornet 4 Drive has 110 hp
#> Hornet Sportabout has 175 hp
#> Valiant has 105 hp
unglue_data(glued_data, "{rownames(.)} has {hp} hp")
#>         rownames...  hp
#> 1         Mazda RX4 110
#> 2     Mazda RX4 Wag 110
#> 3        Datsun 710  93
#> 4    Hornet 4 Drive 110
#> 5 Hornet Sportabout 175
#> 6           Valiant 105
```

### use several patterns, the first that matches will be used

``` r
facts <- c("Antarctica is the largest desert in the world!",
"The largest country in Europe is Russia!",
"The smallest country in Europe is Vatican!",
"Disneyland is the most visited place in Europe! Disneyland is in Paris!",
"The largest island in the world is Green Land!")

patterns <- c("The {adjective} {place_type} in {bigger_place} is {place}!",
            "{place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*}")
unglue_data(facts, patterns)
#>        place    adjective place_type bigger_place
#> 1 Antarctica      largest     desert    the world
#> 2     Russia      largest    country       Europe
#> 3    Vatican     smallest    country       Europe
#> 4 Disneyland most visited      place       Europe
#> 5 Green Land      largest     island    the world
```

Note that the second pattern uses some regex, regex needs to be typed
after an `=` sign, if its has no left hand side then the expression
won’t be attributed to a variable. in fact the pattern `"{foo}"` is a
shorthand for `"{foo=.*?}"`.

### escaping characters

Special characters outside of the curly braces should not be escaped.

``` r
sentences <- c("666 is [a number]", "foo is [a word]", "42 is [the answer]", "Area 51 is [unmatched]")
patterns2 <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
unglue_data(sentences, patterns2)
#>   number       what word
#> 1    666   a number <NA>
#> 2   <NA>     a word  foo
#> 3     42 the answer <NA>
#> 4   <NA>       <NA> <NA>
```

### type conversion

In order to convert types automatically we can set `convert = TRUE`, in
the example above the column `number` will be converted to numeric.

``` r
unglue_data(sentences, patterns2, convert = TRUE)
#>   number       what word
#> 1    666   a number <NA>
#> 2     NA     a word  foo
#> 3     42 the answer <NA>
#> 4     NA       <NA> <NA>
```

`convert = TRUE` triggers the use of `utils::type.convert` with
parameter `as.is = TRUE`. We can also set `convert` to another
conversion function such as `readr::type_convert`, or to a formula is
*rlang* is installed.

### `unglue_unnest()`

`unglue_unnest()` is named as a tribute to `tidyr::unnest()` as it’s
equivalent to using sucessively `unglue()` and `unnest()` on a data
frame column. It is similar to `tidyr::extract()` in its syntax and
efforts were made to make it as consistent as possible.

``` r
facts_df <- data.frame(id = 1:5, facts)
unglue_unnest(facts_df, facts, patterns)
#>   id      place    adjective place_type bigger_place
#> 1  1 Antarctica      largest     desert    the world
#> 2  2     Russia      largest    country       Europe
#> 3  3    Vatican     smallest    country       Europe
#> 4  4 Disneyland most visited      place       Europe
#> 5  5 Green Land      largest     island    the world
unglue_unnest(facts_df, facts, patterns, remove = FALSE)
#>   id
#> 1  1
#> 2  2
#> 3  3
#> 4  4
#> 5  5
#>                                                                     facts
#> 1                          Antarctica is the largest desert in the world!
#> 2                                The largest country in Europe is Russia!
#> 3                              The smallest country in Europe is Vatican!
#> 4 Disneyland is the most visited place in Europe! Disneyland is in Paris!
#> 5                          The largest island in the world is Green Land!
#>        place    adjective place_type bigger_place
#> 1 Antarctica      largest     desert    the world
#> 2     Russia      largest    country       Europe
#> 3    Vatican     smallest    country       Europe
#> 4 Disneyland most visited      place       Europe
#> 5 Green Land      largest     island    the world
```

### `unglue_vec()`

While `unglue()` returns a list of data frames, `unglue_vec()` returns a
character vector (unless `convert = TRUE`), if several matches are found
in a string the extracted match will be chosen by name or by position.

``` r
unglue_vec(sentences, patterns2, "number")
#> [1] "666" NA    "42"  NA
unglue_vec(sentences, patterns2, 1)
#> [1] "666" "foo" "42"  NA
```

### `unglue_detect()`

`unglue_detect()` returns a logical vector, it’s convenient to check
that the input was matched by a pattern, or to subset the input to take
a look at unmatched elements.

``` r
unglue_detect(sentences, patterns2)
#> [1]  TRUE  TRUE  TRUE FALSE
subset(sentences, !unglue_detect(sentences, patterns2))
#> [1] "Area 51 is [unmatched]"
```

### `unglue_regex()`

`unglue_regex()` returns a character vector of regex patterns, all over
functions are wrapped around it and it can be used to leverage the
*unglue* syntax in other functions.

``` r
unglue_regex(patterns)
#>            The {adjective} {place_type} in {bigger_place} is {place}! 
#>                                "^The (.*?) (.*?) in (.*?) is (.*?)!$" 
#> {place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*} 
#>                            "^(.*?) is the (.*?) ([^ ]+) in (.*?)!.*$"
unglue_regex(patterns, named_capture = TRUE)
#>                                 The {adjective} {place_type} in {bigger_place} is {place}! 
#>     "^The (?<adjective>.*?) (?<place_type>.*?) in (?<bigger_place>.*?) is (?<place>.*?)!$" 
#>                      {place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*} 
#> "^(?<place>.*?) is the (?<adjective>.*?) (?<place_type>[^ ]+) in (?<bigger_place>.*?)!.*$"
unglue_regex(patterns, attributes = TRUE)
#>            The {adjective} {place_type} in {bigger_place} is {place}! 
#>                                "^The (.*?) (.*?) in (.*?) is (.*?)!$" 
#> {place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*} 
#>                            "^(.*?) is the (.*?) ([^ ]+) in (.*?)!.*$" 
#> attr(,"groups")
#> attr(,"groups")$`The {adjective} {place_type} in {bigger_place} is {place}!`
#>    adjective   place_type bigger_place        place 
#>            1            2            3            4 
#> 
#> attr(,"groups")$`{place} is the {adjective} {place_type=[^ ]+} in {bigger_place}!{=.*}`
#>        place    adjective   place_type bigger_place 
#>            1            2            3            4
```

### duplicated labels

We can ensure that a pattern is repeated by repeating its label

``` r
unglue_data(c("black is black","black is dark"), "{color} is {color}")
#>   color
#> 1 black
#> 2  <NA>
```

We can change this behavior by feeding a function to the `multiple`
parameter, in that case this function will be applies on the matches.

``` r
unglue_data(c("black is black","black is dark"), "{color} is {color}", multiple = paste)
#>         color
#> 1 black black
#> 2  black dark
```
