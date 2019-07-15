
<!-- README.md is generated from README.Rmd. Please edit that file -->
unglue
======

The package *unglue* features functions `unglue()` and `unglue_data()` which use a syntax inspired from the functions of Jim Hester's *glue* package to extract matched substrings using a pattern.

It wraps `stringr::str_match_all()` and `stringr::str_replace_all()` (doesn't depend on *glue*) and provides in many cases a more readable alternative to regex. Simple cases indeed don't require regex knowledge at all.

Installation:

``` r
remotes::install_github("moodymudskipper/unglue")
```

Examples
--------

### using an example from `?glue::glue`

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
```

Note that the second pattern uses some regex, regex needs to be typed after an `=` sign, if its has no left hand side then the expression won't be attributed to a variable. in fact the pattern `"{foo}"` is a shorthand for `"{foo=.*?}"`.

`unglue()` is more suitable than `unglue_data()` in pipe chains:

``` r
suppressMessages(library(tidyverse))
facts_df %>%
  mutate(unglued = unglue(facts, patterns)) %>%
  unnest()
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

It's not necessary to escape special characters outside of the curly braces.

``` r
sentences <- c("666 is [a number]", "foo is [a word]", "42 is [the answer]", "Area 51 is [unmatched]")
patterns <- c("{number=\\d+} is [{what}]", "{word=\\D+} is [{what}]")
unglue_data(sentences, patterns)
#>    number       what word
#> 1     666   a number <NA>
#> 2      NA     a word  foo
#> 3      42 the answer <NA>
#> 11     NA       <NA> <NA>
```

Types are converted automatically so in the example the column `number` is numeric.

To switch off the behavior set `convert = FALSE`

``` r
unglue_data(sentences, patterns, convert = FALSE)
#>    number       what word
#> 1     666   a number <NA>
#> 2    <NA>     a word  foo
#> 3      42 the answer <NA>
#> 11   <NA>       <NA> <NA>
```
