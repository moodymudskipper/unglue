# Changelog

## unglue 0.0.1

CRAN release: 2019-10-07

- Added a `NEWS.md` file to track changes to the package.
- Changed names, order, and default values of arguments to be consistent
  with
  [`tidyr::extract()`](https://tidyr.tidyverse.org/reference/extract.html)
  as both functions had similar syntax and functionality and differences
  might create confusion.
- A new function [`unglue_regex()`](../reference/unglue_regex.md) can
  translate unglue patterns into proper regex.
- A new function [`unglue_vec()`](../reference/unglue.md) extracts a
  single value from each string.
- A new function [`unglue_detect()`](../reference/unglue_detect.md)
  returns a logical indicator, indicating which elements from the input
  could be matched.
- The elements of the list output of
  [`unglue()`](../reference/unglue.md) now only contain values
  extracted. using the relevant pattern.
- Instead of using
  [`make.unique()`](https://rdrr.io/r/base/make.unique.html) when
  duplicate subpattern labels are given, *unglue* functions now check if
  identically named subpattern match the same content.
- *unglue* functions now have a `multiple` argument, when `multiple` is
  a function instead of the default `NULL`, groups labelled by the same
  name are combined.

## unglue 0.1

Bug fixes:

- Choosing `[` and `]` as opening and closing brackets now works as
  expected

Features :

- Support of `multiple` argument for all relevant functions was fixed
- An `na` argument can be used to give a different value than `NA` to
  unmatched substrings in [`unglue_data()`](../reference/unglue.md),
  [`unglue_unnest()`](../reference/unglue.md) and
  [`unglue_vec()`](../reference/unglue.md)
- A new function [`unglue_sub()`](../reference/unglue_sub.md) allows
  convenient substring substitution
