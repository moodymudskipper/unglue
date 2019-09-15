# unglue 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Changed names, order, and default values of arguments to be consistent with
`tidyr::extract()` as both functions had similar syntax and functionality and
differences might create confusion.
* A new function `unglue_regex()` can translate unglue patterns
into proper regex.
* A new function `unglue_vec()` extracts a single value from each string.
* A new function `unglue_detect()` returns a logical indicator, indicating which
elements from the input could be matched.
* The elements of the list output of `unglue()` now only contain values extracted.
using the relevant pattern.
* Instead of using `make.unique()` when duplicate subpattern labels are given,
 *unglue* functions now check if identically named subpattern match the same
 content.
* *unglue* functions now have a `multiple` argument, when `multiple` is a 
 function instead of the default `NULL`, groups labelled by the same name are 
 combined.
