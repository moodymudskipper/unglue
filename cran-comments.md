## Test environments
* Microsoft windows 10 Enterprise, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Responding to Uwe Ligge's comments 2019-09-18

Title field was simplified to 'Extract Matched Substrings Using a Pattern'

Functionality was elaborated in the Description field

## Responding to Martina Schmirl's comments 2019-09-26

* glue package was written as 'glue' (with quotes) in DESCRIPTION file
* \dontrun{} was replaced by if(require(...)){} in examples
* \value was provided in every help file

Additionally the function `unglue_unnest()` was renamed to `unglue_unpack()` +
a few very small changes in doc or examples.

