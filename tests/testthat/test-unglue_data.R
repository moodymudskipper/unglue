# So we can use data.frame in tests without specifying it each time
options(stringsAsFactors = FALSE)

test_that("unglue_data features all work", {
  # simple case
  expect_identical(
    unglue_data("hello world", "{x} {y}"),
    data.frame(x = "hello", y = "world"))
  # using delimiter in input
  expect_identical(
    unglue_data("{ .* }","{x} {y} {z}"),
    data.frame(x = "{", y = ".*", z = "}"))
  # matching the characters used as delimiters
  expect_identical(
    unglue_data("These are curly braces: {}","These are {what}: {braces=\\{\\}}"),
    data.frame(what = "curly braces", braces = "{}"))
  # doubling the delimiters to escape them in the pattern
  expect_identical(
    unglue_data("'(' is round, '{' is curly", "'(' is {shape1}, '{{' is {shape2}"),
    data.frame(shape1 = "round", shape2 = "curly"))
  # using multiple input and patterns
  expect_identical(
    unglue_data(
      c("a & b", "c | d", "e & f", "g . h"),
      c("{x} & {and}", "{x} | {or}")),
    data.frame(
      x   = c("a", "c", "e", NA) ,
      and = c("b",  NA, "f", NA),
      or  = c( NA, "d",  NA, NA)))
  # converting to relevant type
  expect_identical(
    sapply(unglue_data("1 a", "{x} {y}", convert = TRUE), typeof),
    c(x = "integer", y = "character"))
  expect_identical(
    sapply(unglue_data("1 a", "{x} {y}", convert = FALSE), typeof),
    c(x = "character", y = "character"))
  # using alternate delimiters that don't need escaping
  expect_identical(
    unglue_data("hello world", "<x> <y>", open = "<", ">"),
    data.frame(x = "hello", y = "world"))
  # using alternate delimiters that need escaping
  expect_identical(
    unglue_data("hello world", "(x) (y)", open = "(", ")"),
    data.frame(x = "hello", y = "world"))
  # omitting the lhs works
  expect_identical(
    unglue_data("hello world", "{x} {=.*?}"),
    data.frame(x = "hello"))
})

# test_that("when no match get an empty data frame with correct number of rows", {
#   expect_ncol
# })


string <- c("2 letters: A and B",
            "2 letters: A and A")

test_that("duplicated names are handled properly", {
  # repeated names default behavior
  pattern <- "{n} letters: {letter} and {letter}"
  expect_equal(
    unglue_data(string, pattern, convert = TRUE),
    data.frame(n = c(NA, 2), letter = c(NA, "A")))
  # repeated label works with custom function
  expect_equivalent(
    unglue_data(string, pattern, multiple = paste0, convert = TRUE),
    data.frame(n = c(2, 2), letter = c("AB", "AA")))
  # repeated label works with custom function outputing a vector
  expect_equivalent(
    unglue_data(string, pattern, multiple = c, convert = TRUE),
    structure(list(n = c(2, 2), letter = list(c("A","B"), c("A","A"))),
              class= "data.frame",
              row.names=1:2))
  # repeated label works with custom function outputing a list
  expect_equivalent(
    unglue_data(string, pattern, multiple = list, convert = TRUE),
    structure(list(n = c(2, 2), letter = list(list("A","B"), list("A","A"))),
              class= "data.frame",
              row.names=1:2))

  # with default params you can't assign regex to duplicate names
  pattern <- "{n} letters: {letter} and {letter=\\2}"
  expect_error(unglue_data(string, pattern))
  # but if multiple is a function then you can
  expect_equal(
    unglue_data(string, pattern, multiple = paste0, convert = TRUE),
    data.frame(n = c(NA, 2), letter = c(NA, "AA")))
})
