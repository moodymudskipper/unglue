#tests

# works
test_that("unglue_data features all work", {
  # simple case
  expect_identical(
    unglue_data("hello world", "{x} {y}"),
    data.frame(x = "hello", y = "world",
               stringsAsFactors=FALSE))
  # using delimiter in input
  expect_identical(
    unglue_data("{ .* }","{x} {y} {z}"),
    data.frame(x = "{", y = ".*", z = "}",
               stringsAsFactors=FALSE))
  # matching the characters used as delimiters
  expect_identical(
    unglue_data("These are curly braces: {}","These are {what}: {braces=\\{\\}}"),
    data.frame(what = "curly braces", braces = "{}",
               stringsAsFactors=FALSE))
  # doubling the delimiters to escape them in the pattern
  expect_identical(
    unglue_data("'(' is round, '{' is curly", "'(' is {shape1}, '{{' is {shape2}"),
    data.frame(shape1 = "round", shape2 = "curly",
               stringsAsFactors=FALSE))
  # using multiple input and patterns
  expect_identical(
    unglue_data(
      c("a & b", "c | d", "e & f", "g . h"),
      c("{x} & {and}", "{x} | {or}")),
    data.frame(
      x   = c("a", "c", "e", NA) ,
      and = c("b",  NA, "f", NA),
      or  = c( NA, "d",  NA, NA),
      stringsAsFactors=FALSE))
  # converting to relevant type
  expect_identical(
    sapply(unglue_data("1 a", "{x} {y}"), typeof),
    c(x = "integer", y = "character"))
  expect_identical(
    sapply(unglue_data("1 a", "{x} {y}", convert = FALSE), typeof),
    c(x = "character", y = "character"))
  # using alternate delimiters that don't need escaping
  expect_identical(
    unglue_data("hello world", "<x> <y>", open = "<", ">"),
    data.frame(x = "hello", y = "world",
               stringsAsFactors=FALSE))
  # using alternate delimiters that need escaping
  expect_identical(
    unglue_data("hello world", "(x) (y)", open = "(", ")"),
    data.frame(x = "hello", y = "world",
               stringsAsFactors=FALSE))
})
