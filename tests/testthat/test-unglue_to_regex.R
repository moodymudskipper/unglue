test_that("unglue_to_regex works", {
  sentences <- c("666 is a number", "foo is a word",
                 "42 is the answer", "Area 51 is unmatched")
  patterns <- c("{number=\\d+} is {what}", "{word=\\D+} is {what}")
  expect_error(
    unglue_to_regex("{x} and {y}", multiple = paste0, named_capture = TRUE))
  expect_error(
    unglue_to_regex("{x} and {y}", open = "|", close = "|"))
  expect_error(
    unglue_to_regex("{x} and {y}", open = "<<"))
  expect_identical(
    unglue_to_regex("{x} and {y}", named_capture = TRUE),
    c(`{x} and {y}` = "^(?<x>.*?) and (?<y>.*?)$"))
})
