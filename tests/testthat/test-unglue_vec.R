test_that("unglue_vec works", {
  sentences <- c("666 is a number", "foo is a word",
                 "42 is the answer", "Area 51 is unmatched")
  patterns <- c("{number=\\d+} is {what}", "{word=\\D+} is {what}")
  expect_equal(unglue_vec(sentences, patterns, 1),
               c("666", "foo", "42", NA))
  expect_equal(unglue_vec(sentences, patterns, 2),
               c("a number", "a word", "the answer", NA))
  expect_equal(unglue_vec(sentences, patterns, "word"),
               c(NA, "foo", NA, NA))
  expect_equal(unglue_vec(sentences, patterns, "number"),
               c("666", NA, "42", NA))
  expect_equal(unglue_vec(sentences, patterns, "number", convert = TRUE),
               c(666, NA, 42, NA))
  expect_equal(unglue_vec(sentences, patterns, "number", convert = type.convert),
               c(666, NA, 42, NA))
  expect_equal(unglue_vec(sentences, patterns, "number", convert = ~type.convert(., as.is = FALSE)),
               c(666, NA, 42, NA))
  expect_error(
      with_mock(requireNamespace = function(...) FALSE,
                unglue_vec(sentences, patterns, "number", convert = ~type.convert(., as.is = FALSE)),
                "rlang"))
  expect_error(unglue_vec(sentences, patterns, c("number","word")))
  expect_equal(unglue_vec(sentences, patterns, 3),
               rep(NA_character_,4))

  debugonce(unglue_vec0)
})
