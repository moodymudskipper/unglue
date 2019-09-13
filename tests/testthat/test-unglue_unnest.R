test_that("unglue_unnest works", {
  df <- data.frame(sentence=c("666 is a number", "foo is a word",
                 "42 is the answer", "Area 51 is unmatched"), stringsAsFactors = FALSE)
  patterns <- c("{number=\\d+} is {what}", "{word=\\D+} is {what}")
  expect_equal(unglue_unnest(df, sentence, patterns, remove = FALSE),
               transform(df,
                         number = c("666", NA, "42", NA),
                         what = c("a number", "a word", "the answer", NA),
                         word = c(NA, "foo", NA, NA)))
  expect_equal(unglue_unnest(df, sentence, patterns, remove = TRUE),
               data.frame(stringsAsFactors = FALSE,
                         number = c("666", NA, "42", NA),
                         what = c("a number", "a word", "the answer", NA),
                         word = c(NA, "foo", NA, NA)))
})
