test_that("unglue works", {
  sentences <- c("666 is a number", "foo is a word",
                 "42 is the answer", "Area 51 is unmatched")
  patterns <- c("{number=\\d+} is {what}", "{word=\\D+} is {what}")
  expect_equivalent(sapply(unglue(sentences, patterns), names),
               list(c("number", "what"),
                    c("word", "what"),
                    c("number", "what"),
                    character(0)))
})
