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

test_that("unglue_sub works", {
  expect_equal(
    unglue_sub(
      c("a and b", "foo or BAR"),
      c("{x} and {y}", "{x} or {z}"),
      list(x= "XXX", y = ~toupper(.), z = tolower)),
    c("XXX and B",  "XXX or bar"))
})
