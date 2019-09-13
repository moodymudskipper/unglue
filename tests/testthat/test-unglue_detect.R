test_that("unglue_detect works", {
  expect_true(unglue_detect("this and that", "{x} and {y}"))
  expect_false(unglue_detect("this and that", "{x} or {y}"))
})
