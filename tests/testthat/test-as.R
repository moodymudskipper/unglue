test_that("as.data.frame2 fails on inconsistent lengths", {
  expect_error(as.data.frame2(list(a=1,b=2:3)))
})

