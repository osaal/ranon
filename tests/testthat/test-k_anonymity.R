test_that("Aborts if data is not data frame or tibble", {
  testdata <- c(1, 2, 3)
  testlimit <- 5
  expect_error(k_anonymity(testdata, 5))
})
