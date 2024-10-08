test_that("Aborts if data is not data frame or tibble", {
  testdata <- c(1, 2, 3)
  testlimit <- 5
  expect_error(k_anonymity(testdata, testlimit))
})

test_that("Aborts if limit is not integer", {
  testdata <- data.frame()
  testlimit <- 0.1
  expect_error(k_anonymity(testdata, testlimit))
})

test_that("Aborts if not all supplied column names are in data set", {
  testdata <- data.frame(
    "A" = sample(1:3, 100, replace = TRUE),
    "B" = sample(4:5, 100, replace = TRUE),
    "C" = sample(6:8, 100, replace = TRUE)
  )
  testlimit <- 5
  expect_error(k_anonymity(testdata, testlimit, A, D))
})