test_that("matches grades correctly", {
  values <- c(350, 450, 501)
  grades <- c(450, 481, 501, 1000)
  expected <- c(NA_character_, "I", "III")
  observed <- match_ae_grade(values, grades)

  expect_equal(observed, expected)
})

test_that("NA values are not graded", {
  values <- c(NA_integer_)
  grades <- c(450, 481, 501, 1000)
  expected <- c(NA_character_)
  observed <- match_ae_grade(values, grades)

  expect_equal(observed, expected)
})

test_that("Handle decreasing grades", {
  values <- c(94, 65, 130, 64)
  grades <- c(105, 94, 79, 65)
  expected <- c("II", "III", NA_character_, "IV")
  observed <- match_ae_grade(values, grades)

  expect_equal(observed, expected)
})
