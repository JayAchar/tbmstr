test_that("calculates vector correctly", {
  df <- data.frame(
    date = c(days(1), days(5)),
    result = c("No growh", "No growh")
  )

  expected <- c(0, 4)

  expect_equal(
    calculate_result_diffs(
      df,
    ), expected
  )
})

test_that("custom date variable works", {
  df <- data.frame(
    specimendate = c(days(1), days(5)),
    result = c("No growh", "No growh")
  )

  expected <- c(0, 4)

  expect_equal(
    calculate_result_diffs(
      df,
      date_var = "specimendate"
    ), expected
  )
})


test_that("accounts for positive result", {
  df <- data.frame(
    specimendate = c(days(1), days(5), days(20)),
    result = c("No growh", "No growh", "MTB complex")
  )

  expected <- c(0, 4, NA)

  expect_equal(
    calculate_result_diffs(
      df,
      date_var = "specimendate"
    ), expected
  )
})

test_that("handle unordered results", {
  df <- data.frame(
    specimendate = c(days(5), days(1), days(3)),
    result = c("No growh", "No growh", "No growh")
  )

  expect_error(
    calculate_result_diffs(
      df,
      date_var = "specimendate"
    )
  )
})
