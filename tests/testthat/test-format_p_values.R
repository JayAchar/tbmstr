test_that("single p-value works", {
  expect_equal(
    format_p_values(c(0.32)),
    "0.32"
  )

  expect_equal(
    format_p_values(c(1)),
    "1.00"
  )

  expect_equal(
    format_p_values(c(0.00001)),
    "<0.0001"
  )

  expect_equal(
    format_p_values(c(0.0002)),
    "0.0002"
  )

  expect_equal(
    format_p_values(c(0.00011)),
    "0.0001"
  )

  expect_equal(
    format_p_values(c(0.00503)),
    "0.0050"
  )
})


test_that("multiple p-values work", {
  expect_equal(
    format_p_values(c(0.32, 1.00, 0.00001)),
    c("0.32", "1.00", "<0.0001")
  )
})

test_that("handles missing values", {
  expect_equal(
    format_p_values(c(0.32, NA_real_, 0.00001)),
    c("0.32", "", "<0.0001")
  )
})
