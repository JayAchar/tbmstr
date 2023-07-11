test_that("errors with incorrect input", {
  expect_error(
    create_cc_days(
      trtstdat = 1,
      convdat = 5
    ), "Inputs should be of type `POSIXct`."
  )

  expect_error(
    create_cc_days(
      trtstdat = as.POSIXct(c("2021-01-01", "2021-02-01")),
      convdat = as.POSIXct("2021-01-0333")
    ), "Input arguments should be the same length."
  )
})

test_that("calculates days to culture conversion accurately", {
  expect_equal(
    create_cc_days(
      trtstdat = as.POSIXct("2021-01-01"),
      convdat = as.POSIXct("2021-01-03")
    ), 2
  )

  expect_equal(
    create_cc_days(
      trtstdat = as.POSIXct("2021-01-01"),
      convdat = as.POSIXct(NA_character_)
    ), NA_real_
  )

  expect_equal(
    create_cc_days(
      trtstdat = as.POSIXct(c("2021-01-01", "2021-02-01")),
      convdat = as.POSIXct(c("2021-01-03", "2021-02-05"))
    ), c(2, 4)
  )
})
