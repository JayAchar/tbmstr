test_that("date conversion works", {
  input <- c(
    "2021-07-23T00:00:00.000Z",
    NA_character_
  )

  observed <- transform_to_date(input)

  expect_true(class(observed) == "Date")
})

test_that("message if parsing fails", {
  input <- c(
    "Thu Sep 02 2021 23:45:49 GMT+0200 (Central European Summer Time)"
  )

  observed <- suppressMessages(transform_to_date(input))

  expect_message(transform_to_date(input))
})
