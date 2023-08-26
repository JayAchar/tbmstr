custom_outcome <- function(str) {
  factor(str,
    levels = c(
      "No TB", "Reoccurance",
      "Died", "Not evaluated"
    )
  )
}

test_that("reshaping works", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_outcome(c("No TB")),
    evldat3 = c(as.POSIXct("2021-01-01")),
    stat6 = custom_outcome(c("Died")),
    evldat6 = c(as.POSIXct("2021-03-01"))
  )

  expected <- data.frame(
    globalrecordid = c("a", "a"),
    status = custom_outcome(c("No TB", "Died")),
    fudat = as.POSIXct(c("2021-01-01", "2021-03-01"))
  )

  observed <- prepare_eos_outcomes(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("reshaping works with dynamic variable names", {
  input <- data.frame(
    globalrecordid = "a",
    stat9 = custom_outcome(c("No TB")),
    evldat9 = c(as.POSIXct("2021-01-01")),
    stat12 = custom_outcome(c("Died")),
    evldat12 = c(as.POSIXct("2021-03-01"))
  )

  expected <- data.frame(
    globalrecordid = c("a", "a"),
    status = custom_outcome(c("No TB", "Died")),
    fudat = as.POSIXct(c("2021-01-01", "2021-03-01"))
  )

  observed <- prepare_eos_outcomes(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

