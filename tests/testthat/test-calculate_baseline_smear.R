result_levels <- c("Positive", "Negative", "No result")
test_that("calculates baseline smear correctly", {
  bdf <- data.frame(
    globalrecordid = c("a1"),
    trtstdat = as.POSIXct("2021-01-01")
  )
  mdf <- data.frame(
    fkey = c("a1"),
    test_type = c("afb1"),
    result = c(1),
    datespecimen = as.POSIXct("2020-12-27")
  )

  observed <- calculate_baseline_smear(
    baseline = bdf,
    myco = mdf
  )

  expect_equal(
    observed,
    data.frame(
      globalrecordid = c("a1"),
      trtstdat = as.POSIXct("2021-01-01"),
      smear = factor("Positive",
        levels = result_levels
      )
    )
  )
})

test_that("handles lack of smear result", {
  skip()
  bdf <- data.frame(
    globalrecordid = c("a1"),
    trtstdat = as.POSIXct("2021-01-01")
  )
  mdf <- data.frame(
    fkey = c("a1"),
    test_type = c("afb1"),
    result = c(9),
    datespecimen = as.POSIXct("2020-12-27")
  )

  observed <- calculate_baseline_smear(
    baseline = bdf,
    myco = mdf
  )

  expect_equal(
    observed,
    data.frame(
      globalrecordid = c("a1"),
      trtstdat = as.POSIXct("2021-01-01"),
      smear = factor(NA,
        levels = result_levels
      )
    )
  )
})
