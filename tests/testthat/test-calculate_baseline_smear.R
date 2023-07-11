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


  bdf2 <- data.frame(
    globalrecordid = c("a1", "b2"),
    trtstdat = as.POSIXct(c("2021-01-01", "2021-02-01"))
  )
  mdf2 <- data.frame(
    fkey = c("a1"),
    test_type = c("afb1"),
    result = c(9),
    datespecimen = as.POSIXct("2020-12-27")
  )

  observed <- calculate_baseline_smear(
    baseline = bdf2,
    myco = mdf2
  )


  expect_equal(
    observed,
    data.frame(
      globalrecordid = c("a1", "b2"),
      trtstdat = as.POSIXct(c("2021-01-01", "2021-02-01")),
      smear = factor(c(NA, NA),
        levels = result_levels
      )
    )
  )
})

test_that("new variable is attached to input data frame", {
  bdf <- data.frame(
    globalrecordid = c("a1"),
    trtstdat = as.POSIXct("2021-01-01"),
    cohort = c("historical")
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
    names(observed),
    c(
      "globalrecordid",
      "trtstdat",
      "cohort",
      "smear"
    )
  )
})

test_that("handles multiple specimens on the same day", {
  bdf <- data.frame(
    globalrecordid = c("a1"),
    trtstdat = as.POSIXct("2021-01-01")
  )
  mdf <- data.frame(
    fkey = c("a1", "a1"),
    test_type = c("afb1", "afb1"),
    result = c(1, 1),
    datespecimen = as.POSIXct(c("2020-12-27", "2020-12-27"))
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

  mdf2 <- mdf
  mdf2$result[2] <- 2

  observed <- calculate_baseline_smear(
    baseline = bdf,
    myco = mdf2
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


test_that("handles subjects with no valid results", {
  bdf <- data.frame(
    globalrecordid = c("a1", "b2"),
    trtstdat = as.POSIXct(c("2021-01-01", "2021-01-01"))
  )
  mdf <- data.frame(
    fkey = c("a1", "b2"),
    test_type = c("afb1", "afb1"),
    result = c(1, 2),
    datespecimen = as.POSIXct(c("2021-02-01", "2021-01-02"))
  )

  observed <- calculate_baseline_smear(
    baseline = bdf,
    myco = mdf
  )

  expect_equal(
    observed,
    data.frame(
      globalrecordid = c("a1", "b2"),
      trtstdat = as.POSIXct(c("2021-01-01", "2021-01-01")),
      smear = factor(c(NA, "Negative"),
        levels = result_levels
      )
    )
  )
})
