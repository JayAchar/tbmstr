create_factor_outcome <- function(lvls) {
  return(
    function(str) {
      factor(str,
        levels = lvls
      )
    }
  )
}

custom_eos_outcome <- create_factor_outcome(internal$definitions$eos_levels)
custom_eot_outcome <- create_factor_outcome(internal$definitions$eot_levels)

test_that("detecting on-treatment failure result works", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Died", "Died")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c("No TB", "Died")),
    fudat = as.POSIXct(c("2021-01-01", "2021-03-01"))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Died"),
    eos_date = as.POSIXct("2020-10-15")
  )

  observed <- calculate_eos_outcome(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})


test_that("detects first follow-up failure", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Cured", "Cured")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c("No TB", "Died")),
    fudat = as.POSIXct(c("2021-01-01", "2021-03-01"))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Died"),
    eos_date = as.POSIXct("2021-03-01")
  )

  observed <- calculate_eos_outcome(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("handle unsorted follow-up episodes", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Cured", "Cured")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c("Reoccurance", "Died")),
    fudat = as.POSIXct(c("2021-03-01", "2021-01-01"))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Died"),
    eos_date = as.POSIXct("2021-01-01")
  )

  observed <- calculate_eos_outcome(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("handle study success outcome", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Cured", "Cured")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c("No TB", "No TB")),
    fudat = as.POSIXct(c("2021-03-01", "2021-01-01"))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("No TB"),
    eos_date = as.POSIXct("2021-03-01")
  )

  observed <- calculate_eos_outcome(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})


test_that("handle on treatment failure", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Failed", "Failed")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c(NA_character_, NA_character_)),
    fudat = as.POSIXct(c(NA_character_, NA_character_))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Treatment failure"),
    eos_date = as.POSIXct("2020-10-15")
  )

  observed <- calculate_eos_outcome(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("warning for withdrawn", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Withdrawn", "Withdrawn")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c(NA_character_, NA_character_)),
    fudat = as.POSIXct(c(NA_character_, NA_character_))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Not evaluated"),
    eos_date = as.POSIXct("2020-10-15")
  )

  expect_warning(calculate_eos_outcome(input))

  observed <- suppressWarnings(calculate_eos_outcome(input))

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("handle missing follow-up episodes", {
  input <- data.frame(
    globalrecordid = c("a", "a"),
    outcome = custom_eot_outcome(c("Cured", "Cured")),
    trtendat = as.POSIXct(c("2020-10-15", "2020-10-15")),
    status = custom_eos_outcome(c(NA_character_, NA_character_)),
    fudat = as.POSIXct(c(NA_character_, NA_character_))
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("No TB"),
    eos_date = as.POSIXct("2020-10-15")
  )

  observed <- calculate_eos_outcome(input)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})
