test_that("labelling works", {
  input_df <- data.frame(
    outcome = c(1, 2, 3, 4, 1)
  )

  expected <- (
    c("Cured", "Completed", "Failed", "Died", "Cured")
  )

  observed <- apply_labels(
    df = input_df,
    variable = "outcome",
    convert_to_factor = FALSE
  )

  expect_equal(observed, expected)
})


test_that("handles missing values", {
  input_df <- data.frame(
    outcome = c(1, 2, 3, 4, NA)
  )

  expected <- (
    c("Cured", "Completed", "Failed", "Died", NA_character_)
  )


  observed <- suppressWarnings(apply_labels(
    df = input_df,
    variable = "outcome",
    convert_to_factor = FALSE
  ))

  expect_equal(observed, expected)
})


test_that("handles introduced missing values", {
  input_df <- data.frame(
    outcome = c(1, 2, 3, 4, 0)
  )

  expected <- (
    c("Cured", "Completed", "Failed", "Died", NA_character_)
  )

  observed <- suppressWarnings(apply_labels(
    df = input_df,
    variable = "outcome",
    convert_to_factor = FALSE
  ))

  expect_warning(apply_labels(
    df = input_df,
    variable = "outcome",
    convert_to_factor = FALSE
  ))

  expect_equal(observed, expected)
})

test_that("returns original vector if not found in LUT", {
  expected <- c(1, 2, 3, 4, 0)

  input_df <- data.frame(
    foo = expected
  )

  expect_equal(suppressMessages(
    apply_labels(input_df, "foo", convert_to_factor = FALSE)
  ), expected)

  expect_message(apply_labels(input_df, "foo", convert_to_factor = FALSE))
})

test_that("converts to factor correctly", {
  input_df <- data.frame(
    outcome = c(1, 2, 3, 4)
  )

  expected <- factor(
    c("Cured", "Completed", "Failed", "Died"),
    levels = c(
      "Cured", "Completed", "Failed", "Died",
      "Lost to follow-up", "Not evaluted",
      "Withdrawn"
    )
  )

  observed <- apply_labels(
    df = input_df,
    variable = "outcome",
    convert_to_factor = TRUE
  )
  expect_equal(observed, expected)
  expect_equal(class(observed), "factor")
})
