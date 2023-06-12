test_that("labelling works", {
  input_df <- data.frame(
    outcome = c(1, 2, 3, 4, 1)
  )
  
  expected <- (
    c("Cured", "Completed", "Failed", "Died", "Cured")
  )
  
  observed <- apply_labels(
    df = input_df,
    variable = "outcome"
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
    variable = "outcome"
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
    variable = "outcome"
  ))
  
  expect_warning(apply_labels(
    df = input_df,
    variable = "outcome"
  ))

  expect_equal(observed, expected)
})

test_that("returns original vector if not found in LUT", {
  
  expected <- c(1, 2, 3, 4, 0)
  
  input_df <- data.frame(
    foo = expected
  )
  
  expect_equal(suppressWarnings(
    apply_labels(input_df, "foo")
  ), expected)
  
  expect_warning(apply_labels(input_df, "foo"))
  
})

