binary_levels <- c("Successful", "Unsuccessful")
outcome_levels <- c(
  "Cured", "Completed",
  "Failed", "Died", "Lost to follow-up", "Not evaluated",
  "Withdrawn"
)

test_that("uses numeric labels correctly", {
  expect_equal(
    create_binary_tx_outcome(
      c(1, 2, 4, 5)
    ),
    factor(
      c(
        "Successful", "Successful",
        "Unsuccessful", "Unsuccessful"
      ),
      levels = binary_levels
    )
  )
})

test_that("uses text labels correctly", {
  expect_equal(
    create_binary_tx_outcome(
      factor(c("Cured", "Completed", "Died", "Failed"),
        levels = outcome_levels
      )
    ),
    factor(
      c(
        "Successful", "Successful",
        "Unsuccessful", "Unsuccessful"
      ),
      levels = binary_levels
    )
  )
})

test_that("handles missing outcome values", {
  expect_equal(
    create_binary_tx_outcome(
      factor(c("Cured", "Completed", NA, "Failed"),
        levels = outcome_levels
      )
    ),
    factor(
      c(
        "Successful", "Successful",
        NA, "Unsuccessful"
      ),
      levels = binary_levels
    )
  )
})
