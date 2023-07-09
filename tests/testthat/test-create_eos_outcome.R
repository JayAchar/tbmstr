eos_levels <- c(
  "No TB", "Reoccurance", "Died during follow-up",
  "Unsuccessful treatment", "Not evaluated"
)

test_that("errors when incorrect inputs", {
  expect_error(
    create_eos_outcome(
      eot = data.frame(),
      eof = 1
    ), "Input arguments should be numeric vectors or factors."
  )

  expect_error(
    create_eos_outcome(
      eot = c(1, 2),
      eof = c(1)
    ),
    "Input arguments should be the same length."
  )

  expect_error(
    create_eos_outcome(
      eot = 1,
      eof = factor("No TB")
    ),
    "Input arguments should be the same class."
  )
})

test_that("detect success correctly", {
  # end of treatment outcome is success
  # and eos outcome is no TB

  # using values
  expect_equal(create_eos_outcome(
    eot = c(1, 2),
    eof = c(1, 1)
  ), factor(c("No TB", "No TB"),
    levels = eos_levels
  ))

  # using character vector
  expect_equal(create_eos_outcome(
    eot = factor("Cured"),
    eof = factor("No TB")
  ), factor("No TB",
    levels = eos_levels
  ))

  expect_equal(create_eos_outcome(
    eot = factor("Completed"),
    eof = factor("No TB")
  ), factor("No TB",
    levels = eos_levels
  ))
})

test_that("detect on treatment failure correctly", {
  expect_equal(
    create_eos_outcome(
      eot = 3,
      eof = NA_real_
    ),
    factor("Unsuccessful treatment",
      levels = eos_levels
    )
  )

  expect_equal(
    create_eos_outcome(
      eot = factor("Failed"),
      eof = factor("Reoccurance")
    ),
    factor("Unsuccessful treatment",
      levels = eos_levels
    )
  )

  expect_equal(
    create_eos_outcome(
      eot = factor("Failed"),
      eof = factor("No TB")
    ),
    factor("Unsuccessful treatment",
      levels = eos_levels
    )
  )
})

test_that("detect post-treatment failure", {
  expect_equal(
    create_eos_outcome(
      eot = 1,
      eof = 2
    ),
    factor("Reoccurance",
      levels = eos_levels
    )
  )
})

test_that("detect post-treatment death", {
  expect_equal(
    create_eos_outcome(
      eot = 2,
      eof = 3
    ),
    factor("Died during follow-up",
      levels = eos_levels
    )
  )
})

test_that("detect not evaluated", {
  expect_equal(
    create_eos_outcome(
      eot = factor("Not evaluated"),
      eof = factor("No TB")
    ),
    factor("Not evaluated",
      levels = eos_levels
    )
  )
})
