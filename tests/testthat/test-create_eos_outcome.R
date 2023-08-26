test_that("errors when incorrect inputs", {
  skip()
  expect_error(
    create_eos_outcome(
      df = data.frame()
    ), "Input data frame does not include required variables."
  )

  expect_error(
    create_eos_outcome(
      df = list()
    ), "Input argument should be a data frame."
  )
})

test_that("complete follow-up success", {
  skip()
  # end of treatment outcome is success
  # and eos outcome is no TB
  skip()
  input <- data.frame(
    id = c(1),
    outcome = c("Cured"),
    stat3 = c("No TB"),
    stat6 = c("No TB"),
    stat9 = c("No TB"),
    stat12 = c("No TB"),
  )
  # using values
  expect_equal(create_eos_outcome(
    df = input
  ), factor(c("No TB"),
    levels = internal$definitions$eos_levels
  ))
})

test_that("partial follow-up success", {
  skip()
  input <- data.frame(
    id = c(1),
    outcome = c("Completed"),
    stat3 = c("No TB"),
    stat6 = c("No TB"),
    stat9 = c(NA_character_),
    stat12 = c(NA_character_),
  )
  # using values
  expect_equal(create_eos_outcome(
    df = input
  ), factor(c("No TB"),
    levels = internal$definitions$eos_levels
  ))
})

test_that("detect on treatment failure correctly", {
  skip()

  input <- data.frame(
    id = c(1),
    outcome = c("Failed"),
    stat3 = c("No TB"),
    stat6 = c("No TB"),
    stat9 = c("Reoccrance"),
    stat12 = c(NA_character_),
  )
  expect_equal(
    create_eos_outcome(
      df = input
    ),
    factor("Unsuccessful treatment",
      levels = internal$definitions$eos_levels
    )
  )

  expect_equal(
    create_eos_outcome(
      eot = factor("Failed"),
      eof = factor("Reoccurance")
    ),
    factor("Unsuccessful treatment",
      levels = internal$definitions$eos_levels
    )
  )
})

test_that("detect post-treatment failure", {
  skip()
  input <- data.frame(
    id = c(1),
    outcome = c("Completed"),
    stat3 = c("No TB"),
    stat6 = c("No TB"),
    stat9 = c("Reoccurance"),
    stat12 = c(NA_character_),
  )
  expect_equal(
    create_eos_outcome(
      df = input
    ),
    factor("Reoccurance",
      levels = internal$definitions$eos_levels
    )
  )
})
