test_that("works with success", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome("No TB"),
    stat6 = custom_eos_outcome("No TB"),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("No TB"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("works with death", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome("No TB"),
    stat6 = custom_eos_outcome("Died"),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01"),
    deathdat = as.POSIXct("2021-04-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("Died"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("Ignores not evaluated", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome("No TB"),
    stat6 = custom_eos_outcome("No TB"),
    stat9 = custom_eos_outcome("Not evaluated"),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01"),
    evldat9 = as.POSIXct("2021-07-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("No TB"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6, 9)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("Ignores missing status", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome("No TB"),
    stat6 = custom_eos_outcome("No TB"),
    stat9 = custom_eos_outcome(NA_character_),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01"),
    evldat9 = as.POSIXct("2021-07-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("No TB"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6, 9)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )

  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome(NA_character_),
    stat6 = custom_eos_outcome("No TB"),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("No TB"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("Ignores follow-up after death", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome("No TB"),
    stat6 = custom_eos_outcome("Died"),
    stat9 = custom_eos_outcome("No TB"),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01"),
    evldat9 = as.POSIXct("2021-07-01"),
    deathdat = as.POSIXct("2021-04-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("Died"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6, 9)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("use date of death", {
  input <- data.frame(
    globalrecordid = "a",
    deathdat = as.POSIXct("2021-03-20"),
    stat3 = custom_eos_outcome("No TB"),
    stat6 = custom_eos_outcome("Died"),
    stat9 = custom_eos_outcome("Died"),
    evldat3 = as.POSIXct("2021-01-01"),
    evldat6 = as.POSIXct("2021-04-01"),
    evldat9 = as.POSIXct("2021-07-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("Died"),
      final_fu_date = as.POSIXct("2021-03-20")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6, 9)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})


test_that("Handles participants with no post-treatment follow-up", {
  input <- data.frame(
    globalrecordid = "a",
    trtendat = as.POSIXct("2021-01-01"),
    outcome = custom_eot_outcome("Completed"),
    stat3 = custom_eos_outcome(NA_character_),
    evldat3 = as.POSIXct("2021-04-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("No TB"),
      final_fu_date = as.POSIXct("2021-01-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})

test_that("Handles incorrectly ordered eval dates", {
  input <- data.frame(
    globalrecordid = "a",
    stat3 = custom_eos_outcome("Died"),
    stat6 = custom_eos_outcome("No TB"),
    evldat3 = as.POSIXct("2021-04-01"),
    evldat6 = as.POSIXct("2021-01-01"),
    deathdat = as.POSIXct("2021-04-01")
  )

  expected <- cbind(
    input,
    data.frame(
      final_fu_status = custom_eos_outcome("Died"),
      final_fu_date = as.POSIXct("2021-04-01")
    )
  )

  observed <- calculate_final_follow_up(
    input,
    eval_months = c(3, 6)
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})
