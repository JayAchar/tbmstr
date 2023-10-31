test_that("detecting on-treatment failure result works", {
  input <- list(
    df = data.frame(
      globalrecordid = "a",
      outcome = custom_eot_outcome("Died"),
      trtendat = as.POSIXct("2020-10-15")
    ),
    fu = data.frame(
      globalrecordid = "a"
    )
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Died"),
    eos_date = as.POSIXct("2020-10-15")
  )

  observed <- calculate_eos_outcome(
    input$df,
    input$fu
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})


test_that("detects first follow-up failure", {
  input <- list(
    df = data.frame(
      globalrecordid = "a",
      outcome = custom_eot_outcome("Cured"),
      trtendat = as.POSIXct("2020-10-15"),
      deathfu = FALSE,
      deathdat = NA
    ),
    fu = data.frame(
      globalrecordid = "a",
      final_fu_status = "Died",
      final_fu_date = as.POSIXct("2021-03-01")
    )
  )
  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("Died"),
    eos_date = as.POSIXct("2021-03-01")
  )

  observed <- calculate_eos_outcome(input$df, input$fu)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})


test_that("handle study success outcome", {
  input <- list(
    df = data.frame(
      globalrecordid = "a",
      outcome = custom_eot_outcome("Cured"),
      trtendat = as.POSIXct("2020-10-15"),
      deathfu = FALSE,
      deathdat = NA
    ),
    fu = data.frame(
      globalrecordid = "a",
      final_fu_status = "No TB",
      final_fu_date = as.POSIXct("2021-10-12")
    )
  )

  expected <- data.frame(
    globalrecordid = c("a"),
    eos_outcome = custom_eos_outcome("No TB"),
    eos_date = as.POSIXct("2021-10-12")
  )

  observed <- calculate_eos_outcome(
    input$df,
    input$fu
  )

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})
