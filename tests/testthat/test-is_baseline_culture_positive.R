test_that("detects positive baseline culture", {
  input <- data.frame(
    fkey = c("a", "a"),
    datespecimen = days(c(30, 80)),
    trtstdat = days(50),
    result = c(1, 0)
  )

  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = TRUE
  )

  observed <- is_baseline_culture_positive(input)

  expect_equal(observed, expected)
})

test_that("detects any eligible positive culture", {
  input <- data.frame(
    fkey = c("a", "a", "a"),
    datespecimen = days(c(30, 50, 80)),
    trtstdat = days(50),
    result = c(1, 0, 0)
  )

  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = TRUE
  )

  observed <- is_baseline_culture_positive(input)

  expect_equal(observed, expected)
})


test_that("handles no eligible positive cultures", {
  input <- data.frame(
    fkey = c("a", "a"),
    datespecimen = days(c(30, 50)),
    trtstdat = days(50),
    result = c(0, 0)
  )

  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = FALSE
  )

  observed <- is_baseline_culture_positive(input)

  expect_equal(observed, expected)
})

test_that("handles no eligible cultures", {
  input <- data.frame(
    fkey = c("a"),
    datespecimen = days(c(60)),
    trtstdat = days(50),
    result = c(0)
  )

  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = FALSE
  )

  observed <- is_baseline_culture_positive(input)

  expect_equal(observed, expected)
})
