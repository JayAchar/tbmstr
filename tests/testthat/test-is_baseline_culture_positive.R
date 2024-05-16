test_that("detects positive baseline culture", {
  input <- data.frame(
    fkey = c("a", "a"),
    datespecimen = days(c(30, 80)),
    trtstdat = days(50),
    result = c(1, 3)
  )

  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = TRUE
  )

  observed <- is_baseline_culture_positive(input)

  expect_equal(observed, expected)
})

test_that("detects positive baseline culture with labels applied", {
  input <- data.frame(
    fkey = c("a", "a"),
    datespecimen = days(c(30, 80)),
    trtstdat = days(50),
    result = c("MTB complex", "No growh")
  )
  
  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = TRUE
  )
  
  observed <- is_baseline_culture_positive(input)
  
  expect_equal(observed, expected)
})

test_that("errors if results are outside expected range", {
  input <- data.frame(
    fkey = c("a", "a"),
    datespecimen = days(c(30, 80)),
    trtstdat = days(50),
    result = c(2, 9)
  )
  
  expect_error(is_baseline_culture_positive(input))
})

test_that("detects any eligible positive culture", {
  input <- data.frame(
    fkey = c("a", "a", "a"),
    datespecimen = days(c(30, 50, 80)),
    trtstdat = days(50),
    result = c(1, 3, 3)
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
    result = c(3, 3)
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
    result = c(3)
  )

  expected <- data.frame(
    globalrecordid = unique(input$fkey),
    is_baseline_culture_positive = FALSE
  )

  observed <- is_baseline_culture_positive(input)

  expect_equal(observed, expected)
})
