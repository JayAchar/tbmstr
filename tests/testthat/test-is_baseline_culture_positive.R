test_that("multiplication works", {
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
