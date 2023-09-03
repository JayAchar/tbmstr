test_that("errors when incorrect inputs", {
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
