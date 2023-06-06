test_that("errors with invalid inputs", {
  expect_error(
    remove_invalid_records(
      list()
    )
  )

  expect_error(
    remove_invalid_records(
      data.frame(
        var1 = character(0)
      )
    )
  )
})


test_that("correctly removes rows", {
  input_df <- data.frame(recstatus = c(0, 1))
  expect_equal(
    nrow(suppressMessages(
      remove_invalid_records(input_df)
    )), 1
  )
  expect_true(
    is.data.frame(suppressMessages(
      remove_invalid_records(input_df)
    ))
  )

  expect_message(
    remove_invalid_records(input_df)
  )
})
