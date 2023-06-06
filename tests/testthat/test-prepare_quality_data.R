test_that("errors on incorrect arguements", {
  expect_error(
    prepare_quality_data(
      data.frame()
    )
  )

  expect_error(
    prepare_quality_data(
      list(
        foo = character(0)
      )
    )
  )
})


test_that("return list of data frames", {
  input <- list(
    baseline = data.frame(
      globalrecordid = c()
    ),
    dst = data.frame(fkey = c()),
    myco = data.frame(fkey = c())
  )
skip("skip")
  expect_true(
    is.list(prepare_quality_data(input))
  )
})
