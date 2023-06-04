test_that("errors with incorrect input", {
  expect_error(
    run_tutorial(c("tutorial_one", "tutorial_two"))
  )

  expect_error(
    run_tutorial("foo")
  )
})
