test_that("errors correctly", {
  expect_error(
    prepare_ae_summary_data(
      baseline = list(),
      adverse = list(),
      type = "SAE"
    )
  )
})
