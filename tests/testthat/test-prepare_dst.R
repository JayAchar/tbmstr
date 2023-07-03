test_that("error correctly", {
  expect_error(
    prepare_dst(data.frame(),
      data.frame(),
      purpose = "foo"
    )
  )
})

test_that("removes post treatment initiation dsts when quality data required", {
  dst <- data.frame(
    fkey = c("1", "2", "1"),
    datedst = c(
      "2021-09-02T23:45:49.000Z",
      "2021-12-09T15:13:51.000Z",
      # the thrid dst row should be removed since it is collected after
      # treatment has been initiated for subject 1
      "2021-12-10T15:13:51.000Z"
    )
  )

  baseline <- data.frame(
    globalrecordid = c(1, 2),
    trtstdat = c(
      "2021-09-02T23:45:49.000Z",
      "2021-12-09T15:13:51.000Z"
    )
  )

  expect_equal(nrow(prepare_dst(
    purpose = "quality",
    dst = dst,
    baseline = baseline
  )), 2)
})
