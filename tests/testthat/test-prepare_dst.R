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
      "Thu Sep 02 2021 23:45:49 GMT+0200 (Central European Summer Time)",
      "Thu Dec 09 2021 15:13:51 GMT+0100 (Central European Standard Time)",
      # the thrid dst row should be removed since it is collected after
      # treatment has been initiated for subject 1
      "Fri Dec 10 2021 15:13:51 GMT+0100 (Central European Standard Time)"
    )
  )

  baseline <- data.frame(
    globalrecordid = c(1, 2),
    trtstdat = c(
      "Thu Sep 02 2021 23:45:49 GMT+0200 (Central European Summer Time)",
      "Thu Dec 09 2021 15:13:51 GMT+0100 (Central European Standard Time)"
    )
  )

  expect_equal(nrow(prepare_dst(
    purpose = "quality",
    dst = dst,
    baseline = baseline
  )), 2)
})
