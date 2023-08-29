test_that("basic adjustment works", {
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      var1 = c("a", "b")
    )
  )

  adjustments <- list(
    list(
      name = "baseline",
      id = "globalrecordid",
      adjustments = list(
        list(
          id = "1",
          var = "var1",
          value = "z"
        )
      )
    )
  )

  expected <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      var1 = c("z", "b")
    )
  )

  observed <- apply_manual_adjustments(
    input, adjustments
  )

  expect_equal(observed,
    expected,
    ignore_attr = TRUE
  )
})


test_that("multiple changes on the same data frame works", {
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      var1 = c("a", "b")
    )
  )

  adjustments <- list(
    list(
      name = "baseline",
      id = "globalrecordid",
      adjustments = list(
        list(
          id = "1",
          var = "var1",
          value = "z"
        ),
        list(
          id = "2",
          var = "var1",
          value = "x"
        )
      )
    )
  )

  expected <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      var1 = c("z", "x")
    )
  )

  observed <- apply_manual_adjustments(
    input, adjustments
  )

  expect_equal(observed,
    expected,
    ignore_attr = TRUE
  )
})


test_that("changes on multiple data frames", {
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      var1 = c("a", "b")
    ),
    adverse = data.frame(
      fkey = c("5", "6"),
      value = c("yes", "no")
    )
  )

  adjustments <- list(
    list(
      name = "baseline",
      id = "globalrecordid",
      adjustments = list(
        list(
          id = "1",
          var = "var1",
          value = "z"
        )
      )
    ),
    list(
      name = "adverse",
      id = "fkey",
      adjustments = list(
        list(
          id = "6",
          var = "value",
          value = "yes"
        )
      )
    )
  )

  expected <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      var1 = c("z", "b")
    ),
    adverse = data.frame(
      fkey = c("5", "6"),
      value = c("yes", "yes")
    )
  )

  observed <- apply_manual_adjustments(
    input, adjustments
  )

  expect_equal(observed,
    expected,
    ignore_attr = TRUE
  )
})
