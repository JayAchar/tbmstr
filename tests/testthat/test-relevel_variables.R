test_that("multiplication works", {
  input <- data.frame(
    var1 = factor(c("a", "b"),
      levels = c("a", "b")
    ),
    var2 = factor(c("a", "b"),
      levels = c("a", "b")
    ),
    var3 = factor(c("a", "b"),
      levels = c("a", "b")
    )
  )

  config <- list(
    var1 = "b",
    var2 = "b"
  )

  expected <- data.frame(
    var1 = factor(c("a", "b"),
      levels = c("b", "a")
    ),
    var2 = factor(c("a", "b"),
      levels = c("b", "a")
    ),
    var3 = factor(c("a", "b"),
      levels = c("a", "b")
    )
  )

  observed <- relevel_variables(input, config)

  expect_equal(observed, expected,
    ignore_attr = TRUE
  )
})
