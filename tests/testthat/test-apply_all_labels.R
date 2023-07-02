test_that("labelling works", {
  input <- list(
    baseline = data.frame(
      prison = c(1, 2, 9, NA_integer_),
      age = c(28, NA, 56, 24)
    ),
    adverse = data.frame(
      severity = c(1, 2, 9)
    )
  )

  expected <- list(
    baseline = data.frame(
      prison = factor(c("Yes", "No", "Unknown", NA_integer_),
        levels = c("Yes", "No", "Unknown")
      ),
      age = c(28, NA, 56, 24)
    ),
    adverse = data.frame(
      severity = factor(c("Grade I", "Grade II", "Unkown"),
        levels = c(
          "Grade I", "Grade II",
          "Grade III", "Grade IV", "Unkown"
        )
      )
    )
  )

  observed <- apply_all_labels(input)

  expect_equal(observed, expected)
})

test_that("error with wrong inputs", {
  expect_error(
    apply_all_labels(
      lst = data.frame()
    )
  )

  expect_error(
    apply_all_labels(
      lst = list()
    )
  )
})
