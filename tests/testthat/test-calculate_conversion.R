test_that("error on incorrect inputs", {
  expect_error(
    calculate_conversion(list())
  )

  expect_error(
    calculate_conversion(
      data.frame(
        id = c(1, 2),
        date = as.Date(c(1, 2), origin = "1970-01-01"),
        result = c(0, 1)
      )
    )
  )

  expect_error(
    calculate_conversion(
      data.frame(
        id = c(1, 1),
        date = as.Date(c(1, 2), origin = "1970-01-01"),
        result = c(0, 4)
      )
    )
  )

  expect_error(
    calculate_conversion(
      data.frame(
        id = c(1, 1),
        date = as.Date(c(1, NA), origin = "1970-01-01"),
        result = c(0, 1)
      )
    )
  )
})

test_that("conversion check works", {
  input <- data.frame(
    id = c(1, 1),
    date = as.Date(c(1, 50), origin = "1970-01-01"),
    result = c(0, 0)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(1, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("adjusting tolerance works", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.Date(c(1, 29, 50), origin = "1970-01-01"),
    result = c(0, 0, 0)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(1, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  adjusted <- calculate_conversion(
    subject_df = input,
    tolerance = 28
  )

  expect_equal(observed, expected)

  expect_equal(adjusted, data.frame(
    id = 1,
    date = as.Date(1, origin = "1970-01-01")
  ))
})

test_that("handles positive results", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.Date(c(1, 10, 50), origin = "1970-01-01"),
    result = c(1, 0, 0)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(10, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("handles 2 initial positive results", {
  input <- data.frame(
    id = c(1, 1, 1, 1),
    date = as.Date(c(1, 10, 50, 90), origin = "1970-01-01"),
    result = c(1, 1, 0, 0)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(50, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})


test_that("handles terminal positive results", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.Date(c(1, 10, 50), origin = "1970-01-01"),
    result = c(0, 1, 0)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(NA, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("return earliest conversion", {
  input <- data.frame(
    id = c(1, 1, 1, 1, 1),
    date = as.Date(c(1, 35, 40, 45, 90), origin = "1970-01-01"),
    result = c(0, 0, 1, 0, 0)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(1, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("handles all positive", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.Date(c(1, 35, 40), origin = "1970-01-01"),
    result = c(1, 1, 1)
  )

  expected <- data.frame(
    id = 1,
    date = as.Date(NA, origin = "1970-01-01")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})
