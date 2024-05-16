DAY <- 24 * 60 * 60
test_that("error on incorrect inputs", {
  expect_error(
    calculate_conversion(list())
  )

  expect_error(
    calculate_conversion(
      data.frame(
        id = c(1, 2),
        date = as.POSIXct(c(1, 2) * DAY, origin = "1970-01-01", tz = "UTC"),
        result = c(0, 1)
      )
    )
  )

  expect_error(
    calculate_conversion(
      data.frame(
        id = c(1, 1),
        date = as.POSIXct(c(1, 2) * DAY, origin = "1970-01-01", tz = "UTC"),
        result = c(0, 4)
      )
    )
  )

  expect_error(
    calculate_conversion(
      data.frame(
        id = c(1, 1),
        date = as.POSIXct(c(1, NA) * DAY, origin = "1970-01-01", tz = "UTC"),
        result = c(0, 1)
      )
    )
  )
})

test_that("conversion check works", {
  input <- data.frame(
    id = c(1, 1),
    date = as.POSIXct(c(1, 50) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c(3, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(1 * DAY, origin = "1970-01-01", tz = "UTC")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("conversion check works with result labels", {
  input <- data.frame(
    id = c(1, 1),
    date = as.POSIXct(c(1, 50) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c("No growh", "No growh")
  )
  
  expected <- data.frame(
    id = 1,
    date = as.POSIXct(1 * DAY, origin = "1970-01-01", tz = "UTC")
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
    date = as.POSIXct(c(1, 29, 50) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c(3, 3, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(1 * DAY, origin = "1970-01-01", tz = "UTC")
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
    date = as.POSIXct(1 * DAY, origin = "1970-01-01", tz = "UTC")
  ))
})

test_that("handles positive results", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.POSIXct(c(1, 10, 50) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c(1, 3, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(10 * DAY, origin = "1970-01-01", tz = "UTC")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("handles positive results with labels", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.POSIXct(c(1, 10, 50) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c("MTB complex", "No growh", "No growh")
  )
  
  expected <- data.frame(
    id = 1,
    date = as.POSIXct(10 * DAY, origin = "1970-01-01", tz = "UTC")
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
    date = as.POSIXct(c(1, 10, 50, 90) * DAY,
      origin = "1970-01-01", tz = "UTC"
    ),
    result = c(1, 1, 3, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(50 * DAY, origin = "1970-01-01", tz = "UTC")
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
    date = as.POSIXct(c(1, 10, 50) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c(3, 1, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(NA * DAY, origin = "1970-01-01", tz = "UTC")
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
    date = as.POSIXct(c(1, 35, 40, 45, 90) * DAY,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    result = c(3, 3, 1, 3, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(1 * DAY, origin = "1970-01-01", tz = "UTC")
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
    date = as.POSIXct(c(1, 35, 40) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c(1, 1, 1)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})

test_that("handles unordered results", {
  input <- data.frame(
    id = c(1, 1, 1),
    date = as.POSIXct(c(35, 1, 40) * DAY, origin = "1970-01-01", tz = "UTC"),
    result = c(3, 3, 3)
  )

  expected <- data.frame(
    id = 1,
    date = as.POSIXct(1 * DAY, origin = "1970-01-01", tz = "UTC")
  )

  observed <- calculate_conversion(
    subject_df = input,
    tolerance = 30
  )

  expect_equal(observed, expected)
})
