test_that("single study data file works", {
  df_list <- list(
    site_1 = list(
      baseline = cars
    )
  )

  observed <- merge_imported(df_list = df_list)
  expect_true(is.list(observed))
  expect_equal(length(observed), 1)
  expect_true(names(observed) == "baseline")
  expect_equal(
    observed$baseline, cars
  )
})

test_that("two study data files work", {
  df_list <- list(
    site_1 = list(
      baseline = cars[1:25, ],
      adverse = iris[1:100, ]
    ),
    site_2 = list(
      baseline = cars[26:50, ],
      adverse = iris[101:150, ]
    )
  )

  observed <- merge_imported(df_list = df_list)

  expect_true(is.list(observed))
  expect_equal(length(observed), 2)

  expect_true(is.data.frame(observed$baseline))
  expect_equal(observed$baseline, cars)

  expect_true(is.data.frame(observed$adverse))
  expect_equal(observed$adverse, iris)
})

test_that("handles multi-country data", {
  modified_cars <- cbind(cars, data.frame(cntry = 1))
  df_list <- list(
    site_1 = list(
      baseline = modified_cars[1:25, ],
      adverse = iris[1:100, ]
    ),
    site_2 = list(
      baseline = modified_cars[26:50, ],
      adverse = iris[101:150, ]
    )
  )

  observed <- suppressMessages(merge_imported(df_list = df_list))

  expect_true(is.list(observed))
  expect_equal(length(observed), 2)

  expect_true(is.data.frame(observed$baseline))
  expect_equal(observed$baseline, modified_cars)

  expect_true(is.data.frame(observed$adverse))
  expect_equal(observed$adverse, iris)


  multiple_cars <- cbind(cars, data.frame(cntry = c(1, 2)))
  df_list <- list(
    site_1 = list(
      baseline = multiple_cars[1:25, ],
      adverse = iris[1:100, ]
    ),
    site_2 = list(
      baseline = multiple_cars[26:50, ],
      adverse = iris[101:150, ]
    )
  )

  expect_error(merge_imported(df_list = df_list))

  observed <- suppressMessages(merge_imported(
    df_list = df_list,
    multi_country = TRUE
  ))

  expect_true(is.list(observed))
  expect_equal(length(observed), 2)

  expect_true(is.data.frame(observed$baseline))
  expect_equal(observed$baseline, multiple_cars)

  expect_true(is.data.frame(observed$adverse))
  expect_equal(observed$adverse, iris)
})
