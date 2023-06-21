test_that("error if file does not exist", {
  expect_error(
    read_in(
      "test-data",
      file_names = list(baseline = "baseline")
    )
  )
})

# Create folder structure and add single file
temp_dir <- file.path(tempdir(), "tests")
dir_count <- 3

invisible(lapply(
  X = 1:dir_count,
  \(x) {
    dir.create(
      file.path(
        temp_dir, as.character(x)
      ),
      recursive = TRUE
    )

    write.csv(mtcars,
      file = file.path(
        temp_dir, as.character(x),
        "test_data.csv"
      )
    )
  }
))

test_that("read_in reads in CSV files with parent_dir argument correctly", {
  # test the read_csv_file function with parent_dir argument
  observed <- suppressMessages(read_in(
    parent_dir = temp_dir,
    file_names = list(
      baseline = "test_data"
    )
  ))

  expect_true(is.list(observed))
  expect_true(length(observed) == dir_count)
  expect_equal(names(observed), c("1", "2", "3"))

  expect_true(is.data.frame(observed[["1"]]$baseline))

  # one additional column is added when writing mtcars to CSV
  # since the row.names are converted to their own column
  expect_equal(dim(observed[["1"]][["baseline"]]), c(32, 12))

  # # check that the column names are correct
  expect_equal(
    colnames(observed[["1"]][["baseline"]]),
    c("X", colnames(mtcars))
  )

  expect_message(read_in(
    parent_dir = temp_dir,
    file_names = list(baseline = "test_data")
  ))

  expect_true(
    is.list(
      suppressMessages(
        read_in(
          parent_dir = temp_dir,
          file_names = list(
            baseline = "test_data",
            myco = "test_data"
          )
        )
      )
    )
  )
})

test_that("errors when arguments are passed incorrectly", {
  expect_error(
    read_in(
      parent_dir = temp_dir,
      file_names = c("baseline", "myco")
    )
  )

  expect_error(
    read_in(
      parent_dir = temp_dir,
      file_names = list(
        not_allowed = "test_data"
      )
    )
  )
})

# remove the temporary directory and test file
unlink(
  file.path(temp_dir),
  recursive = TRUE
)
