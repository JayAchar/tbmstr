test_that("error if file does not exist", {
  expect_error(
    read_in(
      "test-data",
      file_names = list(baseline = "baseline")
    )
  )
})

# create a temporary directory and test CSV file
temp_dir <- tempdir()
test_file <- file.path(temp_dir, "test_data.csv")
write.csv(mtcars, file = test_file, row.names = FALSE)

test_that("read_in reads in CSV files with dir_path argument correctly", {
  # test the read_csv_file function with dir_path argument
  observed <- suppressMessages(read_in(
    dir_path = temp_dir,
    file_names = list(
      baseline = "test_data"
    )
  ))

  expect_true(is.list(observed))
  expect_true(length(observed) == 1)
  expect_true(names(observed) == "baseline")

  # check that the data frame has the correct dimensions
  expect_equal(dim(observed[["baseline"]]), c(32, 11))

  # check that the column names are correct
  expect_equal(colnames(observed[["baseline"]]), colnames(mtcars))

  expect_message(read_in(
    dir_path = temp_dir,
    file_names = list(baseline = "test_data")
  ))
})

test_that("errors when arguments are passed incorrectly", {
  expect_error(
    read_in(
      dir_path = temp_dir,
      file_names = c("baseline", "myco")
    )
  )

  expect_error(
    read_in(
      dir_path = temp_dir,
      file_names = list(
        not_allowed = "test_data"
      )
    )
  )
})

# remove the temporary directory and test file
unlink(test_file)
unlink(temp_dir, recursive = TRUE)

