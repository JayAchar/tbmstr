test_that("error if file does not exist", {
  expect_error(
    read_in(
      "test-data",
      file_names = "baseline"
    )
  )
})

test_that("read_in reads in CSV files with dir_path argument correctly", {
  # create a temporary directory and test CSV file
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_data.csv")
  write.csv(mtcars, file = test_file, row.names = FALSE)
  
  # test the read_csv_file function with dir_path argument
  observed <- suppressMessages(read_in(dir_path = temp_dir,
                file_names = c("test_data")))
  
  expect_true(is.list(observed))
  expect_true(length(observed) == 1)
  
  # check that the data frame has the correct dimensions
  expect_equal(dim(observed[[1]]), c(32, 11))
  
  # check that the column names are correct
  expect_equal(colnames(observed[[1]]), colnames(mtcars))
  
  expect_message(read_in(dir_path = temp_dir,
                         file_names = c("test_data")))
  
  # remove the temporary directory and test file
  unlink(test_file)
  unlink(temp_dir, recursive = TRUE)
})