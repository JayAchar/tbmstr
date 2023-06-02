if (Sys.getenv("_R_CHECK_CRAN_INCOMING_") == "") {
  test_that("confirm test data is available", {
    expect_true(dir.exists(here::here("data")))
    expect_true(dir.exists(here::here("data", "test-data")))
    expect_true(dir.exists(here::here(
      "data", "test-data",
      "redcap"
    )))
    expect_true(dir.exists(here::here(
      "data", "test-data",
      "epiinfo"
    )))
    expect_true(file.exists(here::here(
      "data", "test-data",
      "redcap", "redcap.csv"
    )))
  })
}
