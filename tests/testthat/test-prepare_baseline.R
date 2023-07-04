test_that("check errors", {
  expect_error(
    prepare_baseline(
      df_list = data.frame()
    )
  )
  expect_warning(
    prepare_baseline(
      df_list = list(
        adverse = data.frame()
      )
    )
  )
  expect_equal(
    suppressWarnings(prepare_baseline(
      df_list = list(
        adverse = data.frame()
      )
    )), list(adverse = data.frame())
  )

  expect_message(
    prepare_baseline(
      df_list = list(
        baseline = data.frame(
          recstatus = c(1, 0)
        )
      )
    )
  )

  expect_equal(
    suppressMessages(prepare_baseline(
      df_list = list(
        baseline = data.frame(recstatus = c(1, 1))
      )
    )), list(baseline = data.frame(recstatus = c(1, 1)))
  )
})

test_that("mutate `had_sae`", {
  # use sae variable as integer - i.e. with no label applied
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      recstatus = c(1, 1)
    ),
    adverse = data.frame(
      globalrecordid = c("1"),
      sae = c(1)
    )
  )

  expect_equal(
    prepare_baseline(
      df_list = input
    )$baseline$had_sae, c(TRUE, FALSE)
  )

  # use text label applied to SAE variable
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      recstatus = c(1, 1)
    ),
    adverse = data.frame(
      globalrecordid = c("1"),
      sae = c("Seriouse")
    )
  )
  expect_equal(
    prepare_baseline(
      df_list = input
    )$baseline$had_sae, c(TRUE, FALSE)
  )
})
