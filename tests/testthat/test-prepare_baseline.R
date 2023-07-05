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
          recstatus = c(1, 1),
          outcome = c(1, 1)
        )
      )
    ), "`tx_outcome` variable refactored from `outcome`"
  )

  expect_message(
    prepare_baseline(
      df_list = list(
        baseline = data.frame(
          recstatus = c(1, 0),
          outcome = c(1, 1)
        )
      )
    ), "Found 1 record to remove."
  )

  expect_equal(
    suppressMessages(prepare_baseline(
      df_list = list(
        baseline = data.frame(
          recstatus = c(1, 1),
          outcome = c(1, 1)
        )
      )
    ), "cliMessage"), list(baseline = data.frame(
      recstatus = c(1, 1),
      outcome = c(1, 1),
      tx_outcome = factor(c("Success", "Success"),
        levels = c("Success", "Failure")
      ),
      had_sae = NA
    ))
  )
})

test_that("mutate `had_sae`", {
  # use sae variable as integer - i.e. with no label applied
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      recstatus = c(1, 1),
      outcome = c(1, 1)
    ),
    adverse = data.frame(
      globalrecordid = c("1"),
      sae = c(1)
    )
  )

  expect_equal(
    suppressMessages(prepare_baseline(
      df_list = input
    ), "cliMessage")$baseline$had_sae,
    c(TRUE, FALSE)
  )


  # use text label applied to SAE variable
  input <- list(
    baseline = data.frame(
      globalrecordid = c("1", "2"),
      recstatus = c(1, 1),
      outcome = c(1, 1)
    ),
    adverse = data.frame(
      globalrecordid = c("1"),
      sae = c("Seriouse")
    )
  )
  expect_equal(
    suppressMessages(prepare_baseline(
      df_list = input
    ), "cliMessage")$baseline$had_sae,
    c(TRUE, FALSE)
  )
})

test_that("create binary tx_outcome var", {
  input <- list(
    baseline = data.frame(
      recstatus = c(1, 1),
      outcome = c(1, 3)
    )
  )

  expect_equal(
    suppressMessages(
      prepare_baseline(input),
      "cliMessage"
    ),
    list(baseline = data.frame(
      recstatus = c(1, 1),
      outcome = c(1, 3),
      tx_outcome = factor(c("Success", "Failure"),
        levels = c("Success", "Failure")
      ),
      had_sae = NA
    ))
  )
})
