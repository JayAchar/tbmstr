input <- list(
  baseline = data.frame(
    globalrecordid = c(1, 2),
    recstatus = c(1, 1),
    outcome = c(1, 1),
    height = c(120, 120),
    weight = c(34, 34)
  ),
  adverse = data.frame()
)


test_that("check errors", {
  expect_error(
    prepare_baseline(
      df_list = data.frame()
    )
  )
  expect_warning(
    suppressMessages(prepare_baseline(
      df_list = list(
        adverse = data.frame()
      )
    ), "cliMessage")
  )
  expect_equal(
    suppressWarnings(
      suppressMessages(prepare_baseline(
        df_list = list(
          adverse = data.frame()
        )
      ), "cliMessage")
    ), list(adverse = data.frame())
  )


  remove_withdrawn <- input
  remove_withdrawn$baseline$recstatus[2] <- 0L
  expect_message(
    prepare_baseline(
      df_list = remove_withdrawn
    ), "Found 1 record to remove."
  )
})
#
test_that("mutate `had_sae`", {
  # use sae variable as integer - i.e. with no label applied
  numeric_sae <- input
  numeric_sae$adverse <- data.frame(
    globalrecordid = c(1),
    sae = c(1)
  )

  expect_equal(
    suppressMessages(prepare_baseline(
      df_list = numeric_sae
    ), "cliMessage")$baseline$had_sae,
    c(TRUE, FALSE)
  )


  # use text label applied to SAE variable
  text_sae <- input
  text_sae$adverse <- data.frame(
    globalrecordid = c(1),
    sae = c("Seriouse")
  )

  expect_equal(
    suppressMessages(prepare_baseline(
      df_list = text_sae
    ), "cliMessage")$baseline$had_sae,
    c(TRUE, FALSE)
  )
})

test_that("create binary tx_outcome var", {
  tx_outcome_input <- input
  tx_outcome_input$baseline$outcome[2] <- 3L

  expect_equal(
    prepare_baseline(tx_outcome_input)$baseline$tx_outcome,
    factor(c("Success", "Failure"),
      levels = c("Success", "Failure")
    )
  )
})

test_that("calculate BMI correctly", {
  expect_equal(
    round(
      prepare_baseline(input)$baseline$bmi,
      2
    ), c(23.61, 23.61)
  )

  missing_height <- input
  missing_height$baseline$height[1] <- NA
  expect_equal(
    round(
      prepare_baseline(missing_height)$baseline$bmi,
      2
    ), c(NA, 23.61)
  )
})
