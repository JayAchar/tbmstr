#' Calculate study variables
#'
#' @param df data frame of baseline characteristics
#'
#' @return data frame of baseline characteritcs with additional
#'   variables
#'
calculate_variables <- function(df) {
  calculated_var_present <- c(
    "tx_outcome",
    "bmi",
    "bmi_group"
  ) %in% names(df)


  has_calculated_vars <- any(
    calculated_var_present
  )

  has_all_calculated_vars <- all(
    calculated_var_present
  )

  if (has_calculated_vars && !has_all_calculated_vars) {
    cli::cli_abort("Partial pre-calculated variable name match.")
  }

  if (has_all_calculated_vars) {
    return(df)
  }

  # create binary end of treatment outcome variable
  df$tx_outcome <- create_binary_tx_outcome(
    df$outcome
  )

  alert_info("`tx_outcome` variable refactored from `outcome`.")

  # calculate BMI variable
  df$bmi <- df$weight /
    ((df$height / 100)^2)

  df$bmi_group <- factor(
    cut(df$bmi, c(0, 18.4999999, 100)),
    labels = c("\u003C18.5", "\u226518.5")
  )

  alert_info("`bmi` variable calculated from \\
                      `height` and `weight`.")

  return(df)
}
