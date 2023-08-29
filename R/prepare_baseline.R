#' Prepare baseline data frame
#'
#' @param df_list List of named data frames
#' @param cohort define cohort type required
#'
#' @importFrom cli cli_abort cli_warn cli_alert_warning cli_alert_info
#'
#' @return df_list with modifications
#'
prepare_baseline <- function(df_list, cohort = c("treatment", "adverse")) {
  cohort <- match.arg(cohort)

  if (!is.list(df_list) || is.data.frame(df_list)) {
    cli::cli_abort("`df_list` must be a list")
  }

  required <- c("baseline", "adverse", "myco")
  is_not_df <- vapply(
    required,
    \(name) !is.data.frame(df_list[[name]]),
    logical(1)
  ) |> setNames(required)

  if (!all(required %in% names(df_list))) {
    cli::cli_alert_danger("Required data frames not detected in input list.")
    cli::cli_h1("Required named data frames:")
    ul <- cli::cli_ul()
    cli::cli_li("baseline")
    cli::cli_li("adverse")
    cli::cli_end(ul)
    cli::cli_warn("Returning input list.")
    return(df_list)
  }

  if (any(is_not_df)) {
    cli::cli_warn("All elements in `df_list` must be a data frame")
    cli::cli_alert_warning("Returning input list.")
    return(df_list)
  }

  df_list$baseline <- remove_invalid_records(
    df_list[["baseline"]]
  )

  if (cohort == "treatment") {
    df_list$baseline <- remove_withdrawn_subjects(df_list[["baseline"]])
  }

  # create binary end of treatment outcome variable
  df_list$baseline$tx_outcome <- create_binary_tx_outcome(
    df_list$baseline$outcome
  )
  if (!is_testing()) {
    cli::cli_alert_info("`tx_outcome` variable refactored from `outcome`.")
  }

  # TODO: create baseline CD4 count categorical variable

  # calculate BMI variable
  df_list$baseline$bmi <- df_list$baseline$weight /
    ((df_list$baseline$height / 100)^2)

  # TODO: categorise BMI variable

  if (!is_testing()) {
    cli::cli_alert_info("`bmi` variable calculated from \\
                      `height` and `weight`.")
  }

  # if trtendat is missing, impute endat
  df_list$baseline$trtendat <- ifelse(
    is.na(df_list$baseline$trtendat),
    df_list$baseline$endat,
    df_list$baseline$trtendat
  )

  df_list$baseline <- create_eos_outcome(
    df_list$baseline
  )

  if (!is_testing()) {
    cli::cli_alert_info("`eos_outocome` variable calculated as first
                        unsuccessful outcome.")
  }

  # FIXME: use create_cc_days function to check the cc days var
  df_list$baseline$cc_days <- create_cc_days(
    trtstdat = df_list$baseline$trtstdat,
    convdat = df_list$baseline$convdat
  )

  if (!is_testing()) {
    cli::cli_alert_info("`cc_days` variable calculated from \\
                      `trtstdat` and `convdat`.")
  }

  df_list$baseline <- calculate_baseline_smear(
    baseline = df_list$baseline,
    myco = df_list$myco
  )

  if (!is_testing()) {
    cli::cli_alert_info("`smear` variable calculated from \\
                      `myco` data frame.")
  }

  if (!"had_sae" %in% names(df_list$baseline)) {
    had_sae <- unique(df_list$adverse$globalrecordid[which(
      df_list$adverse$sae %in% list(1, "Seriouse")
    )])
    df_list$baseline$had_sae <- df_list$baseline$globalrecordid %in% had_sae
  }

  return(df_list)
}
