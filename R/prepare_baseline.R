#' Prepare baseline data frame
#'
#' @param df_list List of named data frames
#' @param cohort define cohort type required
#'
#' @importFrom cli cli_abort cli_warn cli_alert_warning cli_alert_info
#' @export
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

  # calculate BMI variable
  df_list$baseline$bmi <- df_list$baseline$weight /
    ((df_list$baseline$height / 100)^2)

  df_list$baseline$bmi_group <- factor(
    cut(df_list$baseline$bmi, c(0, 18.4999999, 100)),
    labels = c("\u003C18.5", "\u226518.5")
  )

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

  df_list$baseline <- create_conversion_variables(
    baseline = df_list$baseline,
    myco = df_list$myco
  )

  if (!is_testing()) {
    cli::cli_alert_info("`cc_days` variable calculated from \\
                      `trtstdat` and `convdat`.")
  }

  df_list$baseline <- calculate_baseline_smear(
    baseline = df_list$baseline,
    myco = df_list$myco
  )

  df_list$baseline$outcome <- droplevels(
    df_list$baseline$outcome
  )


  if (!is_testing()) {
    cli::cli_alert_info("`smear` variable calculated from \\
                      `myco` data frame.")
  }

  df_list$baseline <- handle_factors(
    df_list$baseline
  )

  if (!"had_sae" %in% names(df_list$baseline)) {
    had_sae <- unique(df_list$adverse$globalrecordid[which(
      df_list$adverse$sae %in% list(1, "Seriouse")
    )])
    df_list$baseline$had_sae <- df_list$baseline$globalrecordid %in% had_sae
  }

  return(df_list)
}
