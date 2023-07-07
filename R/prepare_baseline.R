#' Prepare baseline data frame
#'
#' @param df_list List of named data frames
#'
#' @importFrom cli cli_abort cli_warn cli_alert_warning cli_alert_info
#'
#' @return df_list with modifications
#'
prepare_baseline <- function(df_list) {
  if (!is.list(df_list) || is.data.frame(df_list)) {
    cli::cli_abort("`df_list` must be a list")
  }

  required <- c("baseline", "adverse")
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

  # create binary end of treatment outcome variable
  df_list$baseline$tx_outcome <- factor(ifelse(
    df_list$baseline$outcome %in% list("Cured", "Completed", 1, 2),
    "Success", "Failure"
  ), levels = c("Success", "Failure"))
  cli::cli_alert_info("`tx_outcome` variable refactored from `outcome`.")

  # calculate BMI variable
  df_list$baseline$bmi <- df_list$baseline$weight /
    ((df_list$baseline$height / 100)^2)

  if (!"had_sae" %in% names(df_list$baseline)) {
    had_sae <- unique(df_list$adverse$globalrecordid[which(
      df_list$adverse$sae %in% list(1, "Seriouse")
    )])
    df_list$baseline$had_sae <- df_list$baseline$globalrecordid %in% had_sae
  }

  return(df_list)
}
