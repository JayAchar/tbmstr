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

  if (!"baseline" %in% names(df_list) || !is.data.frame(df_list$baseline)) {
    cli::cli_warn("`baseline` data frame not detected")
    return(df_list)
  }

  df_list$baseline <- remove_invalid_records(
    df_list[["baseline"]]
  )

  df_list$baseline$tx_outcome <- factor(ifelse(
    df_list$baseline$outcome %in% list("Cured", "Completed", 1, 2),
    "Success", "Failure"
  ), levels = c("Success", "Failure"))
  cli::cli_alert_info("`tx_outcome` variable refactored from `outcome`.")

  if ("adverse" %in% names(df_list)) {
    if (!"had_sae" %in% df_list$baseline) {
      had_sae <- unique(df_list$adverse$globalrecordid[which(
        df_list$adverse$sae %in% list(1, "Seriouse")
      )])
      df_list$baseline$had_sae <- df_list$baseline$globalrecordid %in% had_sae
    }
  } else {
    cli::cli_alert_warning("`adverse` data frame not detected \\
                         - `had_sae` variable not calculated")
    df_list$baseline$had_sae <- NA
  }

  return(df_list)
}
