#' Prepare baseline data frame
#'
#' @param df_list List of named data frames
#'
#' @importFrom cli cli_abort cli_warn cli_alert_warning
#'
#' @return
#'
prepare_baseline <- function(df_list) {
  if (!is.list(df_list) || is.data.frame(df_list)) {
    cli::cli_abort("`df_list` must be a list")
  }

  if (!"baseline" %in% names(df_list)) {
    cli::cli_warn("`baseline` data frame not detected")
    return(df_list)
  }

  df_list$baseline <- remove_invalid_records(
    df_list[["baseline"]]
  )
  if (!"adverse" %in% names(df_list)) {
    cli::cli_alert_warning("`adverse` data frame not detected \\
                         - `had_sae` variable not appended")
    return(df_list)
  }

  had_sae <- unique(df_list$adverse$globalrecordid[which(
    df_list$adverse$sae %in% list(1, "Seriouse")
  )])
  df_list$baseline$had_sae <- df_list$baseline$globalrecordid %in% had_sae

  return(df_list)
}
