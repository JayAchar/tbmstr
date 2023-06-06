#' Remove user-defined invalid records
#'
#' During data entry, users can mark a record as invalid instead of deleting
#' it. This function removes these invalid records and logs how many rows
#' have been removed
#'
#' @param df Data frame with the recstatus variable defined.
#'
#' @importFrom cli cli_abort cli_alert_info
#'
#' @return Data frame with invalid records removed
#'
remove_invalid_records <- function(df) {
  is_valid_arg <- is.data.frame(df) & "recstatus" %in% names(df)
  if (!is_valid_arg) {
    cli::cli_abort("This function requires a data frame with the `recstatus`
                   variable to be provided")
  }

  is_recstatus_valid <- all(df$recstatus %in% c(0, 1))
  if (!is_recstatus_valid) {
    cli::cli_abort(
      "The `recstatus` variable should only include 0 or 1."
    )
  }

  to_remove <- df[which(df$recstatus == 0), , drop = FALSE]

  if (nrow(to_remove) > 0) {
    cli::cli_alert_info(
      "Found {nrow(to_remove)} record{?s} to remove."
    )
  }

  df[which(df$recstatus == 1), , drop = FALSE]
}
