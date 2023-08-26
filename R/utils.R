#' Merge and Filter by start date
#'
#' Merge a data frame with the `baseline` data frame then filter entries
#' according to the treatment initaition date
#'
#' @param baseline Data frame of baseline study data.
#' @param target Data frame of target study data.
#' @param target_date_var String representing the date variable in the target.
#'
#' @return Data frame
merge_and_filter_by_start_date <- function(baseline, target, target_date_var) {
  if (!(target_date_var %in% names(target))) {
    cli::cli_abort(
      "The `{target_date_var}` variable was not found in the target data frame."
    )
  }

  merged <- merge(
    baseline,
    target,
    by.x = "globalrecordid",
    by.y = "fkey",
    all.x = TRUE
  )

  merged$trtstdat <- transform_to_date(merged$trtstdat)
  merged[[target_date_var]] <- transform_to_date(merged[[target_date_var]])
  # remove all results that are after treatment initiation
  merged[which(
    merged[[target_date_var]] <= merged$trtstdat
  ), ]
}



#' @noRd

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


#' @noRd
create_binary_tx_outcome <- function(outcome) {
  var <- factor(ifelse(
    outcome %in% list("Cured", "Completed", 1, 2),
    "Successful", "Unsuccessful"
  ), levels = c("Successful", "Unsuccessful"))
  var[which(is.na(outcome))] <- NA
  var
}
