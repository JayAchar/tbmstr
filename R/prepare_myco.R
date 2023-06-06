#' Prepare myco data for analysis
#'
#' @param myco Data frame of study mycobacterial data.
#' @param baseline Data frame of baseline study data.
#' @param purpose String variable to represent the puprpose of data preparation
#'    Allowed values are:
#'      * quality
#'
#' @return A data frame merged between `baseline` and `myco` with selected
#' variables
prepare_myco <- function(myco, baseline, purpose = c("quality")) {
  purpose <- match.arg(purpose)

  if (purpose == "quality") {
    return(merge_and_filter_by_start_date(
      baseline = baseline,
      target = myco,
      target_date_var = "datespecimen"
    ))
  }

  stop("This error should not be seen")
}
