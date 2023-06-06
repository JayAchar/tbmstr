#' Prepare DST data for analysis
#'
#' @param dst Data frame of study DST data.
#' @param baseline Data frame of baseline study data.
#' @param purpose String variable to represent the puprpose of data preparation
#'    Allowed values are:
#'      * quality
#'
#' @return A data frame merged between `baseline` and `dst` with selected
#' variables
prepare_dst <- function(dst, baseline, purpose = c("quality")) {
  purpose <- match.arg(purpose)

  if (purpose == "quality") {
    return(
      merge_and_filter_by_start_date(
        baseline = baseline,
        target = dst,
        target_date_var = "datedst"
      )
    )
  }

  stop("This error should not be seen")
}
