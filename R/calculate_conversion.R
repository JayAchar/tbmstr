#' Calculate conversion date
#'
#' @param subject_df data frame with ID, date and result variables
#' @param tolerance integer to represent number of days between negative
#' results which constitute conversion
#'
#' @return data frame with subject ID and conversion date variable
#'
calculate_conversion <- function(subject_df,
                                 tolerance = 30L) {
  stopifnot(
    is.data.frame(subject_df),
    all(c("id", "date", "result") %in% names(subject_df)),
    length(unique(subject_df$id)) == 1L,
    all(subject_df$result %in% list(3, 1, "No growh", "MTB complex")),
    all(!is.na(subject_df$date))
  )

  # order by date
  subject_df <- subject_df[order(subject_df$date), ]

  diffs <- calculate_result_diffs(
    results = subject_df
    # use function default values
  )

  cum_diffs <- Reduce(
    x = diffs,
    f = \(acc, value) {
      if (is.na(value)) {
        return(c(acc, NA))
      }
      lag_value <- ifelse(
        length(acc) == 0,
        0, acc[length(acc)]
      )
      if (is.na(lag_value)) {
        return(c(acc, value))
      }
      return(c(acc, lag_value + value))
    }, init = numeric(0)
  )

  gt_tolerance <- which(cum_diffs >= tolerance)

  conversion_date <- as.POSIXct(NA, tz = "UTC")

  if (length(gt_tolerance) > 0) {
    all_zeros <- which(cum_diffs == 0)
    target_zero <- min(all_zeros[all_zeros < min(gt_tolerance)])
    conversion_date <- subject_df[target_zero, "date"]
  }

  return(
    data.frame(
      id = unique(subject_df$id),
      date = conversion_date
    )
  )
}
