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
    all(subject_df$result %in% c(0, 1)),
    all(!is.na(subject_df$date))
  )

  # capture number of results
  result_sequence <- seq_len(nrow(subject_df))

  # add days_after_previous_negative variable
  diffs <- vapply(
    X = result_sequence,
    FUN = function(row) {
      if (row == 1 || subject_df[row, "result"] == 1) {
        return(0)
      }

      # if the difference is less than the tolerance, check the previous
      # row.

      # For each result, recurse backwards to find the longest sequence of
      # negative results then calculate the number of days

      days_diff <- Reduce(
        x = row:min(result_sequence),
        f = function(res, inc) {
          if (inc == 1) {
            return(res)
          }
          if (subject_df[inc, "result"] == 1) {
            return(0)
          }
          # stop recursing back if the current diff since negative > required
          if (res >= tolerance) {
            return(res)
          }
          # if we encounter a positive result when recursing back,
          # reset the value
          if (subject_df[inc - 1, "result"] == 1) {
            return(0)
          }
          # if the recursed result is also negative, add accumulate the
          # additional diff_days
          if (subject_df[inc - 1, "result"] == 0) {
            diff <- as.numeric(subject_df[row, "date"] -
              subject_df[row - 1, "date"])
            return(diff + res)
          }
        },
        init = 0
      )
      return(days_diff)
    },
    FUN.VALUE = double(1)
  )

  subject_df$diff <- diffs
  # retain candidate results which might represent conversion
  candidates <- subject_df[which(subject_df$diff >= tolerance), ]

  if (nrow(candidates) == 0) {
    conversion_date <- as.Date(NA)
  } else {
    conversion_date <- min(candidates$date)
  }

  return(
    data.frame(
      id = unique(subject_df$id),
      date = conversion_date
    )
  )
}
