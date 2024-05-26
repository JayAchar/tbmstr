#' Calculate differences in results by days
#'
#' @param results participant results data frame
#' @param date_var string - name of date variable
#' @param result_var string - name of result variable
#' @param positive_results list of results which reset the difference
#'   vector to zero
#' @param negative_results list of results which continue the difference
#'   vector
#'
#' @return vector of doubles with with NA representing a positive result
#' @export
#'

calculate_result_diffs <- function(results,
                                   date_var = "date",
                                   result_var = "result",
                                   positive_results = list(1, "MTB complex"),
                                   negative_results = list(3, "No growh")) {
  stopifnot(
    is.data.frame(results),
    is.list(positive_results),
    is.list(negative_results),
    is.character(date_var),
    is.character(result_var)
  )

  date_diffs <- diff(results[[date_var]])

  if (!(all(date_diffs >= 0) || all(date_diffs <= 0))) {
    stop("Data frame is unordered by the date variable")
  }

  row_n <- seq_len(nrow(results))

  vapply(
    X = row_n,
    FUN = function(row) {
      row_result <- results[row, result_var]

      if (row_result %in% positive_results) {
        return(NA)
      }

      if (row == 1) {
        return(0)
      }

      previous_row_result <- results[row - 1, result_var]

      if (previous_row_result %in% negative_results) {
        diff_lag <-
          diff_days(
            results[row - 1, date_var],
            results[row, date_var]
          )
        return(diff_lag)
      }
      return(0)
    },
    FUN.VALUE = double(1)
  )
}
