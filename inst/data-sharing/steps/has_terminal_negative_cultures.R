has_terminal_negative_cultures <- function(results) {
  diffs <- calculate_result_diffs(results,
    date_var = "datespecimen"
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
  if (any(is.na(cum_diffs))) {
    na_position <- which(is.na(cum_diffs))[1]
    cum_diffs <- cum_diffs[1:na_position - 1]
  }
  max_results <- which(min(cum_diffs) == cum_diffs)

  min(cum_diffs) <= -90 && max_results[1] >= 3
}
