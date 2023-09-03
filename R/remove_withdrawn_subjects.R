#' Remove withdrawn subjects
#'
#' @param df baseline data frame
#'
#' @return data frame with withdrawn subject rows removed
#'
remove_withdrawn_subjects <- function(df) {
  is_withdrawn <- df$outcome %in% list(7, "Withdrawn")
  if (sum(is_withdrawn) > 0) {
    cli::cli_inform("{sum(is_withdrawn)} withdrawn subjects have been removed.")
  }

  df[which(!is_withdrawn), ]
}

