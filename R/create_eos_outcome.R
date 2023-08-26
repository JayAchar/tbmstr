#' Create End of Study outcome
#'
#' The original end of study outcome - `stat12` only provides information
#' about a participants status 12 months after treatment completion. It does
#' not incorporate any information about their on-treatment experience - for
#' example, if a participant fails on treatment, their `status12` will be
#' missing. This function combines the end of treatment outcome and the
#' `status12` variable to create a valid end of study outcome.
#'
#' @param df baseline data frame
#'
#' @return vector (factor) with the same length as the input arguments which
#' represents the end of study outcome

create_eos_outcome <- function(df) {
  # TODO: refactor after completing prepare_eos_outcome & calculate_eos_outcome
  # input should be the baseline data frame
  # pass basline data frame to prepare_eos_outcome
  # pass output to calculate_eos_outcome
  # confirm that each original subject has an outcome
  # generate warnings for odd outcomes
  # recurrence after death

  required_vars <- c(
    "globalrecordid", "trtendat",
    "outcome", "stat3",
    "stat6", "stat9", "stat12",
    "evldat3", "evldat6", "evldat9", "evldat12"
  )

  if (!is.data.frame(df)) {
    cli::cli_abort("Input argument should be a data frame.")
  }

  if (!all(required_vars %in% names(df))) {
    cli::cli_abort("Input data frame does not include required variables.")
  }

  eos_levels <- c(
    "No TB", "Reoccurance", "Died during follow-up",
    "Unsuccessful treatment", "Not evaluated"
  )

  eos <- vapply(
    seq_along(eot),
    \(i) {
      if (
        (eot[i] %in% list(
          1, 2, "Cured", "Completed"
        )) && eof[i] %in% list(
          1, "No TB"
        )) {
        return("No TB")
      }

      if (
        eot[i] %in% list(
          3, 4, 5,
          "Failed",
          "Died",
          "Lost to follow-up"
        )
      ) {
        return("Unsuccessful treatment")
      }

      if (
        (eot[i] %in% list(
          1, 2, "Cured", "Completed"
        )) && eof[i] %in% list(
          2, "Reoccurance"
        )
      ) {
        return("Reoccurance")
      }

      if (
        (eot[i] %in% list(
          1, 2, "Cured", "Completed"
        )) && eof[i] %in% list(
          3, "Died"
        )
      ) {
        return("Died during follow-up")
      }


      if (
        eot[i] %in% list(
          6, "Not evaluated"
        ) || eof[i] %in% list(
          4, "Not evvaluated"
        )
      ) {
        return("Not evaluated")
      }

      return(NA_character_)
    }, character(1)
  )

  return(
    factor(eos,
      levels = eos_levels
    )
  )
}
