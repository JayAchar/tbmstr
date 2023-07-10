#' Create End of Study outcome
#'
#' The original end of study outcome - `stat12` only provides information
#' about a participants status 12 months after treatment completion. It does
#' not incorporate any information about their on-treatment experience - for
#' example, if a participant fails on treatment, their `status12` will be
#' missing. This function combines the end of treatment outcome and the
#' `status12` variable to create a valid end of study outcome.
#'
#' @param eot vector of End of treatment outcomes
#' @param eof vector of End of follow-up outcomes
#'
#' @return vector (factor) with the same length as the input arguments which
#' represents the end of study outcome

create_eos_outcome <- function(eot, eof) {
  is_eot_ok <- is.numeric(eot) || is.factor(eot)
  is_eof_ok <- is.numeric(eof) || is.factor(eof)
  if (!is_eot_ok || !is_eof_ok) {
    cli::cli_abort("Input arguments should be numeric vectors or factors.")
  }

  if (length(eot) != length(eof)) {
    cli::cli_abort("Input arguments should be the same length.")
  }

  if (class(eot) != class(eof)) {
    cli::cli_abort("Input arguments should be the same class.")
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
