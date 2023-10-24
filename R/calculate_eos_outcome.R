#' Calculate End of Study outcome
#'
#' Follow up status is collected longitudinally,
#' so the first unsuccessful outcome with its date
#' must be retrieved
#'
#' @param df data frame for one subject with end of
#' treatment and follow up outcomes with their
#' respective date variables
#'
#' @param follow_up_df data frame of describing the final post treatment
#' follow up date, status and month
#'
#' @return data frame with
#'
calculate_eos_outcome <- function(df, follow_up_df) {
  stopifnot(
    nrow(df) == 1,
    nrow(follow_up_df) == 1,
    df$globalrecordid == follow_up_df$globalrecordid
  )

  # definitions from package data
  defs <- internal$definitions

  ## if treatment failure is found return early with
  ## failure result and date
  if (df$outcome %in% c(defs$eot_failure, "Withdrawn")) {
    eos_outcome <- NULL

    if (df$outcome == "Died") {
      eos_outcome <- "Died"
    }

    if (df$outcome == "Failed") {
      eos_outcome <- "Treatment failure"
    }
    if (df$outcome == "Lost to follow-up") {
      eos_outcome <- "Treatment LTFU"
    }
    if (df$outcome == "Withdrawn") {
      warning("Withdrawn subjects detected - suggest removing")
      eos_outcome <- "Not evaluated"
    }

    if (is.null(eos_outcome)) {
      cli::cli_abort("{df$globalrecordid}: Treatment outcome not recognised")
    }

    return(
      data.frame(
        globalrecordid = df$globalrecordid,
        eos_outcome = factor(eos_outcome,
          levels = defs$eos_levels
        ),
        eos_date = df$trtendat
      )
    )
  }

  ## if treatment success
  # FIX: some participants have deathfu == TRUE but no FU eval == Died
  # Suggest including deathfu == TRUE which also have a deathdat
  # but not if the date is missing and no mention in the FU evals
  #
  # FIX: if the eos_outcome will be death, then the deathdat should be used
  # unless it is na
  return(
    data.frame(
      globalrecordid = df$globalrecordid,
      eos_outcome = factor(follow_up_df$final_fu_status,
        levels = defs$eos_levels
      ),
      eos_date = follow_up_df$final_fu_date
    )
  )
}
