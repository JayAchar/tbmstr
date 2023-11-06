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
  DAY <- 60 * 60 * 24

  # definitions from package data
  defs <- internal$definitions

  ## if treatment failure is found return early with
  ## failure result and date
  if (df$outcome %in% c(defs$eot_failure, "Withdrawn")) {
    eos_outcome <- NULL
    eos_date <- df$trtendat

    if (df$outcome == "Died") {
      eos_outcome <- "Died"
    }

    if (df$outcome == "Failed") {
      eos_outcome <- "Treatment failure"
      # censor follow-up time for subjects who fail on treatment.
      # failure should not occur after treatment so end of treatment
      # date should be restricted to plausible treatment dates
      eos_date <- min(
        df$trtendat,
        df$trtstdat + 300 * DAY,
        na.rm = TRUE
      )
    }
    if (df$outcome == "Lost to follow-up") {
      eos_outcome <- "Treatment LTFU"
    }
    if (df$outcome == "Withdrawn") {
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
        eos_date = eos_date
      )
    )
  }

  ## if treatment success

  if (df$deathfu == TRUE && !is.na(df$deathdat)) {
    outcome <- list(value = "Died", dd = df$deathdat)
  } else {
    outcome <- list(
      value = follow_up_df$final_fu_status,
      dd = follow_up_df$final_fu_date
    )
  }

  return(
    data.frame(
      globalrecordid = df$globalrecordid,
      eos_outcome = factor(outcome$value,
        levels = defs$eos_levels
      ),
      eos_date = outcome$dd
    )
  )
}
