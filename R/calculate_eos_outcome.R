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
#' @return data frame with
#'
calculate_eos_outcome <- function(df) {
  stopifnot(
    all(
      c(
        "globalrecordid", "outcome", "trtendat",
        "status", "fudat"
      ) %in%
        names(df)
    ),
    length(unique(df$globalrecordid)) == 1
  )

  # FIXME: incorporate deathdat for death_df

  # definitions from package data
  defs <- internal$definitions

  ## if treatment failure is found return early with
  ## failure result and date
  if (any(df$outcome %in% c(defs$eot_failure, "Withdrawn"))) {
    eos_outcome <- "Died"

    if (df$outcome[1] == "Failed") {
      eos_outcome <- "Treatment failure"
    }
    if (df$outcome[1] == "Lost to follow-up") {
      eos_outcome <- "Treatment LTFU"
    }
    if (df$outcome[1] == "Withdrawn") {
      warning("Withdrawn subjects detected - suggest removing")
      eos_outcome <- "Not evaluated"
    }

    return(
      data.frame(
        globalrecordid = df$globalrecordid[1],
        eos_outcome = factor(eos_outcome,
          levels = defs$eos_levels
        ),
        eos_date = df$trtendat[1],
        event_death = eos_outcome == "Died",
        date_death = ifelse(eos_outcome == "Died",
          as.POSIXct(df$trtendat[1], tz = "UTC"),
          as.POSIXct(NA_character_)
        )
      )
    )
  }

  ## if treatment success
  ## sort data frame by fudat
  sorted <- df[order(df$fudat), ]

  death_indices <- which(sorted$status == "Died")
  ## check each status result
  failure_indices <- which(sorted$status %in% defs$eos_failure)

  death_df <- data.frame(
    globalrecordid = df$globalrecordid[1],
    event_death = FALSE,
    date_death = as.POSIXct(NA_character_)
  )

  # create death event data
  if (length(death_indices) > 0) {
    death_df$event_death <- TRUE
    death_df$date_death <- as.POSIXct(
      sorted$fudat[min(death_indices, na.rm = TRUE)]
    )
  }

  if (length(failure_indices) > 0) {
    earliest_failure_index <- min(failure_indices)
    # for earliest failure and return with date
    return(
      merge(
        data.frame(
          globalrecordid = df$globalrecordid[1],
          eos_outcome = factor(sorted$status[earliest_failure_index],
            levels = defs$eos_levels
          ),
          eos_date = sorted$fudat[earliest_failure_index]
        ),
        death_df,
        by = "globalrecordid"
      )
    )
  }

  final_fu <- df$trtendat[1]

  if (!all(is.na(df$fudat))) {
    final_fu <- max(df$fudat, na.rm = TRUE)
  }

  return(
    merge(
      data.frame(
        globalrecordid = df$globalrecordid[1],
        eos_outcome = factor("No TB",
          levels = defs$eos_levels
        ),
        eos_date = final_fu
      ),
      death_df,
      by = "globalrecordid"
    )
  )
}
