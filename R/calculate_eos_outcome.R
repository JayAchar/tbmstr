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

  # definitions from package data
  defs <- internal$definitions

  ## if treatment failure is found return early with
  ## failure result and date
  if (any(df$outcome %in% defs$eot_failure)) {
    return(
      data.frame(
        globalrecordid = df$globalrecordid[1],
        eos_outcome = factor(df$outcome[1],
          levels = internal$definitions$eos_levels
        ),
        eos_date = df$trtendat[1]
      )
    )
  }

  ## if treatment success
  ## sort data frame by fudat
  sorted <- df[order(df$fudat), ]
  ## check each status result
  earliest_failure_index <- min(
    which(sorted$status %in% defs$eos_failure)
  )
  # for earliest failure and return with date

  return(
    data.frame(
      globalrecordid = df$globalrecordid[1],
      eos_outcome = factor(sorted$status[earliest_failure_index],
        levels = internal$definitions$eos_levels
      ),
      eos_date = sorted$fudat[earliest_failure_index]
    )
  )
}
