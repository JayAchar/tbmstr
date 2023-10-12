#' Create conversion variables
#'
#' This function generates conversion variables including
#' a time at risk and an event status
#'
#' @param baseline  data frame of baseline participant characteristics
#' @param myco data frame of participant mycobacteriology results
#'
#' @return data frame with globalrecordid, cc_days and
#'   cc_event variables

create_conversion_variables <- function(baseline, myco) {
  # TODO: create `calculate_cc_date` variable to use myco data

  # confirm baseline culture result



  risk_end_date <- as.POSIXct(ifelse(
    is.na(baseline$convdat),
    baseline$trtendat,
    baseline$convdat
  ), tz = "UTC")

  cc_days <- create_cc_days(
    trtstdat = baseline$trtstdat,
    convdat = risk_end_date
  )

  cc_event <- ifelse(
    is.na(baseline$convdat),
    FALSE, TRUE
  )

  merge(
    baseline,
    data.frame(
      globalrecordid = baseline$globalrecordid,
      cc_days = cc_days,
      cc_event = cc_event
    ),
    by = "globalrecordid",
    all.x = TRUE
  )
}
