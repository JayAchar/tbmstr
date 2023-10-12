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
  # confirm baseline culture result
  culture_positive_df <- has_positive_baseline_culture(baseline, myco)

  # add is_baseline_culture_positive variable
  baseline_positive <- merge(
    baseline,
    culture_positive_df,
    by = "globalrecordid",
    all.x = TRUE
  )

  risk_end_date <- as.POSIXct(ifelse(
    is.na(baseline$convdat),
    baseline$trtendat,
    baseline$convdat
  ), tz = "UTC")

  baseline_lab_cc <- create_cc_days(
    trtstdat = NULL,
    convdat = NULL,
    baseline = baseline_positive,
    myco = myco,
    lab = TRUE
  )

  # lab_cc_event <- ifelse(
  #   is.na(calculated_cc_days),
  #   FALSE, TRUE
  # )

  cc_days <- create_cc_days(
    trtstdat = baseline$trtstdat,
    convdat = risk_end_date
  )

  cc_event <- ifelse(
    is.na(baseline$convdat),
    FALSE, TRUE
  )

  merge(
    baseline_lab_cc,
    data.frame(
      globalrecordid = baseline$globalrecordid,
      cc_days = cc_days,
      cc_event = cc_event
    ),
    by = "globalrecordid",
    all.x = TRUE
  )
}
