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

create_conversion_variables <- function(baseline,
                                        myco) {
  risk_end_date <- as.POSIXct(ifelse(
    is.na(baseline$convdat),
    baseline$trtendat,
    baseline$convdat
  ), tz = "UTC")

  valid_specimens <- list(
    liquid = list(
      type = "culq",
      label = "lq"
    ),
    solid = list(
      type = "culsld",
      label = "sld"
    ),
    joint = list(
      type = c("culq", "culsld"),
      label = "all"
    )
  )

  baseline_culture_status <- lapply(
    FUN = function(spec) {
      has_positive_baseline_culture(
        baseline = baseline,
        myco = myco,
        culture_type = spec$type,
        var_suffix = spec$label
      )
    },
    X = valid_specimens
  )

  baseline_status_df <- Reduce(
    x = baseline_culture_status,
    f = function(acc, df) {
      merge(acc, df,
        by = "globalrecordid"
      )
    }
  )

  # add is_baseline_culture_positive variable
  baseline_positive <- merge(
    baseline,
    baseline_status_df,
    by = "globalrecordid",
    all.x = TRUE
  )


  # TODO: if is.na(lab_cc_days) does that mean there was no culture conversion??

  baseline_lab_cc <- create_cc_days(
    baseline = baseline_positive,
    myco = myco,
    lab = TRUE
  )

  # lab_cc_event <- ifelse(
  #   is.na(calculated_cc_days),
  #   FALSE, TRUE
  # )

  # FIXME: convdat variable should not be used to define culture conversion
  # it might be used to share information with authors, but should
  # not be included in the final analysis

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
