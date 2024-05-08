#' Create conversion variables
#'
#' This function generates conversion variables including
#' a time at risk and an event status
#'
#' @param baseline  data frame of baseline participant characteristics
#' @param myco data frame of participant mycobacteriology results
#' @export
#'
#' @return data frame with globalrecordid, cc_days and
#'   cc_event variables

create_conversion_variables <- function(baseline,
                                        myco) {
  valid_specimens <- list(
    liquid = list(
      type = "culqd",
      label = "lq"
    ),
    solid = list(
      type = "culsld",
      label = "sld"
    ),
    joint = list(
      type = c("culqd", "culsld"),
      label = "all"
    )
  )

  baseline_culture_status_lst <- lapply(
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

  conversion_lst <- lapply(
    X = valid_specimens,
    FUN = function(spec) {
      time_df <- create_cc_days(
        baseline = baseline,
        myco = myco,
        lab = TRUE,
        culture_type = spec$type,
        var_suffix = spec$label
      )

      event_var <- paste0("lab_cc_event_", spec$label)
      date_var <- paste0("lab_cc_date_", spec$label)
      days_var <- paste0("lab_cc_days_", spec$label)

      time_df[[event_var]] <- ifelse(
        is.na(time_df[[paste0("lab_cc_date_", spec$label)]]),
        FALSE, TRUE
      )
      augmented <- merge(
        time_df,
        baseline[, c(
          "globalrecordid",
          "trtstdat",
          "trtendat"
        )]
      )

      # impute end of treatment date if no culture conversion detected
      # defines end of risk time
      augmented[[date_var]] <- as.POSIXct(ifelse(is.na(augmented[[date_var]]),
        augmented$trtendat,
        augmented[[date_var]]
      ))

      augmented[[days_var]] <- ifelse(is.na(augmented[[days_var]]),
        diff_days(
          augmented$trtstdat,
          augmented$trtendat
        ), augmented[[days_var]]
      )

      augmented$trtendat <- NULL
      augmented$trtstdat <- NULL

      return(augmented)
    }
  )

  lab_conversion <- lapply(
    X = names(valid_specimens),
    FUN = function(spec_name) {
      merge(
        baseline_culture_status_lst[[spec_name]],
        conversion_lst[[spec_name]],
        by = "globalrecordid",
        all = TRUE
      )
    }
  ) |> setNames(names(valid_specimens))


  return(lab_conversion)
}
