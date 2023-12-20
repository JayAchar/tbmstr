create_hiv_cohort <- function(dd) {
  dd <- dd[which(dd$hiv == "Yes"), ]

  # relabel ART regimen to None if patient is
  # not receiving ART
  dd$artreg <- ifelse(
    dd$art == "Yes" | is.na(dd$art),
    as.character(dd$artreg), "None"
  )

  dd$artreg <- factor(dd$artreg,
    levels = c(
      "With Dolutegravir",
      "With Efavirenz",
      "Other", "None"
    )
  )

  return(dd)
}

create_conversion_cohort <- function(dd) {
  lapply(
    X = dd,
    FUN = function(spec) {
      # find boolean variable
      bool_var <- names(spec)[grep(
        "^is_baseline_culture_positive",
        names(spec)
      )]
      event_var <- names(spec)[grep(
        "_event_",
        names(spec)
      )]
      days_var <- names(spec)[grep(
        "_days_",
        names(spec)
      )]

      cohort <- spec[which(spec[[bool_var]]), ]
      cohort[[event_var]][which(cohort[[days_var]] > 4 * 31)] <- FALSE
      cohort[[days_var]][which(cohort[[days_var]] > 4 * 31)] <- 4 * 31
      return(cohort)
    }
  ) |> setNames(names(dd))
}

create_failure_cohort <- function(dd) {
  df <- dd[which(dd$outcome == "Failed"), ]
  df$failure_reasons <- droplevels(df$failure_reasons)
  return(df)
}

create_fu_cohort <- function(dd) {
  max_fu <- 12 * 31
  cohort <- dd[which(dd$tx_outcome == "Successful"), ]
  # fu event = event_fail
  cohort$fu_days <- diff_days(
    cohort$trtendat,
    cohort$eos_date
  )

  # censor
  cohort$event_fail[which(cohort$fu_days > max_fu)] <- FALSE
  cohort$fu_days[which(cohort$fu_days > max_fu)] <- max_fu

  # fu event = event_fail
  return(cohort)
}
