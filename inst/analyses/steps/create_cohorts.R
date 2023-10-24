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
  cohort <- dd[which(dd$is_baseline_culture_positive), ]
  # Failing to culture convert after 4 months of treatment is a
  # failure definition
  # TODO: this censoring should be moved to the function
  # where the variables are calculated if possible
  cohort$cc_event[which(cohort$cc_days > 4 * 31)] <- FALSE
  cohort$cc_days[which(cohort$cc_days > 4 * 31)] <- 4 * 31
  return(cohort)
}

create_failure_cohort <- function(dd) {
  dd[which(dd$outcome == "Failed"), ]
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
