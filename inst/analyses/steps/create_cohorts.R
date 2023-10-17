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
  cohort$cc_event[which(cohort$cc_days > 4 * 31)] <- FALSE
  cohort$cc_days[which(cohort$cc_days > 4 * 31)] <- 4 * 31
  return(cohort)
}

create_failure_cohort <- function(dd) {
  dd[which(dd$outcome == "Failed"), ]
}
