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
  dd[which(dd$is_baseline_culture_positive), ]
}

create_failure_cohort <- function(dd) {
  dd[which(dd$outcome == "Failed"), ]
}
