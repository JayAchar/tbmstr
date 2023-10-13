create_hiv_cohort <- function(dd) {
  dd[which(dd$hiv == "Yes"), ]
}

create_conversion_cohort <- function(dd) {
  dd[which(dd$is_baseline_culture_positive), ]
}

create_failure_cohort <- function(dd) {
  dd[which(dd$outcome == "Failed"), ]
}
