create_hiv_cohort <- function(dd) {
  dd[which(dd$hiv == "Yes"), ]
}
