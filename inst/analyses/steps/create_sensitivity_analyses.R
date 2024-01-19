create_sensitivity_analyses <- function(censored) {
  characteristics_by_fu <- create_characteristics_by_follow_up(censored)
  exclude_ukr <- create_outcomes_wo_ukr(censored)
  list(
    characteristics_by_fu = characteristics_by_fu,
    exclude_ukr = exclude_ukr
  )
}
