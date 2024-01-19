create_sensitivity_analyses <- function(censored) {
  characteristics_by_fu <- create_characteristics_by_follow_up(censored)
  list(
    characteristics_by_fu = characteristics_by_fu
  )
}
