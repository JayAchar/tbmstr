check_outcomes <- function(lst) {
  is_baseline_confirmed <- check_baseline_lab_status(
    baseline = lst$baseline,
    myco = lst$myco
  )

  culture_status <- check_terminal_negative_cultures(
    baseline = lst$baseline,
    myco = lst$myco
  )

  outcomes <- merge(
    lst$baseline[, c("globalrecordid", "outcome")],
    culture_status,
    all.x = TRUE,
    by = "globalrecordid"
  )

  binary_df <- merge(
    outcomes,
    is_baseline_confirmed,
    all.x = TRUE,
    by = "globalrecordid"
  )

  binary_df$is_cured <- binary_df$has_cultures & binary_df$is_baseline_positive

  baseline <- merge(
    lst$baseline,
    binary_df[, c("globalrecordid", "is_cured")],
    all.x = TRUE,
    by = "globalrecordid"
  )

  # FIXME: the outcome variable is changed to a numeric variable here
  baseline$outcome <- ifelse(!is.na(baseline$is_cured),
    baseline$is_cured,
    baseline$outcome
  )

  baseline$is_cured <- NULL

  lst$baseline <- baseline

  return(lst)
}
