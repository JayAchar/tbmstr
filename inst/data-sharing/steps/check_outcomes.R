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

  merge(
    outcomes,
    is_baseline_confirmed,
    all.x = TRUE,
    by = "globalrecordid"
  )

  # FIXME: none of the logic in this function is applied to the main line of
  # analysis

  return(lst)
}
