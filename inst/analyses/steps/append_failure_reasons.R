append_failure_reasons <- function(file, lst) {
  reasons <- readxl::read_xlsx(file) |>
    dplyr::select(globalrecordid, failure_reasons)

  lst$baseline <- dplyr::left_join(
    lst$baseline,
    reasons,
    by = "globalrecordid"
  ) |>
    dplyr::mutate(failure_reasons = factor(failure_reasons,
      levels = c(
        "Failure to culture convert",
        "Culture reverted",
        "Acquired drug resistance",
        "Adverse drug reaction resulting in treatment change",
        "Insufficient treatment received in accepted time",
        "Treatment extended by clinician",
        "Other",
        "Clinical deterioration"
      )
    ))

  return(lst)
}
