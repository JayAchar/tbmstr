import <- function(path) {
  data <- import_data(
    parent_dir = path,
    multi_country = TRUE,
    apply_labels = FALSE,
    file_names = list(
      adverse = "adverse",
      baseline = "baseline",
      myco = "myco",
      dst = "dst",
      adherence = "adherence"
    )
  )

  data$baseline <- tbmstr:::remove_invalid_records(
    data$baseline
  )

  return(data)
}
