import <- function(path) {
  data <- tbmstr::import_data(
    parent_dir = path,
    multi_country = TRUE,
    apply_labels = FALSE,
    file_names = list(
      adverse = "adverse",
      baseline = "baseline",
      myco = "myco",
      dst = "dst"
    )
  )

  data$baseline <- tbmstr:::remove_invalid_records(
    data$baseline
  )

  return(data)
}
