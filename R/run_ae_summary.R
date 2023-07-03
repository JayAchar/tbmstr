#' Create Adverse Event summary table
#'
#' @inheritParams run_quality_check
#'
#' @return
#' @export
#'
run_ae_summary <- function(data_path, output_file) {
  raw <- import_data(data_path,
    file_names = list(
      baseline = "baseline",
      myco = "myco",
      adverse = "adverse",
      dst = "dst"
    )
  )

  # prepare AE summary data


  # output summary table with gtsummary
}
