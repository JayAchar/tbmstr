#' Run data quality check
#'
#' @param data_path path to data directory
#' @param output_file path and name of output file - must be .html
#'
#' @return NULL - creates a HTML file as a side-effect
#' @export
#'
run_quality_check <- function(data_path, output_file) {
  raw <- read_in(data_path,
    file_names = list(
      baseline = "baseline",
      myco = "myco"
    )
  )

  render_internal_rmd(
    input_file = "baseline.Rmd",
    output_file = output_file,
    param = list(
      data = raw$baseline
    )
  )

  return(NULL)
}
