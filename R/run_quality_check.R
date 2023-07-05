#' Run data quality check
#'
#' @param data_path path to data directory
#' @param output_file path and name of output file - must be .html
#' @import pointblank
#'
#' @return NULL - creates a HTML file as a side-effect
#' @export
#'
run_quality_check <- function(data_path, output_file) {
  raw <- import_data(data_path,
    file_names = list(
      baseline = "baseline",
      myco = "myco",
      adverse = "adverse",
      dst = "dst"
    ),
    # applying labels would invalidate quality check report
    apply_labels = FALSE
  )

  # remove invalid records from baseline table
  raw$baseline <- remove_invalid_records(
    raw$baseline
  )
  # prepare data for quality check
  prepared_data <- prepare_quality_data(
    study_data = list(
      baseline = raw$baseline,
      myco = raw$myco,
      adverse = raw$adverse,
      dst = raw$dst
    )
  )

  render_internal_rmd(
    input_file = "quality.Rmd",
    output_file = output_file,
    param = list(
      data = prepared_data,
      raw = raw
    )
  )
}
