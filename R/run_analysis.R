#' Run analysis
#'
#' @inherit import_data
#' @param data_only boolean flag to describe whether to only return
#'   prepared data list
#' @return NULL - creates files in output directory
#' @export
#'
run_analysis <- function(parent_dir,
                         file_names = list(
                           baseline = "baseline",
                           myco = "myco",
                           adverse = "adverse",
                           dst = "dst"
                         ),
                         multi_country = FALSE,
                         apply_labels = TRUE,
                         data_only = FALSE) {
  raw <- import_data(
    parent_dir = parent_dir,
    file_names = file_names,
    apply_labels = apply_labels,
    multi_country = multi_country
  )
  # remove invalid records from baseline table
  if (!"baseline" %in% names(file_names)) {
    return(raw)
  }

  baselined <- prepare_baseline(
    df_list = raw
  )
  # select required variables
  # calculate relevant variables - e.g. age group, BMI
  # output outcome and adverse event data frames in list
  #

  if (data_only) {
    return(baselined)
  }

  cli::cli_abort("Change `data_only` to TRUE to only \\
                        return prepared data")
}
