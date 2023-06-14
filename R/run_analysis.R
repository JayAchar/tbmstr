#' Run analysis
#'
#' @param data_path path to data directory
#' @param output_dir path to output directory
#'
#' @return NULL - creates files in output directory
#' @export
#'
run_analysis <- function(data_path, output_dir) {
  raw <- read_in(data_path,
                 file_names = list(
                   baseline = "baseline",
                   myco = "myco",
                   adverse = "adverse",
                   dst = "dst"
                 )
  )
  
  # remove invalid records from baseline table
  raw$baseline <- remove_invalid_records(
    raw$baseline
  )
  
  # TODO prepare_analysis_data
    # select required variables
    # label all variables
    # calculate relevant variables - e.g. age group, BMI
    # output outcome and adverse event data frames in list
    # 
  
  # TODO output table 1
  
  
  # TODO calculate summary statistics for treatment outcome
  
  # TODO calculate summary statistics for adverse events
  
  # TODO output treatment outcomes by explanatory variable
  
  # TODO output adverse events by explanatory variable
  
  # TODO fit logistic regression model for treatment outcome
  
  # TODO output final fully adjusted model table
  
}
