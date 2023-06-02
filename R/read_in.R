#' Read in study data
#' 
#' Read in study data which has been saved as CSV files
#' @param dir_path directory path to study data files
#' @param file_names vector of file names to read in
#' 
#' @importFrom utils read.csv
#' @importFrom cli cli_abort cli_alert_success
#'
#' @return list of data frames
#' @export
#'

read_in <- function(dir_path, file_names = c("baseline", "myco")) {
  
  if (dir.exists(dir_path) == FALSE) {
    cli::cli_abort("Directory does not exist - try again")
  }
    
  all_file_paths <- lapply(file_names, function(name) {
    file.path(dir_path, paste0(name, ".csv"))
  })
  
  files_exist <- vapply(all_file_paths, function(file_path) {
    file.exists(file_path)
  }, logical(1))
  
  if (all(files_exist) == FALSE) {
    print(list.files(dir_path))
    cli::cli_abort("Requested file(s) does not exist")
  }
  
  dfs <- lapply(all_file_paths, function(fp) {
    read.csv(fp)
  })
  
  names(dfs) <- file_names
  
  cli::cli_alert_success("Files successfully read: {length(dfs)}")
  
  return(dfs)
  
}
