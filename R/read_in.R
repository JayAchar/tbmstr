#' Read in study data
#'
#' Read in study data which has been saved as CSV files
#' @param dir_path Directory path to study data files
#' @param file_names Named list of file names to read in.
#'       Allowed file names are:
#' * adverse
#' * baseline
#' * myco
#'
#' @importFrom utils read.csv
#' @importFrom cli cli_abort cli_alert_success
#'
#' @return A named list of data frames where the names correspond to
#'        the names of elements provided in the **file_names** parameter
#' @export

read_in <- function(dir_path, file_names) {
  if (dir.exists(dir_path) == FALSE) {
    cli::cli_abort("Directory does not exist - try again")
  }
  allowed_files <- c("baseline", "myco", "adverse")

  if (!is.list(file_names)) {
    cli::cli_abort("File names must be specified in a named list")
  }

  if (!all(names(file_names) %in% allowed_files)) {
    cli::cli_abort(
      "File names must be specfied with the following names: {allowed_names}"
    )
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

  names(dfs) <- names(file_names)

  cli::cli_alert_success("Files successfully read: {length(dfs)}")

  return(dfs)
}
