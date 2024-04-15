#' Read in study data
#'
#' Read in study data which has been saved as CSV files
#' @param parent_dir Path to parent directory where
#' @param file_names Named list of file names to read in.
#'       Allowed file names are:
#' * adverse
#' * baseline
#' * dst
#' * myco
#'
#' @details
#' ## Required file directory structure
#'
#' ![](read_in_file_tree.png "Required file tree configuration")
#'
#' @importFrom readr read_csv
#' @importFrom cli cli_abort cli_alert_success
#'
#' @return A named list of named lists where each element is a data
#'  frame. The data frame names correspond to the names of elements
#'  provided in the **file_names** parameter

read_in <- function(parent_dir,
                    file_names) {
  if (dir.exists(parent_dir) == FALSE) {
    cli::cli_abort("Parent directory does not exist - try again")
  }
  allowed_files <- c(
    "baseline", "myco", "adverse", "dst", "adherence",
    "monitor", "change"
  )

  if (!is.list(file_names)) {
    cli::cli_abort("File names must be specified in a named list")
  }

  if (!all(names(file_names) %in% allowed_files)) {
    cli::cli_abort(
      "File names must be specfied with the following names: {allowed_files}"
    )
  }

  all_dirs <- list.dirs(parent_dir, recursive = FALSE)

  if (length(all_dirs) < 1) {
    cli::cli_abort("Parent directory is empty - try again")
  }

  all_file_paths <- lapply(
    all_dirs,
    \(dir) {
      vapply(
        file_names,
        \(file) {
          file.path(dir, paste0(file, ".csv"))
        }, character(1)
      )
    }
  )

  names(all_file_paths) <- list.dirs(parent_dir,
    full.names = FALSE,
    recursive = FALSE
  )

  files_exist <- vapply(unlist(all_file_paths), function(file_path) {
    file.exists(file_path)
  }, logical(1))

  if (any(files_exist) == FALSE) {
    cli::cli_abort("Requested file(s) does not exist")
  }


  dfs <- lapply(
    all_file_paths,
    \(dir) {
      lapply(
        dir,
        \(file) readr::read_csv(file, show_col_types = FALSE)
      )
    }
  )

  cli::cli_alert_success("Data sets successfully read: {length(dfs)}")

  return(dfs)
}
