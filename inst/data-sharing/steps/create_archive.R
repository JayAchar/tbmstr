create_archive <- function(output_dir, static_dir,
                           file_name = "mstr-regional-data.zip") {
  all_files <- list.files(file.path(
    output_dir
  ), full.names = TRUE, all.files = FALSE)

  all_csv <- all_files[grep("*.csv", all_files)]

  static_files <- list.files(file.path(
    static_dir
  ), full.names = TRUE, all.files = FALSE)

  utils::zip(
    zipfile = file.path(output_dir, file_name),
    files = c(all_files, static_files),
    extras = "-j"
  )

  # delete generated csv files
  lapply(
    all_csv, unlink
  )

  return(file.path(output_dir, file_name))
}


create_country_archives <- function(output_dir, static_dir) {
  all_dirs <- list.dirs(output_dir)
  dirs <- all_dirs[all_dirs != output_dir]

  all_names <- list.dirs(output_dir, full.names = FALSE)
  country_names <- all_names[all_names != ""]

  if (length(dirs) != 13) {
    stop("There should be 13 country-specific data directories")
  }

  lapply(
    X = seq_along(dirs),
    FUN = \(dir_index) {
      fn <- paste0(
        "mstr-",
        tolower(country_names[dir_index]),
        ".zip"
      )

      create_archive(
        dirs[dir_index], static_dir,
        file_name = fn
      )

      # move country archive to the output_dire
      zip_file <- list.files(dirs[dir_index], full.names = TRUE)
      file.rename(zip_file, file.path(dirname(dirname(zip_file)), fn))
    }
  )

  # delete country-specific directories
  lapply(dirs, file.remove)

  return(output_dir)
}
