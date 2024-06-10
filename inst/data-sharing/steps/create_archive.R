create_archive <- function(build_dir,
                           static_dir,
                           output_dir,
                           file_name = "mstr-regional.zip") {
  static_files <- list.files(file.path(
    static_dir
  ), full.names = TRUE, all.files = FALSE)

  compiled <- list.files(file.path(
    build_dir
  ), full.names = TRUE, all.files = FALSE)

  utils::zip(
    zipfile = file.path(output_dir, file_name),
    files = c(static_files, compiled),
    extras = "-jq"
  )

  # delete generated csv files
  # unlink(build_dir, recursive = TRUE)

  return(file.path(output_dir, file_name))
}


create_country_archives <- function(build_dir, output_dir, static_dir) {
  all_dirs <- list.dirs(build_dir)
  dirs <- all_dirs[
    !all_dirs %in% c(build_dir, file.path(build_dir, "regional"))
  ]

  all_names <- list.dirs(build_dir, full.names = FALSE)
  country_names <- all_names[
    !all_names %in% c("", "regional")
  ]

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
        build_dir = dirs[dir_index],
        output_dir = output_dir,
        static_dir = static_dir,
        file_name = fn
      )
    }
  )


  return(output_dir)
}
