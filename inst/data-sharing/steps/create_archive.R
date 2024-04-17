create_archive <- function(dir) {
  output_filename <- "mstr-data-sharing.zip"

  all_files <- list.files(file.path(
    dir,
    "output"
  ), full.names = TRUE, all.files = FALSE)

  static_files <- list.files(file.path(
    dir, "static"
  ), full.names = TRUE, all.files = FALSE)

  utils::zip(
    zipfile = file.path(dir, "output", output_filename),
    files = c(all_files, static_files),
    extras = "-j"
  )

  return(file.path(dir, "output", output_filename))
}
