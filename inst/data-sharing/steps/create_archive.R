create_archive <- function(output_dir, static_dir) {
  output_filename <- "mstr-data-sharing.zip"

  all_files <- list.files(file.path(
    output_dir
  ), full.names = TRUE, all.files = FALSE)

  static_files <- list.files(file.path(
    static_dir
  ), full.names = TRUE, all.files = FALSE)

  utils::zip(
    zipfile = file.path(output_dir, output_filename),
    files = c(all_files, static_files),
    extras = "-j"
  )

  return(file.path(output_dir, output_filename))
}
