save_dictionary <- function(df, build_dir) {
  all_dirs <- list.dirs(build_dir)
  dirs <- all_dirs[
    all_dirs != build_dir
  ]

  lapply(
    X = dirs,
    FUN = \(dir) {
      path <- file.path(dir, "dictionary.csv")
      write.csv(df, path,
        row.names = FALSE
      )
    }
  )

  return(build_dir)
}
