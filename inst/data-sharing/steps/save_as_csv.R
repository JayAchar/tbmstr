save_as_csv <- function(lst, output_dir) {
  stopifnot(
    all(
      lapply(lst, is.data.frame)
    )
  )

  lapply(
    X = seq_along(lst),
    FUN = function(i) {
      write.csv(
        x = lst[[i]],
        file = file.path(
          output_dir,
          paste0(
            names(lst)[i],
            ".csv"
          )
        ),
        fileEncoding = "UTF-8"
      )
    }
  )

  return(output_dir)
}
