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

save_countries_to_csv <- function(country_lst, output_dir) {
  country_names <- names(country_lst)
  lapply(
    X = country_names,
    FUN = \(cname) {
      # create folder in output director
      dir.create(file.path(output_dir, cname))

      # run save_as_csv
      save_as_csv(country_lst[[cname]], file.path(output_dir, cname))
    }
  )
  return(output_dir)
}
