save_as_csv <- function(lst, output_dir, dir_name = NULL) {
  stopifnot(
    all(
      lapply(lst, is.data.frame)
    )
  )

  if (!is.null(dir_name)) {
    dir.create(file.path(output_dir, dir_name))
    adjusted_output_dir <- file.path(output_dir, dir_name)
  }

  lapply(
    X = seq_along(lst),
    FUN = function(i) {
      temp_dir <- output_dir
      if (exists("adjusted_output_dir")) {
        temp_dir <- adjusted_output_dir
      }
      write.csv(
        x = lst[[i]],
        file = file.path(
          temp_dir,
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
      # run save_as_csv
      save_as_csv(
        lst = country_lst[[cname]],
        output_dir = file.path(output_dir),
        dir_name = cname
      )
    }
  )
  return(output_dir)
}
