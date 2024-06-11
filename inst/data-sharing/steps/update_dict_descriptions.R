update_dict_descriptions <- function(dict, desc) {
  stopifnot(
    is.list(desc)
  )

  x <- Reduce(
    x = desc,
    f = \(init, dd) {
      filter <- which(init$table_name == dd$table_name &
        init$variable == dd$variable)
      if (length(filter) == 0) {
        stop(
          paste0(
            "update_dict_descriptions: ",
            dd$table_name, " ", dd$variable
          )
        )
      }
      init$description[filter] <- dd$description
      init
    },
    init = dict
  )
  return(x)
}
