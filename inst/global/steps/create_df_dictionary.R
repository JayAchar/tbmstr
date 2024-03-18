create_df_dictionary <- function(df, df_name) {

  types <- list(
                character = "string",
                numeric = "number",
                POSIXct = "date"
  )

  var_meta <- lapply(
    X = names(df),
    FUN = function(var) {
      var_type <- class(df[[var]])
      if (length(var_type) > 1) {
        var_type <- var_type[[1]]
      }

      if (length(unique(df[[var]])) <= 10 || "factor" %in% var_type) {
        return(
          list(
            name = var,
            type = "categorical",
            description = "Some random variable description",
            values = unique(df[[var]])
          )
        )
      }

      return(
        list(
          name = var,
          type = types[[var_type]],
          description = "Some other random variable description"
        )
      )
    }
  )

  return(
    list(
      df_name = df_name,
      var_meta = var_meta
    )
  )
}

