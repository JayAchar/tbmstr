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

      description <- ""

      if (sum(duplicated(df[[var]])) == 0) {
        description <- "Primary Key"
      }


      if (length(unique(df[[var]])) <= 10 || "factor" %in% var_type) {
        values <- unique(df[[var]])
        if (length(values) > 1) {
          values <- values[which(!is.na(values))]
        }
        return(
          data.frame(
            table_name = df_name,
            variable = var,
            format = "categorical",
            description = description,
            values = values
          )
        )
      }


      return(
        data.frame(
          table_name = df_name,
          variable = var,
          format = types[[var_type]],
          description = description,
          values = ""
        )
      )
    }
  )
  Reduce(
    x = var_meta,
    f = rbind
  )
}
