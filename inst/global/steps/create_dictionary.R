create_dictionary <- function(lst, var_df) {
  stopifnot(
    is.data.frame(var_df)
  )

  dfs <- lapply(
    X = names(lst),
    FUN = \(df_name) {
      create_df_dictionary(
        lst[[df_name]],
        df_name
      )
    }
  )

  tbl <- Reduce(
    f = rbind,
    x = dfs
  )

  described <- attach_var_descriptions(tbl, var_df)

  # attach custom descriptions
  described <- update_dict_descriptions(
    dict = described,
    desc = updated_descriptions
  )

  output_col_order <- c(
    "table_name", "variable", "values", "format",
    "description"
  )

  described[order(described$table_name), output_col_order]
}
