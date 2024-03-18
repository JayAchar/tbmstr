create_dictionary <- function(lst) {
  dfs <- lapply(
    X = names(lst),
    FUN = \(df_name) {
      create_df_dictionary(
        lst[[df_name]],
        df_name
      )
    }
  )

  Reduce(
    f = rbind,
    x = dfs
  )
}
