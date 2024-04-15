save_dictionary <- function(df, path) {
  write.csv(df, path,
    row.names = FALSE
  )
}
