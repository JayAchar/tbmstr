# df <- combine_ae_grades(clean, "hbgrd", "hb_bin")
combine_ae_grades <- function(df, target, result) {
  df[[result]] <- as.character(df[[target]])

  df[[result]][df[[result]] != "None"] <- "Yes"
  df[[result]][df[[result]] == "None"] <- "No"

  df[[result]] <- factor(
    df[[result]],
    c("No", "Yes")
  )
  return(df)
}
