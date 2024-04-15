attach_var_descriptions <- function(df, var_df) {
  df$description <- NULL

  keep_vars <- c("name", "original_name", "prompt", "data_type")
  vars <- var_df[!duplicated(var_df$name), keep_vars]

  merged <- merge(
    df,
    vars,
    by.x = "variable",
    by.y = "name",
    all.x = TRUE
  )

  names(merged)[which(names(merged) == "prompt")] <- "description"

  merged$description <- gsub(
    "^[A-Z][0-9]\\. ",
    "",
    merged$description
  )

  return(merged)
}
