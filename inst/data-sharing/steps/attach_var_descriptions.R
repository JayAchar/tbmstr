attach_var_descriptions <- function(df, var_df) {
  keep_vars <- c("name", "original_name", "prompt")
  vars <- var_df[!duplicated(var_df$name), keep_vars]

  merged <- merge(
    df,
    vars,
    by.x = "variable",
    by.y = "name",
    all.x = TRUE
  )

  merged$updated_description <- ifelse(
    is.na(merged$description) | merged$description == "",
    merged$prompt,
    merged$description
  )

  merged$prompt <- NULL
  merged$description <- NULL

  names(merged)[which(names(merged) == "updated_description")] <- "description"

  merged$description <- gsub(
    "^[A-Z][0-9]\\. ",
    "",
    merged$description
  )

  return(merged)
}
