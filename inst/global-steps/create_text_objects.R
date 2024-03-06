create_text_objects <- function(pd) {
  pregnancy <- create_pregnancy_text(df = pd)
  return(
    list(
      pregnancy = pregnancy
    )
  )
}

create_pregnancy_text <- function(df, dp = 1) {
  prg_results <- df[
    which(
      df$sex == "Female" &
        df$age >= 15 &
        df$age < 50
    ), "preg",
  ]

  # number of pregnancy test results
  valid_prg_results <- sum(!is.na(prg_results))

  valid_pct <- round(
    valid_prg_results / length(prg_results) * 100,
    dp
  )

  pos_prg_results <- sum(prg_results == "Yes", na.rm = TRUE)

  pct <- round(
    pos_prg_results / valid_prg_results * 100,
    dp
  )

  glue::glue(
    "Of {length(prg_results)} women aged between 15-49, ",
    "{valid_prg_results} ({valid_pct}%) had a baseline pregnancy test recorded ",
    "of which {pos_prg_results} ({pct}%) were positive."
  )
}
