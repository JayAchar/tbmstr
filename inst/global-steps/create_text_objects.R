create_text_objects <- function(pd, fu) {
  pregnancy <- create_pregnancy_text(df = pd)
  follow_up <- create_follow_up_events_text(df = fu)
  return(
    list(
      pregnancy = pregnancy,
      follow_up = follow_up
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

create_follow_up_events_text <- function(df) {
  recurrence <- nrow(df[which(df$eos_outcome == "Recurrence" &
    df$event_fail == TRUE), ])

  death <- nrow(df[which(df$eos_outcome == "Died" &
    df$event_fail == TRUE), ])

  return(
    list(
      recurrence = recurrence,
      death = death
    )
  )
}
