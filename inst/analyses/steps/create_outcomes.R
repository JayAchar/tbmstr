calculate_follow_up_timepoint <- function(df, timepoint) {
  df$who_days <- timepoint

  df$fu_days <- diff_days(
    df$trtstdat,
    df$eos_date
  )

  df$who_outcome <- dplyr::case_when(
    df$tx_outcome == "Unsuccessful" ~ df$outcome,
    df$eos_outcome %in% c("Recurrence", "Died") &
      df$fu_days <= timepoint ~ df$eos_outcome,
    df$eos_outcome %in% c("Recurrence", "Died") &
      df$fu_days > timepoint ~ "TB free",
    df$eos_outcome == "No TB" & df$fu_days >= timepoint ~ "TB free",
    is.na(df$outcome) ~ NA_character_,
    .default = ("Not evaluated")
  )

  return(
    df[, c("globalrecordid", "who_outcome", "who_days")]
  )
}

create_outcomes <- function(df_lst) {
  # censor death_days to 9 tx months + 12 follow-up months
  # 273 * 1.1 is defined in the study protocol as the maximum allowable
  # treatment duration - 9 months + 10%
  max_follow_up <- 273 * 1.1 + 13 * 31

  df_lst$baseline <- create_eos_outcome(
    df_lst$baseline,
    max_follow_up
  )

  starter <- df_lst$baseline |>
    remove_withdrawn_subjects()

  starter$tx_outcome <- tbmstr:::create_binary_tx_outcome(starter$outcome)

  who_outcomes_lst <- lapply(
    X = c(273, 455, 638, max_follow_up),
    FUN = \(x) {
      calculate_follow_up_timepoint(starter, x)
    }
  )

  who_outcomes_df <- Reduce(rbind, who_outcomes_lst)
  who_outcomes_df$who_outcome <- factor(who_outcomes_df$who_outcome,
    levels = c(
      "TB free", "Died", "Failed", "Recurrence",
      "Lost to follow-up", "Not evaluated"
    ),
    labels = c(
      "TB free", "Died", "Treatment failure", "Recurrence",
      "Treatment LTFU", "Not evaluated"
    )
  )

  df_lst$who_outcomes <- who_outcomes_df

  return(df_lst)
}
