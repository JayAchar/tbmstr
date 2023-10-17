create_outcomes <- function(df_lst) {
  # censor death_days to 9 tx months + 12 follow-up months
  # 273 * 1.1 is defined in the study protocol as the maximum allowable
  # treatment duration - 9 months + 10%
  max_follow_up <- 273 * 1.1 + 13 * 31

  df_lst$baseline <- create_eos_outcome(
    df_lst$baseline,
    max_follow_up
  )
  return(df_lst)
}
