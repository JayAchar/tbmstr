create_outcomes <- function(df_lst) {
  df_lst$baseline <- create_eos_outcome(df_lst$baseline)
  return(df_lst)
}
