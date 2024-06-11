remove_sensitive_variables <- function(lst) {
  rm_vars <- list(
    baseline = c(
      "recstatus", "uniquekey", "firstsavelogonname",
      "firstsavetime", "lastsavelogonname", "lastsavetime",
      "inl", "drnum", "icfdat", "dob", "subjid",
      "bmi_group", "eos_outcome",
      "eos_date", "eos_days",
      "event_fail", "event_death",
      "smear", "age_grp",
      "empl_3grp", "cd4_grp",
      "cd4_4grp", "ast_alt_grd",
      "sm_fact", "ast_alt_bin",
      "prfneu_bin", "hb_bin",
      "creat_bin", "vis_bin",
      "cohort_bilevel", "cohort"
    ),
    myco = c("recstatus", "uniquekey"),
    adherence = c("recstatus", "uniquekey"),
    change = c("recstatus", "uniquekey"),
    dst = c("recstatus", "uniquekey"),
    monitor = c("recstatus", "uniquekey")
  )

  lapply(
    X = names(lst),
    FUN = function(df_name) {
      if (!df_name %in% names(rm_vars)) {
        return(lst[[df_name]])
      }
      vars <- rm_vars[[df_name]]
      lst[[df_name]][vars] <- NULL
      lst[[df_name]]
    }
  ) |> setNames(names(lst))
}
