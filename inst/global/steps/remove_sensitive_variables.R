remove_sensitive_variables <- function(lst) {
  rm_vars <- list(
    baseline = c(
      "recstatus", "uniquekey", "firstsavelogonname",
      "firstsavetime", "lastsavelogonname", "lastsavetime",
      "inl", "drnum", "icfdat",
      "bmi_group", "eos_outcome",
      "eos_date", "eos_days",
      "event_fail", "event_death",
      "smear", "age_grp",
      "empl_3grp", "cd4_grp",
      "cd4_4grp", "ast_alt_grd",
      "sm_fact", "ast_alt_bin",
      "prfneu_bin", "hb_bin",
      "creat_bin", "vis_bin",
      "cohort_bilevel"
    ),
    myco = c("recstatus", "uniquerowid", "uniquekey"),
    adherence = c("recstatus", "uniquerowid", "uniquekey"),
    change = c("recstatus", "uniquerowid", "uniquekey"),
    dst = c("recstatus", "uniquerowid", "uniquekey")
  )

  lapply(
    X = names(lst),
    FUN = function(df_name) {
      if (!df_name %in% names(rm_vars)) {
        return(NULL)
      }
      vars <- rm_vars[[df_name]]
      lst[[df_name]][vars] <- NULL
      lst[[df_name]]
    }
  ) |> setNames(names(lst))
}