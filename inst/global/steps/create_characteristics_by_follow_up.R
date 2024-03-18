create_characteristics_by_follow_up <- function(df) {
  df$is_ltfu <- ifelse(df$eos_days < 600, "LTFU", "Not LTFU")

  gtsummary::tbl_summary(
    data = df[which(df$tx_outcome == "Successful"), ],
    include = dplyr::all_of(c(
      "age", "sex", "cntry", "regimen",
      "hiv", "smear",
      "bmi_group", "smok", "hcvab",
      "cav", "alcohol", "hb_bin", "ast_alt_bin"
    )),
    by = "is_ltfu",
    missing_text = "Missing",
  ) |>
    gtsummary::add_p()
}
