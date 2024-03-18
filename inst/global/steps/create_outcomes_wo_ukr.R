create_outcomes_wo_ukr <- function(df) {
  tt <- list()
  wdf <- df[which(
    df$cntry != "Ukraine"
  ), ]
  tt$eot_outcome <- create_eot_outcome_table(wdf, "Missing")

  # create study failure survival object with exclusion
  fail_so <- ggsurvfit::survfit2(
    survival::Surv(eos_days, event_fail) ~ 1,
    data = wdf
  )
  tt$eos_outcome <- create_survival_table(fail_so)
  return(tt)
}
