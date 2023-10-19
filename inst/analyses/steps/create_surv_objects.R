create_surv_objects <- function(df, hiv_cohort, cc_cohort, fu_cohort) {
  so <- list()

  so$fail <- ggsurvfit::survfit2(
    survival::Surv(fail_days, event_fail) ~ 1,
    data = df
  )

  so$fu_fail <- ggsurvfit::survfit2(
    survival::Surv(fu_days, event_fail) ~ 1,
    data = fu_cohort
  )

  so$death <- ggsurvfit::survfit2(
    survival::Surv(
      death_days, event_death
    ) ~ 1,
    data = df
  )

  so$death_by_hiv <- ggsurvfit::survfit2(
    survival::Surv(
      death_days, event_death
    ) ~ hiv,
    data = df
  )

  so$hiv_death <- ggsurvfit::survfit2(
    survival::Surv(
      death_days, event_death
    ) ~ art,
    data = hiv_cohort
  )

  so$cc <- ggsurvfit::survfit2(
    survival::Surv(cc_days, cc_event) ~ 1,
    data = cc_cohort
  )

  so$mv_fail <- survival::coxph(
    survival::Surv(
      df$fail_days,
      df$event_fail
    ) ~ age + sex + bmi_group + homeless + idu + smok +
      hiv + prison + alcohol + prevtb + cav + hcvab + smear +
      hbgrd +
      survival::frailty(cntry, distribution = "gaussian"),
    data = df
  )

  print(
    survival::cox.zph(so$mv_fail)
  )

  return(so)
}
