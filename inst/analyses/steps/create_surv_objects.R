create_surv_objects <- function(df, hiv_cohort) {
  so <- list()

  so$fail <- ggsurvfit::survfit2(
    survival::Surv(fail_days, event_fail) ~ 1,
    data = df
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
    data = df
  )

  return(so)
}
