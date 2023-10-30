create_surv_objects <- function(df, hiv_cohort, cc_cohort, fu_cohort) {
  # check for duplicate participants
  has_duplicates <- any(duplicated(df$globalrecordid))
  if (has_duplicates) {
    cli::cli_abort("Duplicate rows have been detected")
  }

  so <- list()

  so$fail <- ggsurvfit::survfit2(
    survival::Surv(eos_days, event_fail) ~ 1,
    data = df
  )

  so$fu_fail <- ggsurvfit::survfit2(
    survival::Surv(fu_days, event_fail) ~ 1,
    data = fu_cohort
  )

  so$death <- ggsurvfit::survfit2(
    survival::Surv(
      eos_days, event_death
    ) ~ 1,
    data = df
  )

  so$death_by_hiv <- ggsurvfit::survfit2(
    survival::Surv(
      eos_days, event_death
    ) ~ hiv,
    data = df
  )

  so$hiv_death <- ggsurvfit::survfit2(
    survival::Surv(
      eos_days, event_death
    ) ~ art,
    data = hiv_cohort
  )

  so$cc <- ggsurvfit::survfit2(
    survival::Surv(cc_days, cc_event) ~ 1,
    data = cc_cohort
  )


  so$mv_fail <- calculate_multivariable_model(df)

  return(so)
}
