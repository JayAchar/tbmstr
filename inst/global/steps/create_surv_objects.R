create_surv_objects <- function(df, hiv_cohort, cc_cohorts, fu_cohort) {
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

  so$fail_by_regimen <- ggsurvfit::survfit2(
    survival::Surv(eos_days, event_fail) ~ regimen,
    data = df
  )

  so$fail_by_regimen <- ggsurvfit::survfit2(
    survival::Surv(eos_days, event_fail) ~ regimen,
    data = df
  )

  so$fu_fail <- ggsurvfit::survfit2(
    survival::Surv(fu_days, event_fail) ~ 1,
    data = fu_cohort
  )

  fu_tmp <- fu_cohort
  fu_tmp$end_fu <- fu_tmp$eos_outcome == "No TB"

  so$fu_ltfu <- ggsurvfit::survfit2(
    survival::Surv(fu_days, end_fu) ~ 1,
    data = fu_tmp
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

  so$cc <- lapply(
    X = cc_cohorts,
    FUN = function(cohort) {
      days_var <- names(cohort)[grep(
        "_days_",
        names(cohort)
      )]
      event_var <- names(cohort)[grep(
        "_event_",
        names(cohort)
      )]
      ggsurvfit::survfit2(
        survival::Surv(
          time = cohort[[days_var]],
          event = cohort[[event_var]]
        ) ~ 1
      )
    }
  ) |> setNames(names(cc_cohorts))


  so$mv_fail <- calculate_multivariable_model(df)

  return(so)
}
