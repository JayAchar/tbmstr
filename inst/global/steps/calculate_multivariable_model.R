calculate_multivariable_model <- function(df) {
  # filter data to complete case approach
  # error if >10% rows are lost
  keep <- c(
    "age_grp",
    "sex",
    "hiv",
    "cntry",
    "alcohol",
    "cav",
    "hb_bin",
    "ast_alt_bin",
    "empl_3grp",
    "smok",
    "bmi_group",
    "creat_bin",
    "homeless",
    "idu",
    "prison",
    "prevtb",
    "hcvab",
    "smear",
    "hb_bin",
    "ast_alt_bin",
    "eos_days",
    "prfneu_bin",
    "event_fail"
  )

  narrow <- df[, keep]
  complete <- complete.cases(narrow)
  mod_data <- narrow[complete, ]

  if (nrow(mod_data) / nrow(df) <= 0.9) {
    cli::cli_abort("Complete case anlaysis loses >= 10% of participants")
  }
  so <- survival::Surv(
    mod_data$eos_days,
    mod_data$event_fail
  )

  # starting point - include age group  and HIV status a priori
  null <- survival::coxph(
    so ~ age_grp + hiv + survival::frailty(cntry, distribution = "gaussian"),
    data = mod_data
  )

  # scope for stepper function
  full <- survival::coxph(
    so ~ age_grp + sex + bmi_group + homeless + idu + smok + empl_3grp +
      hiv + prison + alcohol + prevtb + cav + hcvab + smear +
      hb_bin + creat_bin + ast_alt_bin + prfneu_bin +
      survival::frailty(cntry, distribution = "gaussian"),
    data = mod_data
  )

  mod <- step(null,
    direction = "forward",
    scope = formula(full)
  )

  mv <- survival::coxph(
    so ~ age_grp + hiv + alcohol + cav + hb_bin +
      ast_alt_bin + empl_3grp + smok +
      bmi_group + smear +
      survival::frailty(cntry, distribution = "gaussian"),
    data = mod_data
  )

  # check that the optimal model hasn't changed
  mvterms <- labels(terms(formula(mv)))
  modterms <- labels(terms(formula(mod)))
  has_equal_terms <- setequal(mvterms, modterms)
  if (!has_equal_terms) {
    cli::cli_abort("The optimal MV model has not been selected")
  }

  mv_lr_pvals <- calculate_lr_statistic(full, mod_data)

  # FIXME: this function should probably only output one model
  list(
    full = full, mv = mv,
    mv_lr_pvals = mv_lr_pvals
  )
}
