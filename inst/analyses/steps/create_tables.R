create_tables <- function(pd, hiv_cohort, failed, surv_objects, who_outcomes) {
  tables <- list()

  missing_text <- "Missing"
  labels <- create_table_labels()

  covariates <- c(
    "age",
    "age_grp",
    "sex",
    "bmi_group",
    "cntry",
    "cohort_bilevel",
    "idu",
    "homeless",
    "empl_3grp",
    "prison",
    "smok",
    "alcohol",
    "hiv",
    "hcvab",
    "diab",
    "covid",
    "prevtb",
    "cav",
    "smear",
    "regimen",
    "ast_alt_grd",
    "prfneugrd",
    "hbgrd",
    "creatgrd",
    "visgrd",
    "ast_alt_bin",
    "prfneu_bin",
    "hb_bin",
    "creat_bin",
    "vis_bin"
  )

  types <- list(
    age ~ "continuous",
    age_grp ~ "categorical",
    hiv ~ "categorical",
    idu ~ "categorical",
    cohort_bilevel ~ "categorical",
    homeless ~ "categorical",
    smok ~ "categorical",
    prison ~ "categorical",
    diab ~ "categorical",
    alcohol ~ "categorical",
    prevtb ~ "categorical",
    covid ~ "categorical"
  )

  pd <- pd[, c(
    "eos_date", "eos_outcome",
    "outcome", "eos_days", "event_fail",
    "stat12", "tx_outcome", "death_cause_category",
    covariates
  )]

  tables$tx_description <- gtsummary::tbl_summary(
    data = pd,
    include = dplyr::all_of(covariates) & !dplyr::ends_with("_bin"),
    label = labels$tx_desc,
    type = types,
    missing_text = missing_text,
  )

  tables$tx_description_by_regimen <- gtsummary::tbl_summary(
    data = pd,
    include = dplyr::all_of(covariates) & !dplyr::ends_with("_bin") &
      !dplyr::matches("regimen"),
    label = labels$tx_desc,
    type = types,
    by = "regimen",
    missing_text = missing_text,
  )

  tables$tx_outcomes <- create_eot_outcome_table(pd, missing_text)

  tables$tx_outcomes_by_regimen <- create_eot_outcome_table(pd, missing_text,
    by = "regimen"
  )

  ## outcomes stratified by HIV status
  tables$hiv <- create_hiv_tables(pd, hiv_cohort,
    who_outcomes,
    labels = labels
  )

  # output table 2
  # FIXME: there are two MV models tabulated here - the full version and
  # a restricted version based on AIC. Must choose which one to keep for the
  # final analysis
  tables$mv <- list()
  tables$mv$crude <- gtsummary::tbl_uvregression(
    data = pd,
    label = labels$uni,
    method = survival::coxph,
    y = survival::Surv(eos_days, event_fail),
    exponentiate = TRUE,
    include = dplyr::all_of(covariates) &
      !dplyr::ends_with("grd") &
      !dplyr::matches("cntry") &
      !dplyr::matches("^age$") &
      !dplyr::matches("^cohort_bilevel$")
  ) |>
    gtsummary::add_n(location = "label") |>
    gtsummary::add_nevent(location = "level")



  tables$mv$adjusted <- surv_objects$mv_fail$full |>
    gtsummary::tbl_regression(
      exponentiate = TRUE,
      label = labels$mv_all,
    )

  tables$mv$full <- gtsummary::tbl_merge(
    list(
      tables$mv$crude,
      tables$mv$adjusted
    ),
    tab_spanner = c("**Crude**", "**Adjusted**")
  ) |> gtsummary::as_flex_table()

  tables$cc_risk <- lapply(
    X = surv_objects$cc,
    FUN = function(spec) {
      time_tab <- gtsummary::tbl_survfit(
        spec,
        times = c(30, 60, 90, 120),
        reverse = TRUE,
        label_header = "**{time} days**"
      )

      median_tab <- gtsummary::tbl_survfit(
        spec,
        prob = 0.5,
        label_header = "**Median, days**"
      )
      gtsummary::tbl_merge(
        list(
          median_tab,
          time_tab
        ),
        tab_spanner = FALSE
      )
    }
  ) |>
    setNames(names(surv_objects$cc))

  failed$prtclviol <- droplevels(failed$prtclviol)

  tables$failure_reasons <- gtsummary::tbl_summary(
    data = failed,
    include = "failure_reasons",
    label = list(
      failure_reasons ~ "Reasons for treatment failure"
    ),
    sort = list(
      everything() ~ "frequency"
    )
  ) |>
    gtsummary::modify_header(label = "") |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::filter(
          !(variable %in% "failure_reasons" & row_type %in% "label")
        )
    )

  full_fu <- pd[which((!is.na(pd$stat12)) | pd$tx_outcome == "Unsuccessful" |
    pd$eos_outcome != "No TB"), ]

  tables$full_fu <- gtsummary::tbl_summary(
    data = full_fu,
    include = dplyr::all_of(c("outcome", "eos_outcome")),
    label = list(
      outcome ~ "End of treatment outcome",
      eos_outcome ~ "End of study outcome"
    ),
    missing_text = missing_text
  ) |>
    gtsummary::as_flex_table()

  tables$who_fu_outcomes <- create_follow_up_table(who_outcomes)

  tables$fail_survival <- surv_objects$fail |>
    create_survival_table()

  tables$fail_survival_by_regimen <- surv_objects$fail_by_regimen |>
    create_survival_table()

  tables$fu_survival <- surv_objects$fu_fail |>
    gtsummary::tbl_survfit(
      times = c(90, 180, 270, 360),
      label_header = "**{time} days**"
    )


  tables$death_description <- gtsummary::tbl_summary(
    data = pd[which(pd$eos_outcome == "Died"), ],
    include = "death_cause_category",
    label = list(
      death_cause_category ~ "Cause of death"
    ),
    sort = list(
      everything() ~ "frequency"
    ),
    missing_text = missing_text
  ) |>
    gtsummary::modify_header(label = "") |>
    gtsummary::as_flex_table()

  tables
}
