create_tables <- function(pd, hiv_cohort, failed, surv_objects) {
  tables <- list()

  covariates <- c(
    "age", "sex", "cntry", "cav", "bmi_group",
    "hiv", "hcvab", "idu", "homeless", "empl",
    "smok", "sm_fact",
    "smear", "prison", "alcohol", "prevtb", "diab",
    "covid", "regimen", "ast_alt_grd",
    "prfneugrd", "hbgrd", "creatgrd",
    "visgrd"
  )

  labels <- list(
    age ~ "Age (yrs)",
    sex ~ "Sex",
    cntry ~ "Country",
    cav ~ "X-ray cavities",
    bmi_group ~ "Body Mass Index (kg/m^2)",
    hiv ~ "HIV status",
    hcvab ~ "HCV Ab status",
    smear ~ "Baseline smear microscopy status",
    prison ~ "History of incarceration",
    homeless ~ "Homeless",
    empl ~ "Employment status",
    smok ~ "Smoking history",
    sm_fact ~ "Smoking intensity",
    idu ~ "History of injecting drug use",
    alcohol ~ "Excess alcohol use",
    prevtb ~ "Previous TB episode",
    diab ~ "Diabetes",
    covid ~ "Baseline SARS-CoV2 status",
    regimen ~ "Treatment regimen",
    prfneugrd  ~ "Baseline peripheral neuropathy",
    hbgrd ~ "Baseline anaemia",
    creatgrd ~ "Baseline renal dysfunction",
    visgrd ~ "Baseline visual loss",
    ast_alt_grd ~ "Baseline elevated AST/ALT"
  )

  tables$mt1 <- gtsummary::tbl_summary(
    data = pd,
    include = all_of(covariates),
    label = labels,
    # TODO: Reorder variables
    # TODO: `cav` includes two unknown types - combine
    # TODO: Convert baseline smear to grade rather than positive/negative
  ) |> gtsummary::as_flex_table()

  # descriptive outcomes table
  tables$mt2 <- gtsummary::tbl_summary(
    data = pd,
    include = all_of(c("outcome", "eos_outcome")),
    label = list(
      outcome ~ "End of treatment outcome",
      eos_outcome ~ "End of study outcome"
    ),
  ) |> gtsummary::as_flex_table()

  hiv_labels <- list(
    art ~ "Receiving ART",
    artreg ~ "ART regimen",
    cd4 ~ "Baseline CD4 count",
    cd4_grp ~ "Baseline CD4 group"
  )

  ## outcomes stratified by HIV status
  tables$st1 <- gtsummary::tbl_summary(
    data = pd,
    by = "hiv",
    # TODO: combine Unknown HIV status with 'No'
    include = c("outcome", "eos_outcome"),
    label = list(
      outcome ~ "End of treatment outcome",
      eos_outcome ~ "End of study outcome"
    )
  ) |>
    modify_spanning_header(
      c("stat_1", "stat_2", "stat_3") ~ "**HIV status**"
    ) |>
    gtsummary::as_flex_table()

  hiv_labels <- list(
    artreg ~ "Baseline ART regimen",
    cd4 ~ "Baseline CD4 count",
    cd4_grp ~ "Baseline CD4 group"
  )

  tables$st2 <- gtsummary::tbl_summary(
    data = hiv_cohort,
    include = c("artreg", "cd4", "cd4_grp"),
    label = hiv_labels
  ) |> gtsummary::as_flex_table()

  t10 <- gtsummary::tbl_uvregression(
    data = hiv_cohort,
    method = survival::coxph,
    y = survival::Surv(fail_days, event_fail),
    exponentiate = TRUE,
    include = c("art", "cd4", "cd4_grp"),
    label = hiv_labels
  ) |>
    gtsummary::add_n(location = "label") |>
    gtsummary::add_nevent(location = "level")

  t11 <- gtsummary::tbl_uvregression(
    data = hiv_cohort,
    method = survival::coxph,
    y = survival::Surv(death_days, event_death),
    exponentiate = TRUE,
    include = c("art", "cd4", "cd4_grp"),
    label = hiv_labels
  ) |>
    gtsummary::add_n(location = "label") |>
    gtsummary::add_nevent(location = "level")

  tables$st3 <- gtsummary::tbl_merge(
    list(t10, t11),
    tab_spanner = c("**Study failure**", "**Death**")
  ) |> gtsummary::as_flex_table()

  # output table 2
  t3 <- gtsummary::tbl_uvregression(
    data = pd,
    label = labels,
    method = survival::coxph,
    y = survival::Surv(fail_days, event_fail),
    exponentiate = TRUE,
    include = all_of(covariates[!covariates == "cntry"])
  ) |>
    gtsummary::add_n(location = "label") |>
    gtsummary::add_nevent(location = "level")

  mv_fail_labels <- list(
    age ~ "Age (yrs)",
    sex ~ "Sex",
    cav ~ "X-ray cavities",
    bmi_group ~ "Body Mass Index (kg/m^2)",
    hiv ~ "HIV status",
    hcvab ~ "HCV Ab status",
    smear ~ "Baseline smear microscopy status",
    prison ~ "History of incarceration",
    alcohol ~ "Excess alcohol use",
    prevtb ~ "Previous TB episode"
  )

  t4 <- surv_objects$mv_fail |>
    gtsummary::tbl_regression(
      exponentiate = TRUE,
      label = mv_fail_labels,
    )

  tables$mt3 <- gtsummary::tbl_merge(
    list(t3, t4),
    tab_spanner = c("**Crude**", "**Adjusted**")
  ) |> gtsummary::as_flex_table()

  tables$cc_risk <- gtsummary::tbl_survfit(
    surv_objects$cc,
    times = c(30, 60, 90, 120, 150),
    reverse = TRUE,
    label_header = "**{time} days**"
  ) |>
gtsummary::as_flex_table()

failed$prtclviol <- droplevels(failed$prtclviol)

tables$failure_reasons <- gtsummary::tbl_summary(
  data = failed,
  include = "prtclviol",
  label = list(
    prtclviol ~ "Reasons for discontinuation"
  ),
  sort = list(
    everything() ~ "frequency"
  )) |>
  gtsummary::modify_header(label = "") |>
  gtsummary::as_flex_table()

  tables
}
