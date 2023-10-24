create_tables <- function(pd, hiv_cohort, failed, surv_objects) {
  tables <- list()

  missing_text <- "Missing"
  labels <- create_table_labels()

  covariates <- c(
    "age_grp",
    "sex",
    "bmi_group",
    "cntry",

    "idu",
    "homeless",
    "empl_3grp",
    "prison",
    "smok",
    "alcohol",
    "sm_fact",

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
    "visgrd"
  )


    types <- list(
                  age_grp ~ "categorical",
                  hiv ~ "categorical",
                  idu ~  "categorical",
                  homeless ~ "categorical",
                  smok ~ "categorical",
                  prison ~ "categorical",
                  diab ~ "categorical",
                  alcohol ~ "categorical",
                  prevtb ~ "categorical",
                  covid ~ "categorical"
    )

  pd <- pd[, c("eos_date", "eos_outcome",
               "outcome", "eos_days", "event_fail",
               "stat12", "tx_outcome",
                covariates)]

    tables$tx_description <- gtsummary::tbl_summary(
      data = pd,
      include = dplyr::all_of(covariates),
      label = labels$tx_desc,
      type = types,
      missing_text = missing_text,
    ) |> gtsummary::as_flex_table()

    # descriptive outcomes table
    tables$tx_outcomes <- gtsummary::tbl_summary(
      data = pd,
      include = dplyr::all_of(c("outcome", "eos_outcome")),
      label = list(
        outcome ~ "End of treatment outcome",
        eos_outcome ~ "End of study outcome"
      ),
      missing_text = missing_text
    ) |> gtsummary::as_flex_table()

    ## outcomes stratified by HIV status
    tables$tx_outcomes_by_hiv <- gtsummary::tbl_summary(
      data = pd[which(!is.na(pd$hiv)), ],
      by = "hiv",
      include = c("outcome", "eos_outcome"),
      label = list(
        outcome ~ "End of treatment outcome",
        eos_outcome ~ "End of study outcome"
      ),
      missing_text = missing_text
    ) |>
      gtsummary::modify_spanning_header(
        c("stat_1", "stat_2") ~ "**HIV status**"
      ) |>
      gtsummary::as_flex_table()


    tables$hiv_outcomes <- gtsummary::tbl_summary(
      data = hiv_cohort,
      include = c("art", "artreg", "cd4",
                  "cd4_4grp",
                  "cpt"),
      label = labels$hiv,
      missing_text = missing_text
    ) |> gtsummary::as_flex_table()


    t10 <- gtsummary::tbl_uvregression(
      data = hiv_cohort,
      method = survival::coxph,
      y = survival::Surv(eos_days, event_fail),
      exponentiate = TRUE,
      include = c("art", "cd4_4grp",
                  "cpt"),
      label = labels$hiv
    ) |>
      gtsummary::add_n(location = "label") |>
      gtsummary::add_nevent(location = "level")

    t11 <- gtsummary::tbl_uvregression(
      data = hiv_cohort,
      method = survival::coxph,
      y = survival::Surv(eos_days, event_death),
      exponentiate = TRUE,
      include = c("art", "cd4_4grp", "cpt"),
      label = labels$hiv
    ) |>
      gtsummary::add_n(location = "label") |>
      gtsummary::add_nevent(location = "level")

    tables$hiv_failure_death <- gtsummary::tbl_merge(
      list(t10, t11),
      tab_spanner = c("**Study failure**", "**Death**")
    ) |> gtsummary::as_flex_table()

    # output table 2
    t3 <- gtsummary::tbl_uvregression(
      data = pd,
      label = labels$tx_desc,
      method = survival::coxph,
      y = survival::Surv(eos_days, event_fail),
      exponentiate = TRUE,
      include = dplyr::all_of(covariates[!covariates == "cntry"])
    ) |>
      gtsummary::add_n(location = "label") |>
      gtsummary::add_nevent(location = "level")


    t4 <- surv_objects$mv_fail |>
      gtsummary::tbl_regression(
        exponentiate = TRUE,
        label = labels$mv,
      )

    tables$tx_mv_failure <- gtsummary::tbl_merge(
      list(t3, t4),
      tab_spanner = c("**Crude**", "**Adjusted**")
    ) |> gtsummary::as_flex_table()

    tables$cc_risk <- gtsummary::tbl_survfit(
      surv_objects$cc,
      times = c(30, 60, 90, 120),
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

  full_fu <- pd[which((!is.na(pd$stat12)) | pd$tx_outcome == "Unsuccessful" | 
                      pd$eos_outcome != "No TB"), ]

    tables$full_fu <- gtsummary::tbl_summary(
      data = full_fu,
      include = dplyr::all_of(c("outcome", "eos_outcome")),
      label = list(
        outcome ~ "End of treatment outcome",
        eos_outcome ~ "End of study outcome"
      ),
      missing_text = missing_text) |>
gtsummary::as_flex_table()

tables$fu_survival <- surv_objects$fu_fail |>
  gtsummary::tbl_survfit(
    times = c(90, 180, 270, 360),
    label_header = "**{time} days**"
  ) |>
gtsummary::as_flex_table()

    tables
}
