create_hiv_tables <- function(full, hiv_cohort, who_fu, labels) {
  missing_text <- "Missing"

  hiv_who_fu <- who_fu[which(
    who_fu$globalrecordid %in% hiv_cohort$globalrecordid
  ), ]

  outcome <- gtsummary::tbl_summary(
    data = full[which(!is.na(full$hiv)), ],
    by = "hiv",
    include = "outcome",
    label = list(
      outcome ~ "End of treatment outcome"
    ),
    missing_text = missing_text
  ) |>
    gtsummary::modify_header(label = "") |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::filter(
          !(variable %in% "outcome" & row_type %in% "label")
        )
    )

  original_levels <- levels(full$tx_outcome)
  index <- which(original_levels == "Successful")
  original_levels[index] <- "Treatment success"
  levels(full$tx_outcome) <- original_levels

  summary <- gtsummary::tbl_summary(
    data = full[which(!is.na(full$hiv)), ],
    include = "tx_outcome",
    by = dplyr::all_of("hiv"),
    label = list(
      tx_outcome ~ "Binary treatment outcome"
    ),
    missing_text = missing_text
  ) |>
    gtsummary::modify_header(label = "") |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::filter(!(variable %in% "tx_outcome" & row_type %in% "label"))
    ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::filter(variable %in% "tx_outcome" & label == "Treatment success")
    )

  outcome_by_status <- gtsummary::tbl_stack(
    list(
      outcome,
      summary
    ),
    quiet = TRUE
  ) |>
    gtsummary::modify_spanning_header(
      c("stat_1", "stat_2") ~ "**HIV status**"
    )

  hiv_outcomes <- gtsummary::tbl_summary(
    data = hiv_cohort,
    include = c(
      "art", "artreg", "cd4",
      "cd4_4grp",
      "cpt"
    ),
    label = labels$hiv,
    missing_text = missing_text
  )


  failure <- gtsummary::tbl_uvregression(
    data = hiv_cohort,
    method = survival::coxph,
    y = survival::Surv(eos_days, event_fail),
    exponentiate = TRUE,
    include = c(
      "art", "cd4_4grp",
      "cpt"
    ),
    label = labels$hiv
  ) |>
    gtsummary::add_n(location = "label") |>
    gtsummary::add_nevent(location = "level")

  death <- gtsummary::tbl_uvregression(
    data = hiv_cohort,
    method = survival::coxph,
    y = survival::Surv(eos_days, event_death),
    exponentiate = TRUE,
    include = c("art", "cd4_4grp", "cpt"),
    label = labels$hiv
  ) |>
    gtsummary::add_n(location = "label") |>
    gtsummary::add_nevent(location = "level")

  failure_death <- gtsummary::tbl_merge(
    list(failure, death),
    tab_spanner = c("**Study failure**", "**Death**")
  )

  follow_up <- create_follow_up_table(hiv_who_fu)

  return(list(
    outcome_by_status = outcome_by_status,
    hiv_outcomes = hiv_outcomes,
    failure_death = failure_death,
    failure = failure,
    death = death,
    follow_up = follow_up
  ))
}
