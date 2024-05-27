create_survival_table <- function(surv,
                                  times = c(300, 480, 660)) {
  gtsummary::tbl_survfit(
    surv,
    times = times,
    label_header = "**{time} days**"
  ) |>
    gtsummary::modify_header(label = "") |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::filter(!(variable %in% "regimen" & row_type %in% "label"))
    )
}
