create_survival_table <- function(surv,
                                  times = c(300, 480, 660)) {
  # FIXME: change labels to months and add spanning header and change "Overall"
  # to "Cumulative probability"


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
