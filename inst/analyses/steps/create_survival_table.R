create_survival_table <- function(surv,
                                  times = c(301, 484, 667)) {
  # TODO: the number of events and at-risk participants have now been
  # commented out - is this ok?

  # df <- summary(surv, times = times)

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

  # p1 <- gtsummary::tbl_survfit(
  #   surv,
  #   times = times,
  #   label_header = "**{time} days**"
  # ) |>
  #   gtsummary::as_gt()

  # surv_df <- data.frame(
  #   at_risk = df$n.risk,
  #   events = df$n.event
  # )
  #
  # surv_df$cum_events <- Reduce(sum, surv_df$events, accumulate = TRUE)
  #
  # surv_df <- surv_df[, c("cum_events", "at_risk")]
  # names(surv_df) <- c("Cumulative events", "At risk")
  #
  # Reduce(
  #   x = names(surv_df),
  #   f = function(tab, var_name) {
  #     gt::rows_add(
  #       tab,
  #       label = var_name,
  #       stat_1 = as.character(surv_df[1, var_name]),
  #       stat_2 = as.character(surv_df[2, var_name]),
  #       stat_3 = as.character(surv_df[3, var_name])
  #     )
  #   },
  #   init = p1
  # )
}
