create_eot_outcome_table <- function(df, missing_text, by = NULL) {
  stopifnot(
    is.data.frame(df),
    all(c("outcome", "tx_outcome") %in% names(df)),
    any(is.null(by), length(by) == 1)
  )

  outcome <- gtsummary::tbl_summary(
    data = df,
    include = "outcome",
    by = dplyr::all_of(by),
    label = list(
      outcome ~ "End of treatment outcome"
    ),
    missing_text = missing_text
  ) |>
    gtsummary::modify_header(label = "") |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::filter(!(variable %in% "outcome" & row_type %in% "label"))
    )

  # change name of successful outcome level for table
  original_levels <- levels(df$tx_outcome)
  index <- which(original_levels == "Successful")
  original_levels[index] <- "Treatment success"
  levels(df$tx_outcome) <- original_levels

  summary <- gtsummary::tbl_summary(
    data = df,
    include = "tx_outcome",
    by = dplyr::all_of(by),
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

  gtsummary::tbl_stack(
    list(
      outcome,
      summary
    ),
    quiet = TRUE
  )
}
