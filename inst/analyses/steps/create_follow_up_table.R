create_follow_up_table <- function(df) {
  # calculate number of timepoints
  timepoints <- unique(df$who_days)

  not_evals <- lapply(
    X = timepoints,
    FUN = \(x) {
      sum(df$who_outcome[
        which(df$who_days == x)
      ] %in% "Not evaluated")
    }
  )

  filtered <- df[which(df$who_outcome != "Not evaluated"), ]
  filtered$who_outcome <- droplevels(filtered$who_outcome)

  gtsummary::tbl_summary(
    filtered,
    by = "who_days",
    include = "who_outcome",
    missing_text = "Missing",
    label = list(
      who_outcome ~ ""
    )
  ) |>
    gtsummary::modify_spanning_header(
      c("stat_1", "stat_2", "stat_3") ~ "**Follow-up timepoint**"
    ) |>
    gtsummary::modify_header(
      label = "",
      update = list(
        stat_1 ~ "**End of treatment**
        N = {n}",
        stat_2 ~ "**6 months**
        N = {n}",
        stat_3 ~ "**12 months**
        N = {n}"
      )
    ) |>
    gtsummary::modify_footnote(
      update = list(
        stat_1 ~ glue::glue("Not evaluated: {count}",
          count = not_evals[[1]]
        ),
        stat_2 ~ glue::glue("Not evaluated: {count}",
          count = not_evals[[2]]
        ),
        stat_3 ~ glue::glue("Not evaluated: {count}",
          count = not_evals[[3]]
        )
      )
    )
}
