ONE_YEAR <- 60 * 60 * 24 * 365

summarise_country_follow_up <- function(df, end_date) {
  eligible <- nrow(df)

  early_exit <- df$eos_outcome %in% c("Died", "Recurrence")
  attended_12m <- df$stat12 %in% "No TB"

  attended <- df[
    which(early_exit | attended_12m),
  ] |> nrow()

  future <- nrow(df[which(
    !early_exit & !attended_12m &
      df$trtendat + ONE_YEAR > end_date
  ), ])

  missed <- eligible - attended - future

  attended_pct <- format(attended / eligible * 100, digits = 2)
  missed_pct <- format(missed / eligible * 100, digits = 2)
  future_pct <- format(future / eligible * 100, digits = 2)

  return(
    data.frame(
      country = unique(df$cntry),
      eligible = eligible,
      attended = glue::glue("{attended} ({attended_pct}%)"),
      missed = glue::glue("{missed} ({missed_pct}%)"),
      future = glue::glue("{future} ({future_pct}%)")
    )
  )
}


summarise_follow_up <- function(df, end_date = as.POSIXct("2023-07-01")) {
  country_names <- unique(df$cntry)

  countries <- lapply(
    X = country_names,
    FUN = \(n) {
      cntry_cohort <- df[which(df$cntry == n), ]
      summarise_country_follow_up(
        cntry_cohort,
        end_date
      )
    }
  )

  country_df <- Reduce(rbind, countries)
  country_df$country <- as.character(country_df$country)
  country_df[order(country_df$country), ]
}
