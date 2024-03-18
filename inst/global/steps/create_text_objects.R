create_text_objects <- function(pd, fu, surv) {
  pregnancy <- create_pregnancy_text(df = pd)
  follow_up <- create_follow_up_events_text(df = fu)
  fu_ltfu <- create_fu_desc_text(
    surv$fu_ltfu,
    c(0, 180, 330, 365)
  )
  return(
    list(
      pregnancy = pregnancy,
      follow_up = follow_up,
      fu_ltfu = fu_ltfu
    )
  )
}

create_fu_desc_text <- function(surv, time_points) {
  sdf <- summary(surv, times = time_points)

  df <- data.frame(
    time = sdf$time,
    survival = sdf$surv,
    n_risk = sdf$n.risk,
    ci_low = sdf$lower,
    ci_high = sdf$upper,
    events = sdf$n.event
  )

  day_labels <- lapply(time_points, \(tp) paste0("d", tp))

  tp_survival <- lapply(
    X = time_points,
    FUN = function(tp) {
      surv <- df[which(df$time == tp), ]
      pct <- surv[, "survival"]
      n_risk <- surv[, "n_risk"]
      events <- surv[, "events"]
      ci <- list(
        l = round(surv[, "ci_low"] * 100, 1),
        u = round(surv[, "ci_high"] * 100, 1)
      )
      list(
        pct = round(pct * 100, 1),
        n_risk = n_risk,
        ci = ci,
        events = events
      )
    }
  ) |> setNames(day_labels)

  return(list(
    stats = tp_survival,
    total_at_risk = sdf$n
  ))
}

create_pregnancy_text <- function(df, dp = 1) {
  prg_results <- df[
    which(
      df$sex == "Female" &
        df$age >= 15 &
        df$age < 50
    ), "preg",
  ]

  # number of pregnancy test results
  valid_prg_results <- sum(!is.na(prg_results))

  valid_pct <- round(
    valid_prg_results / length(prg_results) * 100,
    dp
  )

  pos_prg_results <- sum(prg_results == "Yes", na.rm = TRUE)

  pct <- round(
    pos_prg_results / valid_prg_results * 100,
    dp
  )

  glue::glue(
    "Of {length(prg_results)} women aged between 15-49, ",
    "{valid_prg_results} ({valid_pct}%) had a baseline pregnancy test recorded ",
    "of which {pos_prg_results} ({pct}%) were positive."
  )
}

create_follow_up_events_text <- function(df) {
  recurrence <- nrow(df[which(df$eos_outcome == "Recurrence" &
    df$event_fail == TRUE), ])

  death <- nrow(df[which(df$eos_outcome == "Died" &
    df$event_fail == TRUE), ])

  return(
    list(
      recurrence = recurrence,
      death = death
    )
  )
}
