create_plots <- function(pd, hiv_cohort) {
  plots <- list()


  plots$p1 <- ggsurvfit::survfit2(
    survival::Surv(fail_days, event_fail) ~ 1,
    data = pd
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to unsuccessful study outcome",
      x = "Time from treatment start (days)"
    )

  plots$p2 <- ggsurvfit::survfit2(
    survival::Surv(
      death_days, event_death
    ) ~ 1,
    data = pd
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to death",
      x = "Time from treatment start (days)"
    )

  plots$p3 <- ggsurvfit::survfit2(
    survival::Surv(
      death_days, event_death
    ) ~ hiv,
    data = pd
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to death by HIV status",
      x = "Time from treatment start (days)"
    )

  plots$p4 <- hiv_cohort |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = art,
        y = cd4,
        group = art
      )
    ) +
    ggplot2::geom_boxplot(alpha = 0.1) +
    ggplot2::geom_jitter(aes(
      color = event_death,
      alpha = event_death
    ), width = 0.1) +
    ggplot2::scale_y_sqrt() +
    ggplot2::theme_linedraw() +
    ggplot2::labs(
      title = "CD4 count and mortality by ART status",
      x = "On ART at baseline",
      y = "Baseline CD4 count (sqrt transformation)"
    )

  plots$p5 <- ggsurvfit::survfit2(
    survival::Surv(
      death_days, event_death
    ) ~ art,
    data = hiv_cohort
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to death by ART status",
      x = "Time from treatment start (days)"
    )

  plots$conversion <- ggsurvfit::survfit2(
    survival::Surv(cc_days, cc_event) ~ 1,
    data = pd
  ) |>
    ggsurvfit::ggsurvfit(type = "risk") +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to sputum culture conversion",
      x = "Time from treatment start (days)"
    )
  plots
}
