create_plots <- function(surv_objects, hiv_cohort) {
  plots <- list()

  plots$p1 <- surv_objects$fail |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to unsuccessful study outcome",
      x = "Days from treatment start",
      y = "Disease-free survival probability"
    )

  plots$fail_by_regimen <- surv_objects$fail_by_regimen |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to unsuccessful study outcome by regimen",
      x = "Days from treatment start",
      y = "Disease-free survival probability"
    )

  plots$p2 <- surv_objects$death |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to death",
      x = "Time from treatment start (days)"
    )

  plots$p3 <- surv_objects$death_by_hiv |>
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

  plots$p5 <- surv_objects$hiv_death |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Kaplan Meier estimates for time to death by ART status",
      x = "Time from treatment start (days)"
    )

  plots$conversion <- lapply(
    X = surv_objects$cc,
    FUN = function(spec) {
      spec |>
        ggsurvfit::ggsurvfit(type = "risk") +
        ggsurvfit::add_confidence_interval() +
        ggsurvfit::add_risktable() +
        ggplot2::coord_cartesian(ylim = c(0, 1)) +
        ggplot2::labs(
          title =
            "Kaplan Meier estimates for time to sputum culture conversion",
          x = "Time from treatment start (days)"
        )
    }
  ) |>
    setNames(names(surv_objects$cc))

  plots$fu_survival <- surv_objects$fu_fail |>
    ggsurvfit::ggsurvfit() +
    ggplot2::labs(
      x = "Days",
      y = "Probability of successful outcome"
    ) + ggsurvfit::add_confidence_interval() +
    ggsurvfit::add_risktable() +
    ggplot2::coord_cartesian(ylim = c(0, 1))


  plots
}
