devtools::load_all()
library(flextable)
library(gtsummary)
library(officer)
library(survival)
library(ggplot2)
source(
  here::here("inst", "analyses", "tables.R")
)

data_path <- here::here("data", "regional_prepared")
output_dir <- here::here("inst", "analyses", "output")
quality_file <- here::here(output_dir, "quality.html")

run_analysis <- function(type = c("dev", "prod")) {
  type <- match.arg(type)
  # import data and apply labels
  raw <- import_data(
    parent_dir = data_path,
    multi_country = TRUE,
    apply_labels = FALSE,
    file_names = list(
      adverse = "adverse",
      baseline = "baseline",
      myco = "myco",
      dst = "dst"
    )
  )

  # check data
  # remove invalid records from baseline table
  raw$baseline <- remove_invalid_records(
    raw$baseline
  )

  raw <- apply_manual_adjustments(raw, internal$adjustments)

  if (FALSE) {
    # prepare data for quality check
    quality_data <- prepare_quality_data(
      study_data = list(
        baseline = raw$baseline,
        myco = raw$myco,
        adverse = raw$adverse,
        dst = raw$dst
      )
    )

    render_internal_rmd(
      input_file = "quality.Rmd",
      output_file = quality_file,
      param = list(
        data = quality_data,
        raw = raw
      )
    )
  }

  # mutate data
  labelled <- apply_all_labels(raw)
  ## add analysis variables
  prepared <- prepare_baseline(labelled,
    cohort = "treatment"
  )

  # remove withdrawn factor level from outcome
  prepared$baseline$outcome <- droplevels(
    prepared$baseline$outcome
  )

  relevel_vars <- list(
    prison = "No",
    alcohol = "No",
    prevtb = "No",
    covid = "No",
    hiv = "No",
    diab = "No",
    idu = "No",
    homeless = "No",
    cav = "No cavity",
    hcvab = "Seronegative",
    smear = "Negative"
  )

  prepared$baseline <- relevel_variables(prepared$baseline,
    config = relevel_vars
  )

  # create HIV cohort
  hiv_cohort <- prepared$baseline[which(prepared$baseline$hiv == "Yes"), ]

  # output table 1
  plots <- list()
  tables <- create_tables(prepared$baseline, 
                          hiv_cohort)

  # descriptive outcomes table
  #
  # ## outcomes stratified by HIV status


  # t10 <- gtsummary::tbl_uvregression(
  #   data = hiv_cohort,
  #   method = survival::coxph,
  #   y = survival::Surv(fail_days, event_fail),
  #   exponentiate = TRUE,
  #   include = c("art", "cd4"),
  #   label = hiv_labels
  # ) |>
  #   gtsummary::add_n(location = "label") |>
  #   gtsummary::add_nevent(location = "level")
  #
  # t11 <- gtsummary::tbl_uvregression(
  #   data = hiv_cohort,
  #   method = survival::coxph,
  #   y = survival::Surv(death_days, event_death),
  #   exponentiate = TRUE,
  #   include = c("art", "cd4"),
  #   label = hiv_labels
  # ) |>
  #   gtsummary::add_n(location = "label") |>
  #   gtsummary::add_nevent(location = "level")
  #
  # tables$st3 <- gtsummary::tbl_merge(
  #   list(t10, t11),
  #   tab_spanner = c("**Study failure**", "**Death**")
  # ) |> as_flex_table()
  #
  # # output table 2
  # t3 <- gtsummary::tbl_uvregression(
  #   data = prepared$baseline,
  #   label = labels,
  #   method = survival::coxph,
  #   y = survival::Surv(fail_days, event_fail),
  #   exponentiate = TRUE,
  #   include = all_of(covariates[!covariates == "cntry"])
  # ) |>
  #   gtsummary::add_n(location = "label") |>
  #   gtsummary::add_nevent(location = "level")
  #
  # # TODO: add random effects to account for clustering by country
  # mv_fail <- survival::coxph(
  #   survival::Surv(
  #     prepared$baseline$fail_days,
  #     prepared$baseline$event_fail
  #   ) ~ age + sex + bmi_group +
  #     hiv + prison + alcohol + prevtb + cav + hcvab + smear,
  #   data = prepared$baseline
  # )
  #
  # mv_fail_labels <- list(
  #   age ~ "Age (yrs)",
  #   sex ~ "Sex",
  #   cav ~ "X-ray cavities",
  #   bmi_group ~ "Body Mass Index (kg/m^2)",
  #   hiv ~ "HIV status",
  #   hcvab ~ "HCV Ab status",
  #   smear ~ "Baseline smear microscopy status",
  #   prison ~ "History of incarceration",
  #   alcohol ~ "Excess alcohol use",
  #   prevtb ~ "Previous TB episode"
  # )
  #
  # t4 <- mv_fail |>
  #   gtsummary::tbl_regression(
  #     exponentiate = TRUE,
  #     label = mv_fail_labels,
  #   )
  #
  # tables$mt3 <- gtsummary::tbl_merge(
  #   list(t3, t4),
  #   tab_spanner = c("**Crude**", "**Adjusted**")
  # ) |> as_flex_table()

  plots$p1 <- ggsurvfit::survfit2(
    survival::Surv(fail_days, event_fail) ~ 1,
    data = prepared$baseline
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
    data = prepared$baseline
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
    data = prepared$baseline
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
    ggplot(
      aes(
        x = art,
        y = cd4,
        group = art
      )
    ) +
    geom_boxplot(alpha = 0.1) +
    geom_jitter(aes(
      color = event_death,
      alpha = event_death
    ), width = 0.1) +
    scale_y_sqrt() +
    theme_linedraw() +
    labs(
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

  if (type == "prod") {
    ggsave(
      filename = "p1.png",
      path = output_dir,
      plot = p1,
      device = "png",
      width = 10,
      height = 7
    )

    ggsave(
      filename = "p2.png",
      path = output_dir,
      plot = p2,
      device = "png",
      width = 10,
      height = 7
    )

    ggsave(
      filename = "p3.png",
      path = output_dir,
      plot = p3,
      device = "png",
      width = 10,
      height = 7
    )

    ggsave(
      filename = "p4.png",
      path = output_dir,
      plot = p4,
      device = "png",
      width = 10,
      height = 7
    )

    ggsave(
      filename = "p5.png",
      path = output_dir,
      plot = p5,
      device = "png",
      width = 10,
      height = 7
    )
    tables_doc <- officer::read_docx() |>
      body_add_par("Main Table 1", style = "heading 1") |>
      body_add_flextable(value = tables$mt1) |>
      body_add_break() |>
      body_add_par("Main Table 2", style = "heading 1") |>
      body_add_flextable(value = tables$mt2) |>
      body_add_break() |>
      body_end_section_portrait() |>
      body_add_par("Main Table 3", style = "heading 1") |>
      body_add_par(
        "Crude and adjusted associations with unsuccessful study outcomes",
        style = "Normal"
      ) |>
      body_add_flextable(value = tables$mt3) |>
      body_add_break() |>
      body_end_section_landscape() |>
      body_add_par("Supplementary Table 1", style = "heading 1") |>
      body_add_flextable(value = tables$st1) |>
      body_add_break() |>
      body_add_par("Supplementary Table 2", style = "heading 1") |>
      body_add_par("HIV cohort treatment and baseline CD4 count", style = "Normal") |>
      body_add_flextable(value = tables$st2) |>
      body_add_break() |>
      body_end_section_portrait() |>
      body_add_par("Supplementary Table 3", style = "heading 1") |>
      body_add_flextable(value = tables$st3) |>
      body_add_break() |>
      body_end_section_landscape() |>
      print(tables_doc,
        target = here::here(output_dir, "tables.docx")
      )
  }

  output_format <- "html_document"
  if (type == "prod") {
    output_format <- "word_document"
  }

  rmarkdown::render(
    input = here::here("inst", "analyses", "dev.Rmd"),
    output_dir = output_dir,
    output_format = output_format,
    params = list(
      tables = tables
    )
  )
}
