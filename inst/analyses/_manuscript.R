# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c(
    "tbmstr", "officer", "flextable",
    "dplyr", "ggplot2", "readr", "cli",
    "gtsummary", "survival", "readxl"
  ),
  imports = c("tbmstr"),
  format = "rds" # default storage format
  # Set other options as needed.
)

# Configure the backend of tar_make_clustermq() (recommended):
options(clustermq.scheduler = "multicore")

# Load the R scripts with your custom functions:
lapply(list.files("inst/global-steps", full.names = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

data_dir <- here::here("data", "regional_prepared", "analysis_2023")
base_dir <- here::here("inst", "analyses")
templates_dir <- file.path(base_dir, "templates")
output_dir <- file.path(base_dir, "output")
adjustments_dir <- file.path(base_dir, "adjustments", "data")

dev_template <- file.path(templates_dir, "dev.Rmd")
success_template <- file.path(templates_dir, "short_success.Rmd")
withdrawal_template <- file.path(templates_dir, "withdrawn.Rmd")
follow_up_summary_template <- file.path(templates_dir, "follow_up.Rmd")
sensitivity_template <- file.path(templates_dir, "sensitivity.Rmd")

deaths_file <- file.path(
  adjustments_dir,
  "deaths.xlsx"
)
failure_reasons_file <- file.path(
  adjustments_dir,
  "comment-translations.xlsx"
)
quality_file <- file.path(output_dir, "quality.html")

## Tagging
git_branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
git_hash <- system("git rev-parse HEAD", intern = TRUE)

git <- list(
  branch = git_branch,
  hash = git_hash
)


## TEMPLATES
list(
  tar_target(file, data_dir, format = "file"),
  tar_target(file_failure_reasons,
    command = failure_reasons_file,
    format = "file"
  ),
  tar_target(file_success_template,
    command = success_template,
    format = "file"
  ),
  tar_target(adjustment_files,
    command = adjustments_dir,
    format = "file"
  ),
  tar_target(file_dev_template,
    command = dev_template,
    format = "file"
  ),
  tar_target(file_deaths_description,
    command = deaths_file,
    format = "file"
  ),
  tar_target(sensitivity_file,
    command = sensitivity_template,
    format = "file"
  ),

  # Withdrawals tabulated output

  tar_target(file_withdrawn_template,
    command = withdrawal_template,
    format = "file"
  ),
  tar_target(withdrawals_file, render_withdrawls(labelled,
    list(output_dir = output_dir),
    template = file_withdrawn_template
  )),

  # Follow-up tabulated output
  tar_target(file_follow_up,
    command = follow_up_summary_template,
    format = "file"
  ),
  tar_target(follow_up_file, render_follow_up(
    fu_cohort,
    list(
      output_dir = output_dir,
      end_date = as.POSIXct("2023-07-01", tz = "UTC")
    ),
    template = file_follow_up
  ), format = "file"),


  # Quality report
  tar_target(quality_report, create_quality_report(
    adjusted,
    raw,
    "quality.Rmd",
    output_dir
  )),

  # import and clean data
  tar_target(raw, import(file)),
  tar_target(adjustments, import_adjustments(adjustment_files)),
  tar_target(adjusted, apply_adjustments(raw, adjustments)),
  tar_target(
    death_descriptions,
    import_death_descriptions(file_deaths_description)
  ),
  tar_target(labelled, apply_all_labels(adjusted)),
  tar_target(include_outcomes, create_outcomes(labelled)),
  tar_target(prepared, prepare_baseline(include_outcomes,
    cohort = "treatment"
  )),
  tar_target(conversion, create_conversion_variables(
    baseline = include_outcomes$baseline,
    myco = include_outcomes$myco
  )),
  tar_target(add_failures, append_failure_reasons(
    file_failure_reasons,
    prepared
  )),
  tar_target(clean, relevel_vars(add_failures)),
  tar_target(
    high_dose_list,
    create_high_dose_list(
      clean,
      list(output_dir = output_dir)
    ),
    format = "file"
  ),
  tar_target(deaths, merge_death_descriptions(clean, death_descriptions)),
  tar_target(censored, apply_censoring(deaths)),
  tar_target(success_file, render_short_success(
    censored,
    list(
      output_dir = output_dir
    ),
    template = file_success_template
  ), format = "file"),
  tar_target(conversion_cohorts, create_conversion_cohort(conversion)),
  tar_target(hiv_cohort, create_hiv_cohort(censored)),
  tar_target(failure_cohort, create_failure_cohort(censored)),
  tar_target(fu_cohort, create_fu_cohort(censored)),
  tar_target(surv_objects, create_surv_objects(
    censored,
    hiv_cohort,
    conversion_cohorts,
    fu_cohort
  )),
  tar_target(
    text_objects, create_text_objects(
      pd = censored,
      fu = fu_cohort
    )
  ),
  tar_target(tables, create_tables(
    censored,
    hiv_cohort,
    failure_cohort,
    surv_objects,
    include_outcomes$who_outcomes
  )),
  tar_target(
    sensivity_analyses,
    create_sensitivity_analyses(
      censored
    )
  ),
  tar_target(plots, create_plots(
    surv_objects,
    hiv_cohort
  )),
  tar_target(reports, render(
    tables,
    plots,
    text = text_objects,
    git,
    list(
      output_dir = output_dir
    ),
    template = file_dev_template
  ), format = "file"),
  tar_target(
    sensitivity_analysis_report,
    render_sensitivity_analyses(
      list(
        calculated = sensivity_analyses,
        mv_surv = surv_objects$mv_fail
      ),
      config = list(
        output_dir = output_dir,
        git = git
      ), template = sensitivity_file
    )
  ),
  tar_target(saved_plots, save_plots(
    plots,
    list(output_dir = file.path(output_dir, "plots"))
  ), format = "file")
)
