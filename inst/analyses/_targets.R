# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c(
    "tbmstr",
    "dplyr", "ggplot2", "readr", "cli",
    "gtsummary", "survival"
  ),
  imports = c("tbmstr"),
  format = "rds" # default storage format
  # Set other options as needed.
)

# Configure the backend of tar_make_clustermq() (recommended):
options(clustermq.scheduler = "multicore")

# Load the R scripts with your custom functions:
lapply(list.files("inst/analyses/steps", full.names = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

data_path <- here::here("data", "regional_prepared")
output_dir <- here::here("inst", "analyses", "output")
quality_file <- here::here(output_dir, "quality.html")

# Replace the target list below with your own:
list(
  tar_target(file, data_path, format = "file"),
  tar_target(raw, import(file)),
  tar_target(adjusted, manual_adjustments(raw)),
  tar_target(quality_report, create_quality_report(
    adjusted,
    raw,
    "quality.Rmd",
    output_dir
  )),
  tar_target(labelled, apply_all_labels(adjusted)),
  tar_target(include_outcomes, create_outcomes(labelled)),
  tar_target(prepared, prepare_baseline(include_outcomes,
    cohort = "treatment"
  )),
  tar_target(clean, relevel_vars(prepared)),
  tar_target(
    high_dose_list,
    create_high_dose_list(
      clean,
      list(output_dir = output_dir)
    ),
    format = "file"
  ),
  tar_target(conversion_cohort, create_conversion_cohort(clean)),
  tar_target(hiv_cohort, create_hiv_cohort(clean)),
  tar_target(failure_cohort, create_failure_cohort(clean)),
  tar_target(surv_objects, create_surv_objects(clean, hiv_cohort)),
  tar_target(tables, create_tables(
    clean,
    hiv_cohort,
    failure_cohort,
    surv_objects
  )),
  tar_target(plots, create_plots(surv_objects, hiv_cohort)),
  tar_target(html_output, render(
    tables,
    list(
      output_dir = output_dir,
      output_format = "html_document"
    )
  ), format = "file"),
  tar_target(docx_output, render(
    tables,
    list(
      output_dir = output_dir,
      output_format = "word_document"
    )
  ), format = "file"),
  tar_target(saved_plots, save_plots(
    plots,
    list(output_dir = output_dir)
  ), format = "file")
)
