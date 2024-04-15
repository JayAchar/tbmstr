# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c(
    "tbmstr"
  ),
  imports = c("tbmstr"),
  format = "rds" # default storage format
  # Set other options as needed.
)

# Configure the backend of tar_make_clustermq() (recommended):
options(clustermq.scheduler = "multicore")

global_dir <- here::here("inst", "global")
data_dir <- here::here("data", "regional_prepared", "final_2023")
base_dir <- here::here("inst", "data-sharing")
steps_dir <- file.path(base_dir, "steps")
output_dir <- file.path(base_dir, "output")
adjustments_dir <- file.path(global_dir, "adjustments")
lookup_dir <- file.path(global_dir, "lookup")

# Load the R scripts with your custom functions:
lapply(list.files(file.path(global_dir, "steps"), full.names = TRUE), source)
lapply(list.files(steps_dir, full.names = TRUE), source)

list(
  tar_target(adjustment_files,
    command = adjustments_dir,
    format = "file"
  ),
  tar_target(var_df_file,
    command = file.path(lookup_dir, "vars.csv"),
    format = "file"
  ),
  tar_target(files, data_dir, format = "file"),
  tar_target(var_df, read.csv(var_df_file)),
  tar_target(raw, import(files)),
  tar_target(adjustments, import_adjustments(adjustment_files)),
  tar_target(adjusted, apply_adjustments(raw, adjustments)),
  tar_target(labelled, apply_all_labels(adjusted)),
  tar_target(prepared, prepare_baseline(labelled,
    cohort = "treatment"
  )),
  tar_target(filtered, remove_sensitive_variables(prepared)),
  tar_target(dictionary, create_dictionary(filtered, var_df)),
  tar_target(saved, save_dictionary(
    dictionary,
    file.path(output_dir, "dictionary.csv")
  ))
)
