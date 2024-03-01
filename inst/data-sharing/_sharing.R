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

# Load the R scripts with your custom functions:
lapply(list.files("inst/global-steps", full.names = TRUE), source)

data_dir <- here::here("data", "regional_prepared", "final_2023")
base_dir <- here::here("inst", "data-sharing")
output_dir <- file.path(base_dir, "output")
adjustments_dir <- file.path(base_dir, "adjustments", "data")

list(
  tar_target(files, data_dir, format = "file"),
  # FIXME: the import file needs to be extended to all CSV files - MSTR-33
  # FIXME: Lithuania data isn't available
  tar_target(raw, import(files)),
  # TODO: the adjustments directory needs to be moved to a shared location
  tar_target(adjustments, import_adjustments(adjustment_files)),
)
