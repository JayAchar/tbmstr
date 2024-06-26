# generate lookup table from `vars` and `codes` CSV files
# in the mstr-app DB initData directory

suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(readr))

lookup_dir <- here::here("inst/global/lookup/")

file_names <- list.files(
  lookup_dir
) |>
  stringr::str_remove(".csv$")

lookup <- purrr::map(
  list.files(
    lookup_dir,
    full.names = TRUE
  ),
  ~ readr::read_csv(.x)
) |> purrr::set_names(file_names)

lut <- lookup$vars |>
  filter(language == "EN") |>
  select(name, source_table, prompt) |>
  filter(!is.na(source_table)) |>
  left_join(
    lookup$codes |>
      filter(language == "EN") |>
      select(table, value, description),
    by = join_by(source_table == table),
    relationship = "many-to-many"
  ) |>
  select(-source_table) |>
  # consolidate saetype labels to remove duplicates
  mutate(name = if_else(
    name == "saetype1", "saetype", name
  )) |>
  filter(!stringr::str_detect(
    name,
    "^saetype[2-5]{1}$"
  )) |>
  # consolidate sae labels to remove duplicates
  mutate(name = if_else(
    name == "sae1", "sae", name
  )) |>
  filter(!stringr::str_detect(
    name,
    "^sae[2-5]{1}$"
  )) |>
  # consolidate aeterm labels to remove duplicates
  mutate(name = if_else(
    name == "aeterm1", "aeterm", name
  )) |>
  filter(!stringr::str_detect(
    name,
    "^aeterm[2-5]{1}$"
  )) |>
  # consolidate severity labels to remove duplicates
  mutate(name = if_else(
    name == "severity1", "severity", name
  )) |>
  filter(!stringr::str_detect(
    name,
    "^severity[2-5]{1}$"
  )) |>
  # consolidate aeoutcome labels to remove duplicates
  mutate(name = if_else(
    name == "aeoutcome1", "aeoutcome", name
  )) |>
  filter(!stringr::str_detect(
    name,
    "^aeoutcome[2-5]{1}$"
  )) |>
  # fix spelling errors
  mutate(
    description = if_else(description == "Reoccurance",
      "Recurrence", description
    )
  ) |>
  mutate(
    description = if_else(
      description == "Determined ineligible after enrollemnt",
      "Determined ineligible after enrollment", description
    )
  ) |>
  mutate(
    description = if_else(
      description == "One-sided", "Unilateral",
      description
    )
  ) |>
  mutate(
    description = if_else(
      description == "Two-sided", "Bilateral",
      description
    )
  )
