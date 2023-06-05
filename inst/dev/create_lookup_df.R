# generate lookup table from `vars` and `codes` CSV files
# in the mstr-app DB initData directory

suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(readr))

file_names <- list.files(
  here::here("inst/resources/lookup/")
) |>
  stringr::str_remove(".csv$")

lookup <- purrr::map(
  list.files(
    here::here("inst/resources/lookup/"),
    full.names = TRUE
  ),
  ~ readr::read_csv(.x)
) |> purrr::set_names(file_names)

lut <- lookup$vars |>
  filter(language == "EN") |>
  select(name, source_table) |>
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
  ))



usethis::use_data(lut,
  internal = TRUE,
  overwrite = TRUE
)
