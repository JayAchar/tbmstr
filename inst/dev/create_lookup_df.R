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
    here::here("inst/resources/lookup/"), full.names = TRUE
  ), 
  ~readr::read_csv(.x)
) |> purrr::set_names(file_names)

lut <- lookup$vars |> 
  filter(language == "EN") |> 
  select(name, source_table) |> 
  filter(!is.na(source_table)) |> 
  left_join(lookup$codes |> 
              filter(language == "EN") |> 
              select(table, value, description), 
            by = join_by(source_table == table),
            relationship = "many-to-many") |> 
  select(-source_table)

usethis::use_data(lut, internal = TRUE)

