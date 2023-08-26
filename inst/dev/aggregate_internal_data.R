source(here::here("inst", "dev", "create_lookup_df.R"))
source(here::here("inst", "dev", "create_definitions.R"))


internal <- list(
  lut = lut,
  definitions = definitions
)

usethis::use_data(
  internal,
  internal = TRUE,
  overwrite = TRUE
)
