devtools::install()
library(targets)
setwd(here::here())
tar_make(script = here::here("inst", "analyses", "_targets.R"))

tar_load("clean")

clean$art |> table()
