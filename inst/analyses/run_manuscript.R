devtools::install(quick = TRUE, build = TRUE, upgrade = "never")
Sys.setenv(TAR_PROJECT = "manuscript")
setwd(here::here())
targets::tar_make()
