devtools::load_all()
suppressMessages(library(dplyr))
parent_path <- ""
output_path <- ""



raw <- import_data(
  parent_dir = parent_path,
  file_names = list(
    baseline = "baseline",
    myco = "myco",
    adverse = "adverse",
    dst = "dst"
  ),
  multi_country = TRUE,
  apply_labels = FALSE
)

set.seed(48)
final <- list()
final$baseline <- raw$baseline |>
  filter(recstatus == 1) |>
  filter(globalrecordid %in% sample(raw$baseline$globalrecordid, 200)) |>
  select(
    -cntry, -lastsavelogonname, -inl, -drnum, -dob,
    -firstsavelogonname
  ) |> 
  mutate(cntry = 1)

final$adverse <- raw$adverse |>
  filter(globalrecordid %in% final$baseline$globalrecordid) |>
  select(-aecomment)

final$myco <- raw$myco |>
  filter(fkey %in% final$baseline$globalrecordid)

final$dst <- raw$dst |>
  filter(fkey %in% final$baseline$globalrecordid)

invisible(lapply(
  names(final),
  \(df_name) {
    write.csv(
      x = final[[df_name]],
      file = glue::glue("{output_path}/dummy/{df_name}.csv")
    )
  }
))
