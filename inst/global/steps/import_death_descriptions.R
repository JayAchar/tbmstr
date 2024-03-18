import_death_descriptions <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  descriptions <- lapply(
    sheets,
    FUN = \(sheet) {
      readxl::read_xlsx(file_path, sheet = sheet)
    }
  ) |> setNames(sheets)

  sae <- descriptions$sae_deaths[, c("globalrecordid", "category", "detail")]
  tx_deaths <- descriptions$death_descriptions[, c(
    "globalrecordid",
    "category", "detail"
  )]
  tx_deaths <- tx_deaths[!is.na(tx_deaths$category), ]
  deaths <- rbind(sae, tx_deaths)
  names(deaths) <- c(
    "globalrecordid", "death_cause_category",
    "death_cause_detail"
  )

  deaths$death_cause_category[which(
    deaths$death_cause_category == "No reason"
  )] <- NA_character_

  # remove duplicate row
  deaths <- deaths[!duplicated(deaths), ]

  if (any(duplicated(deaths$globalrecordid))) {
    cli::cli_abort("Duplicated rows detected")
  }
  deaths
}

merge_death_descriptions <- function(df, deaths) {
  merge(
    df,
    deaths,
    by = "globalrecordid",
    all.x = TRUE
  )
}
