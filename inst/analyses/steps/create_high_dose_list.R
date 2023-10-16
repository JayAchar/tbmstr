create_high_dose_list <- function(dd, config) {
  final <- prepare_high_dose_list(dd)
  write.csv(final,
    file = file.path(config$output_dir, "high_dose.csv")
  )
}

prepare_high_dose_list <- function(dd) {
  keep <- c(
    "globalrecordid",
    "cntry",
    "drnum",
    "trtstdat",
    "trtendat",
    "totaldose",
    "trtprtcl",
    "outcome"
  )
  high_dose <- dd[, keep]

  high_dose$trtendat <- as.POSIXct(high_dose$trtendat,
    tz = "UTC"
  )

  high_dose$trt_days <- as.numeric(
    difftime(high_dose$trtendat,
      high_dose$trtstdat,
      units = "days"
    )
  )

  high_doses <- which(high_dose$totaldose >= 300)
  high_days <- which(high_dose$trt_days >= 300)

  keep_rows <- unique(c(high_doses, high_days))

  high_dose[keep_rows, ]
}
