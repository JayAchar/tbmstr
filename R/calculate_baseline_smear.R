#' Calculate baseline smear result
#'
#'
#'
#' @param baseline data frame containing unique subject ID and their
#' treatment initiation date
#' @param myco data frame containing a subject ID, test type, date of
#' speciment collection and the test result
#'
#' @return data frame with baseline data and additional smear factor variable
#'

calculate_baseline_smear <- function(baseline,
                                     myco) {
  # 1. Select key variables from baseline df
  bdf <- baseline[, c("globalrecordid", "trtstdat")]
  # 2. Select key variables from myco df
  mdf <- myco[, c("fkey", "datespecimen", "test_type", "result")]
  # 3. Filter myco by test_type == "afb1"
  smear_df <- mdf[which(mdf$test_type == "afb1"), ]
  # 4. Filter myco by result %in% c(1, 2)
  valid_results <- smear_df[which(smear_df$result %in% c(1, 2)), ]
  # 5. Merge myco with baseline
  merged <- merge(bdf,
    valid_results,
    by.x = "globalrecordid",
    by.y = "fkey"
  )

  merged$test_type <- NULL
  # 6. Remove specimens > 7d after treatment initiation
  ff <- merged[which(as.numeric(merged$datespecimen - merged$trtstdat) <= 7), ]
  # 7. Subtract specimen date from treatment start date - take absolute value
  ff$days <- abs(as.numeric(ff$trtstdat - ff$datespecimen))
  ff$datespecimen <- NULL
  # 8. Keep specimen closest to treatment initiation
  sp <- split(ff, ff$globalrecordid)
  min_lst <- lapply(
    sp,
    \(grp) {
      min_days <- which(grp[["days"]] == min(grp[["days"]]))
      grp[min_days, ]
    }
  )
  df <- Reduce(
    rbind,
    min_lst,
  )
  df$days <- NULL
  names(df)[names(df) == "result"] <- "afb1"
  df$smear <- apply_labels(df, "afb1")
  df$afb1 <- NULL
  df
  # 9. Ensure there's only one value per subject
  # 10. Merge baseline_smear with original baseline df
}
