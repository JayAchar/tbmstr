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
  valid_results <- smear_df[which(
    smear_df$result %in% c(1, 2, "Positive", "Negative")
  ), ]
  # 5. Merge myco with baseline
  merged <- merge(bdf,
    valid_results,
    by.x = "globalrecordid",
    by.y = "fkey",
    all.x = TRUE
  )

  merged$test_type <- NULL
  # 6. Remove specimens > 7d after treatment initiation
  ff <- merged[which(
    difftime(merged$datespecimen, merged$trtstdat, units = "days") <= 7
  ), ]
  # 7. Subtract specimen date from treatment start date - take absolute value
  ff$days <- as.numeric(abs(
    difftime(ff$datespecimen, ff$trtstdat, units = "days")
  ))
  ff$datespecimen <- NULL
  # 8. Keep specimen closest to treatment initiation
  sp <- split(ff, ff$globalrecordid)
  # handle myco data frame with no rows
  if (length(sp) == 0) {
    baseline$afb1 <- NA
    baseline$smear <- apply_labels(baseline, "afb1")
    baseline$afb1 <- NULL
    return(baseline)
  }
  min_lst <- lapply(
    sp,
    \(grp) {
      min_days <- which(grp[["days"]] == min(grp[["days"]]))
      df <- grp[min_days, ]
      if (length(min_days) > 1) {
        if (length(unique(df$result)) == 1) {
          return(
            df[1, ]
          )
        }
        if (any(df$result %in% list(1, "Positive"))) {
          return(
            df[which(df$result %in% list(1, "Positive")), ]
          )
        }
      }
      df
    }
  )
  df <- Reduce(
    rbind,
    min_lst,
  )

  if (inherits(df$result, "numeric")) {
    names(df)[which(names(df) == "result")] <- "afb1"
    df$smear <- apply_labels(df, "afb1")
  } else {
    df$smear <- factor(df$result,
      levels = c("Positive", "Negative", "No result")
    )
  }
  # 10. Merge baseline_smear with original baseline df
  output <- merge(
    baseline,
    df[, c("globalrecordid", "smear")],
    by = "globalrecordid",
    all.x = TRUE
  )

  if (nrow(output) != nrow(baseline)) {
    cli::cli_abort("Output row count should equal input row count.")
  }

  return(output)
}
