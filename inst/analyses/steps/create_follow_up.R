create_follow_up <- function(lst) {
  bdf <- lst$baseline

  # only select required variables
  keep <- names(bdf)[grepl(
    "^stat|^evldat",
    names(bdf)
  )]
  fdf <- bdf[, c("globalrecordid", keep)]

  eval_months <- c(3, 6, 9, 12)

  # create long data frame for each follow-up month
  month_lst <- lapply(
    eval_months,
    FUN = \(m) {
      stat_var <- paste0("stat", m)
      dat_var <- paste0("evldat", m)
      temp_df <- fdf[, c(
        "globalrecordid",
        stat_var, dat_var
      )]
      return(
        data.frame(
          globalrecordid = temp_df$globalrecordid,
          final_fu_month = m,
          final_fu_status = temp_df[[stat_var]],
          final_fu_date = temp_df[[dat_var]]
        )
      )
    }
  )

  # create data frame from monthly list
  full_df <- Reduce(rbind, month_lst)


  # remove missing and not evaluated
  is_invalid_status <- is.na(full_df$final_fu_status) |
    full_df$final_fu_status == "Not evaluated"
  filtered <- full_df[!is_invalid_status, ]

  # split by participant
  pps <- split(filtered, filtered$globalrecordid)

  final <- lapply(pps,
    FUN = \(p) {
      if (length(p$final_fu_date[!is.na(p$final_fu_date)]) == 0) {
        return(
          data.frame(
            globalrecordid = p$globalrecordid,
            final_fu_month = NA,
            final_fu_status = NA,
            final_fu_date = NA
          )
        )
      }
      p_df <- p[which(p$final_fu_date == max(p$final_fu_date, na.rm = TRUE)), ]
      if (nrow(p_df) > 1) {
        # return evaluation with the lowest month
        # when evaluation dates are the same
        return(p_df[order(p_df$final_fu_month), ][1, ])
      }
      return(p_df)
    }
  )

  final_df <- Reduce(rbind, final)
  # remove duplicated records
  rdf <- final_df[!duplicated(final_df$globalrecordid), ]

  mdf <- merge(
    bdf,
    rdf,
    by = "globalrecordid",
    all.x = TRUE
  )
  lst$baseline <- mdf
  return(lst)
}
