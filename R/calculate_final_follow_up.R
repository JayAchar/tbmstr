#' Calculate final follow up
#'
#' Calculate the final follow up date, month and status for
#' participants who successfully completed treatment
#'
#' @param df data frame of all participants' baseline characteristics
#' @param eval_months evaluation months
#'
#' @return
#'
calculate_final_follow_up <- function(df,
                                      eval_months = c(3, 6, 9, 12)) {
  # only select required variables
  keep <- names(df)[grepl(
    "^stat|^evldat",
    names(df)
  )]

  fdf <- df[, c("globalrecordid", keep)]

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
            final_fu_status = NA,
            final_fu_date = NA
          )
        )
      }
      p_df <- p[which(p$final_fu_date == max(p$final_fu_date, na.rm = TRUE)), ]
      if (nrow(p_df) > 1) {
        # if final_status is the same - return any row
        if (length(unique(p_df$final_fu_status)) == 1) {
          return(
            p_df[1, ]
          )
        }
        cli::cli_abort("{p_df$globalrecordid[1]} Multiple final outcomes detected")        
      }
      return(p_df)
    }
  )

  final_df <- Reduce(rbind, final)
  # remove duplicated records
  rdf <- final_df[!duplicated(final_df$globalrecordid), ]

  mdf <- merge(
    df,
    rdf,
    by = "globalrecordid",
    all.x = TRUE
  )
  return(mdf)
}
