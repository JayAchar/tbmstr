#' Calculate final follow up
#'
#' Calculate the final follow up date, month and status for
#' participants who successfully completed treatment
#'
#' @param df data frame of all participants' baseline characteristics
#' @param eval_months evaluation months
#'
#'
calculate_final_follow_up <- function(df,
                                      eval_months = c(3, 6, 9, 12)) {
  # only select required variables
  keep <- names(df)[grepl(
    "^stat|^evldat|trtendat",
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

  # split by participant
  pps <- split(full_df, full_df$globalrecordid)

  final <- lapply(pps,
    FUN = \(p) {
      # remove invalid evaluations
      # remove missing and not evaluated
      is_invalid_status <- is.na(p$final_fu_status) |
        p$final_fu_status == "Not evaluated" | is.na(p$final_fu_date)

      filtered <- p[!is_invalid_status, ]

      if (nrow(filtered) == 0) {
        tx_hx <- df[which(unique(p$globalrecordid) == df$globalrecordid), c(
          "outcome",
          "trtendat"
        )]

        tx_outcome <- ifelse(tx_hx$outcome %in% c("Cured", "Completed"),
          "No TB", NA_character_
        )

        return(
          data.frame(
            globalrecordid = p$globalrecordid,
            final_fu_status = factor_eos_outcome(tx_outcome),
            final_fu_date = tx_hx$trtendat
          )
        )
      }

      rows_died <- which(filtered$final_fu_status == "Died")

      if (length(rows_died) > 0) {
        earliest_died_fu_date <- filtered[min(rows_died), "final_fu_date"]
        deathdat <- df$deathdat[
          which(
            unique(p$globalrecordid) == df$globalrecordid
          )
        ]

        death_date <- ifelse(is.na(deathdat),
          earliest_died_fu_date,
          deathdat
        )

        return(
          data.frame(
            globalrecordid = p$globalrecordid,
            final_fu_status = factor_eos_outcome("Died"),
            final_fu_date = death_date
          )
        )
      }

      p_df <- filtered[which(
        filtered$final_fu_date == max(filtered$final_fu_date,
          na.rm = TRUE
        )
      ), ]

      if (nrow(p_df) > 1) {
        # if final_status is the same - return any row
        if (length(unique(p_df$final_fu_status)) == 1) {
          return(
            p_df[1, ]
          )
        }
        cli::cli_abort(
          "{p_df$globalrecordid[1]} Multiple final outcomes detected"
        )
      }
      return(p_df)
    }
  )

  # FIXME: 2 post treatment deaths were found in previous
  # workflow and are missing in the updated version.
  # Should there be 43 post treatment deaths or 41??
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
