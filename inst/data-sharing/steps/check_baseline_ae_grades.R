check_baseline_ae_grades <- function(df) {
  stopifnot(is.data.frame(df))
  # initialise all baseline AE variables - must include measurement and grading
  stopifnot(is.list(ae_vars))
  # for each ae_var, retrieve value and grade variables

  missing_grades <- Reduce(
    x = ae_vars,
    f = function(init, var) {
      if (is.null(var$levels)) {
        return(init + 1)
      }
      return(init)
    },
    init = 0
  )

  cli::cli_warn("{missing_grades}/{length(ae_vars)} missing grades detected")

  checked_grades <- lapply(
    ae_vars,
    FUN = function(ae_var) {
      temp_df <- df[, c("globalrecordid", ae_var$grade, ae_var$value)]
      # if no grade levels have been defined, return the original data
      if (is.null(ae_var$levels)) {
        temp_df[ae_var$value] <- NULL
        return(temp_df)
      }
      # create temp grade variable
      temp_grd_name <- paste0(ae_var$grade, "_tmp")

      grds <- findInterval(
        temp_df[[ae_var$value]],
        ae_var$levels,
        rightmost.closed = TRUE
      )

      temp_df[[temp_grd_name]] <- vapply(grds, \(val) {
        if (is.na(val) || val == 0) {
          return(NA_character_)
        }
        paste0(rep("I", val), collapse = "")
      }, character(1))

      temp_df[[ae_var$grade]] <- temp_df[[temp_grd_name]]
      temp_df[[temp_grd_name]] <- NULL
      temp_df[[ae_var$value]] <- NULL

      return(temp_df)
    }
  ) |> setNames(names(ae_vars))


  # merge the checked grades with the original baseline data frame
  Reduce(
    x = ae_vars,
    f = function(init, ae_var) {
      init[[ae_var$grade]] <- NULL
      merge(
        init,
        checked_grades[[ae_var$grade]],
        all.x = TRUE,
        by = "globalrecordid"
      )
    },
    init = df
  )
}
