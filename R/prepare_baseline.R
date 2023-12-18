#' Prepare baseline data frame
#'
#' @param df_list List of named data frames
#' @param cohort define cohort type required
#'
#' @importFrom cli cli_abort cli_warn cli_alert_warning
#' @export
#'
#' @return df_list with modifications
#'
prepare_baseline <- function(df_list,
                             cohort = c(
                               "treatment",
                               "adverse"
                             )) {
  print(Sys.getenv())
  cohort <- match.arg(cohort)

  prepare_baseline_checks(df_list)

  df_list$baseline <- remove_invalid_records(
    df_list[["baseline"]]
  )

  if (cohort == "treatment") {
    df_list$baseline <- remove_withdrawn_subjects(df_list[["baseline"]])
  }

  df_list$baseline <- calculate_variables(df_list$baseline)

  df_list$baseline <- create_eos_outcome(
    df_list$baseline
  )

  # df_list$baseline <- create_conversion_variables(
  #   baseline = df_list$baseline,
  #   myco = df_list$myco
  # )

  alert_info("`cc_days` variable calculated from \\
                      `trtstdat` and `convdat`.")

  df_list$baseline <- calculate_baseline_smear(
    baseline = df_list$baseline,
    myco = df_list$myco
  )

  df_list$baseline$outcome <- droplevels(
    df_list$baseline$outcome
  )

  alert_info("`smear` variable calculated from \\
                      `myco` data frame.")

  df_list$baseline <- handle_factors(
    df_list$baseline
  )

  if (!"had_sae" %in% names(df_list$baseline)) {
    had_sae <- unique(df_list$adverse$globalrecordid[which(
      df_list$adverse$sae %in% list(1, "Seriouse")
    )])
    df_list$baseline$had_sae <- df_list$baseline$globalrecordid %in% had_sae
  }

  df_list$baseline[] <- lapply(
    df_list$baseline,
    FUN = \(col) {
      if (!is.factor(col)) {
        return(col)
      }
      unknowns <- which(col == "Unknown")
      if (length(unknowns) == 0) {
        return(col)
      }
      col[which(col == "Unknown")] <- NA
      col <- droplevels(col)
      col
    }
  )

  return(df_list)
}

#' @noRd
prepare_baseline_checks <- function(df_list) {
  if (!is.list(df_list) || is.data.frame(df_list)) {
    cli::cli_abort("`df_list` must be a list")
  }

  required <- c("baseline", "adverse", "myco")
  is_not_df <- vapply(
    required,
    \(name) !is.data.frame(df_list[[name]]),
    logical(1)
  )

  if (!all(required %in% names(df_list))) {
    cli::cli_alert_danger("Required data frames not detected in input list.")
    cli::cli_h1("Required named data frames:")
    ul <- cli::cli_ul()
    cli::cli_li("baseline")
    cli::cli_li("adverse")
    cli::cli_end(ul)
    cli::cli_warn("Returning input list.")
    return(df_list)
  }

  if (any(is_not_df)) {
    cli::cli_warn("All elements in `df_list` must be a data frame")
    cli::cli_alert_warning("Returning input list.")
    return(df_list)
  }
}
