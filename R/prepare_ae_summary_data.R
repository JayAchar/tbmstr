#' Prepare AE summary data
#'
#' @param baseline data frame representing the baseline study data
#' @param adverse data frame representing adverse event data
#' @param type SAE or AE represents the type of AE required
#' @importFrom cli cli_abort
#'
#' @return
#'

prepare_ae_summary_data <- function(baseline, adverse,
                                    type = c("SAE", "AE")) {
  if (!(is.data.frame(baseline) && is.data.frame(adverse))) {
    cli::cli_abort("Input parameters - `baseline` and `adverse` must \\
                       be data frames")
  }

  type <- match.arg(type)
  type_lut <-
    list(
      SAE = "Seriouse",
      AE = "Not seriouse"
    )

  f_adverse <- adverse[which(adverse$sae == type_lut[type]), ]

  df <- merge(
    x = f_adverse,
    y = baseline[, c("globalrecordid", "trtstdat")],
    by = "globalrecordid"
  )

  sae_type <- NULL
  if (type == "SAE") {
    sae_type <- df$saetype
  }

  # TODO: partially completed only
  return(
    data.frame(
      Type = df$aeterm,
      `SAE type` = sae_type,
      Severity = df$severity,
      Outcome = df$aeoutcome
    )
  )
}
