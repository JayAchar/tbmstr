#' Calculate time to culture conversion
#'
#' The mSTR study data contains information about each sputum result
#' and the study team's calculation for culture conversion.
#' This function compares the treatment start date with the study team's
#' recorded culture conversion date to generate a numerical result representing
#' the number of days until culture conversion.
#'
#' @param trtstdat date vector representing the treatment initation date
#' @param convdat date vector representing the study team's recorded culture
#' conversion date.
#' @param baseline  data frame of baseline participant characteristics
#' @param myco data frame of participant mycobacteriology results
#' @param lab boolean to signifiy whether lab results should be used to calculate
#' culture conversion dates
#'
#' @return numeric vector representing the number of days to culture conversion
#'

create_cc_days <- function(
    trtstdat,
    convdat,
    baseline,
    myco,
    lab = FALSE) {
  # TODO: cc_days should be censored at around 4 months based on
  # the failure outcome
  if (!lab) {
    is_trtstdat_class_ok <- all(class(trtstdat) %in% c("POSIXct", "POSIXt"))
    is_convdat_class_ok <- all(class(convdat) %in% c("POSIXct", "POSIXt"))

    if (!all(is_trtstdat_class_ok, is_convdat_class_ok)) {
      cli::cli_abort("Inputs should be of type `POSIXct`.")
    }

    if (length(trtstdat) != length(convdat)) {
      cli::cli_abort("Input arguments should be the same length.")
    }

    return(as.numeric(difftime(convdat, trtstdat, units = "days")))
  }
}
