#' Transform character to date
#'
#' Some versions of the user's study data might include date variables
#' which are transcribed as character vectors. This function will convert
#' these to date variables
#'
#' @param v A character vector which represents dates
#'
#' @importFrom cli cli_abort cli_alert_danger
#' @return A date vector
#'
transform_to_date <- function(v) {
  if (!is.character(v)) {
    cli::cli_abort(
      "Requires character vector"
    )
  }

  transformed <- as.Date(v,
    format = "%a %b %d %Y %H:%M:%S"
  )

  if (any(is.na(transformed))) {
    cli::cli_alert_danger(
      "{sum(is.na(transformed))} dates were not matched and have returned NA"
    )
  }

  transformed
}
