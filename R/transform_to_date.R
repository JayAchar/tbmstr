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
  if (any(class(v) %in% c("POSIXct", "POSIXt"))) return(v)
  if (!is.character(v)) {
    cli::cli_abort(
      "Requires character vector"
    )
  }

  original_na_count <- sum(is.na(v))

  transformed <- tryCatch(
    {
      as.Date(v)
    },
    error = function(cond) {
      cli::cli_alert_danger("Date characters could not be parsed and will \\
                          be returned un-formatted")
      return(v)
    }
  )

  if (sum(is.na(transformed)) > original_na_count) {
    cli::cli_alert_danger(
      "{sum(is.na(transformed))} dates were not matched and have returned NA"
    )
  }

  transformed
}
