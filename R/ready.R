#' Package ready?
#'
#' A simple function to show that the user has successfully installed
#' the package and are ready to use it for analyses.
#'
#' @importFrom cli cli_alert_success
#' @importFrom utils packageVersion
#'
#' @return NULL - prints a message to the console
#'
#' @export

ready <- function() {
  string <- paste(
    Sys.info()["sysname"],
    Sys.timezone(),
    Sys.Date(),
    packageVersion("tbmstr"),
    sep = "-"
  )
  cli::cli_alert_success(
    "Package successfully installed: {tolower(string)}"
  )
}
