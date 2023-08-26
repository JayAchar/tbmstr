#' Apply labels
#'
#' Apply text labels to variables which are represented in the
#' internal package lookup table
#' @param df data frame
#' @param variable character string representing the name of the target
#'   variable in `df`
#' @param convert_to_factor boolean to flat whether to conver the returned
#'   variable to an unordered factor - defaults to TRUE
#'
#' @return character vector where NA is introduced for unmatched values
#'
#' @importFrom cli cli_abort cli_warn
#'

apply_labels <- function(df, variable, convert_to_factor = TRUE) {
  if (!is.data.frame(df)) {
    cli::cli_abort("`df` must be a data frame")
  }

  lut <- internal$lut

  if (!is.character(variable)) {
    cli::cli_abort("`variable` must be a single length string vector")
  }

  if (!variable %in% names(df)) {
    cli::cli_abort("`variable` must be a column name in `df`")
  }

  if (!variable %in% unique(lut$name)) {
    cli::cli_alert_info("`apply_labels()`: `{variable}` is not found in the \\
                  variable look up table")
    return(df[[variable]])
  }

  filtered_lut <- lut[which(lut$name == variable), ]

  assigned <- filtered_lut$description[match(
    df[[variable]],
    filtered_lut$value
  )]

  # cont number of NA's in original vector
  original_na <- sum(is.na(df[[variable]]))

  assigned_na <- sum(is.na(assigned))

  if (!original_na == assigned_na) {
    cli::cli_warn(
      "`apply_labels()`: {variable}: {assigned_na - original_na}
      missing value{?s} have been introduced"
    )
  }

  if (convert_to_factor) {
    assigned <- factor(assigned,
      levels = filtered_lut$description
    )
  }

  return(assigned)
}
