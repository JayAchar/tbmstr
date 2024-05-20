#' Match AE grades to values
#'
#'
#' @param values vector of numerical values
#' @param grades vector of numbers representing the left-boundary of grading
#'  levels
#' @export

match_ae_grade <- function(values, grades) {
  stopifnot(length(grades) == 4L)
  reversed <- all(diff(grades) < 0)
  lhs_open <- FALSE

  if (reversed) {
    grades <- rev(grades)
    lhs_open <- TRUE
  }

  numeric_grades <- findInterval(
    values,
    grades,
    rightmost.closed = TRUE,
    left.open = lhs_open
  )

  if (reversed) {
    numeric_grades <- abs(numeric_grades - 4)
  }

  vapply(numeric_grades, \(val) {
    if (is.na(val) || val == 0) {
      return(NA_character_)
    }
    paste0(rep("I", val), collapse = "")
  }, character(1))
}
