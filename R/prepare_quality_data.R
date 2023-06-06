#' Prepare quality report data
#'
#' Merge, condense and simplify a full set of study data into the data
#' structure required for the study data quality report template.
#'
#' @param study_data A named list of data frames with the following keys:
#'  * adverse
#'  * baseline
#'  * dst
#'  * myco
#'
#' @importFrom cli cli_abort
#'
#' @return Named list
#'
prepare_quality_data <- function(study_data) {
  allowed_names <- c("baseline", "dst", "myco", "adverse")

  if (!is.list(study_data)) {
    cli::cli_abort(
      "`study_data` must be named list"
    )
  }

  if (!all(allowed_names %in% names(study_data))) {
    cli::cli_abort(
      "Data frames must be specfied with the following names: {allowed_names}"
    )
  }

  baseline <- study_data$baseline
  dst <- study_data$dst
  myco <- study_data$myco
  adverse <- study_data$adverse

  # baseline data frame
  narrow_baseline <- baseline[, c("globalrecordid", "trtstdat")]
  # select only relevant variables

  # dst data frame
  # select only releveant variables
  # TODO include the select statement in `prepare_dst()`
  narrow_dst <- dst[, c(
    "fkey", "datedst", "dstr", "dstlfx", "dstmfx",
    "dstlzd1", "dstbdq1", "dstcfz"
  )]
  # merge with baseline data frame
  merged_dst <- prepare_dst(
    baseline = narrow_baseline,
    dst = narrow_dst,
    purpose = "quality"
  )
  # myco data frame
  # merge with baseline data frame
  # select relevant variables
  narrow_myco <- myco[, c(
    "fkey", "datespecimen",
    "test_type", "result"
  )]

  merged_myco <- prepare_myco(
    baseline = narrow_baseline,
    myco = narrow_myco,
    purpose = "quality"
  )
  # only include tests that are relevant for baseline lab confirmation

  return(list(
    baseline = baseline,
    dst = merged_dst,
    myco = merged_myco,
    adverse = adverse
  ))
}
