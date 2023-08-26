#' Create End of Study outcome
#'
#' The original end of study outcome - `stat12` only provides information
#' about a participants status 12 months after treatment completion. It does
#' not incorporate any information about their on-treatment experience - for
#' example, if a participant fails on treatment, their `status12` will be
#' missing. This function combines the end of treatment outcome and the
#' `status12` variable to create a valid end of study outcome.
#'
#' @param df baseline data frame
#' 
#' @importFrom cli cli_progress_along
#'
#' @return Original baseline data frame with added variables - eos_outcome &is  eos_date

create_eos_outcome <- function(df) {

  required_vars <- c(
    "globalrecordid", "trtendat",
    "outcome", "stat3",
    "stat6", "stat9", "stat12",
    "evldat3", "evldat6", "evldat9", "evldat12"
  )

  if (!is.data.frame(df)) {
    cli::cli_abort("Input argument should be a data frame.")
  }

  if (!all(required_vars %in% names(df))) {
    cli::cli_abort("Input data frame does not include required variables.")
  }
  
  row_n <- seq_along(df$globalrecordid)
  
  
  l_df <- lapply(
    X = cli::cli_progress_along(df$globalrecordid, 
                                name = "Calculating EoS outcomes"),
    FUN = \(index) {
      long <- prepare_eos_outcomes(df[index, ])
      calculate_eos_outcome(long)
    })
  
  eos_outcome_df <- Reduce(
    f = rbind,
    x = l_df
  )
  

  merge(
    df, 
    eos_outcome_df,
    by = "globalrecordid"
  )
  
}
