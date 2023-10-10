#' Handle factors
#'
#' Clean, relevel, and generally prepare factor variables
#' in the baseline data frame for analysis
#'
#' @param baseline_df
#'

handle_factors <- function(baseline_df) {
  stopifnot(is.data.frame(baseline_df))

  # create grouped baseline CD4 variable
  baseline_df$cd4_grp <- cut(
    baseline_df$cd4,
    c(0, 50, 100, 200, 300, 400, 500, 100000)
  )

  levels(baseline_df$cd4_grp) <- c(
    "0-50", "51-100",
    "101-200",
    "201-300", "301-400", "401-500",
    "500+"
  )

  # convert basleine ART status to a factor
  baseline_df$art <- factor(baseline_df$art,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )

  ae_vars <- c("prfneugrd", "hbgrd", "creatgrd", "visgrd")

  ae_labels <- c(
    "None",
    "Grade 1",
    "Grade 2",
    "Grade 3",
    "Grade 4"
  )

  ae_levels <- c("None", "I", "II", "III", "IV")

  baseline_df[ae_vars] <- lapply(
    ae_vars,
    \(var) {
      y <- as.character(baseline_df[[var]])
      y[is.na(y)] <- "None"
      factor(y,
        levels = ae_levels,
        labels = ae_labels
      )
    }
  )

  return(baseline_df)
}
