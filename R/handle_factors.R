#' Handle factors
#'
#' Clean, relevel, and generally prepare factor variables
#' in the baseline data frame for analysis
#'
#' @param baseline_df baseline characteristics data frame
#'

handle_factors <- function(baseline_df) {
  stopifnot(is.data.frame(baseline_df))

  ae_labels <- c(
    "None",
    "Grade 1",
    "Grade 2",
    "Grade 3",
    "Grade 4"
  )

  ae_levels <- c("None", "I", "II", "III", "IV")

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

  # combine baseline AST/ALT AE grade
  baseline_df$ast_alt_grd <- pmax(
    as.numeric(baseline_df$astgrd),
    as.numeric(baseline_df$altgrd),
    na.rm = TRUE
  )

  baseline_df$ast_alt_grd[is.na(baseline_df$ast_alt_grd)] <- 0

  baseline_df$ast_alt_grd <- factor(
    baseline_df$ast_alt_grd,
    levels = c(0:4),
    labels = ae_labels
  )

  ae_vars <- c("prfneugrd", "hbgrd", "creatgrd", "visgrd")

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

  # convert smoking intensity into a factor variable
  baseline_df$sm_fact <- ifelse(
    baseline_df$smdur >= 20,
    2, 1
  )

  baseline_df$sm_fact[is.na(baseline_df$sm_fact)] <- 0

  baseline_df$sm_fact <- factor(
    baseline_df$sm_fact,
    levels = c(0:2),
    labels = c("None", "<20 pack-years", "\u226520 pack-years")
  )

  return(baseline_df)
}
