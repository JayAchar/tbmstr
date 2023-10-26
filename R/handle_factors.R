#' Handle factors
#'
#' Clean, relevel, and generally prepare factor variables
#' in the baseline data frame for analysis
#'
#' @param baseline_df baseline characteristics data frame
#'

handle_factors <- function(baseline_df) {
  stopifnot(is.data.frame(baseline_df))

  # categorise age groups
  # aim to have relatively equal groups
  age_grp_labels <- c(
    "<15",
    "15-25",
    "26-35",
    "36-45",
    "46-55",
    "56-65",
    ">65"
  )
  baseline_df$age_grp <- cut(
    baseline_df$age,
    c(
      0, 15,
      25,
      35,
      45,
      55,
      65,
      90
    ),
    labels = age_grp_labels
  )


  ae_labels <- c(
    "None",
    "Grade 1",
    "Grade 2",
    "Grade 3",
    "Grade 4"
  )

  ae_levels <- c("None", "I", "II", "III", "IV")

  baseline_df$smear <- droplevels(baseline_df$smear)

  # group employment levels
  baseline_df$empl_3grp <- baseline_df$empl

  baseline_df$empl_3grp[which(
    baseline_df$empl_3grp %in% c("Student", "Retired")
  )] <- "Other"

  baseline_df$empl_3grp <- droplevels(baseline_df$empl_3grp)

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

  baseline_df$cd4_4grp <- cut(
    baseline_df$cd4,
    c(0, 100, 250, 500, 100000)
  )

  levels(baseline_df$cd4_4grp) <- c(
    "0-100", "101-250",
    "251-500",
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

  # combine levels of baseline AE variable
  baseline_df <- combine_ae_grades(baseline_df, "ast_alt_grd", "ast_alt_bin") |>
    combine_ae_grades("prfneugrd", "prfneu_bin") |>
    combine_ae_grades("hbgrd", "hb_bin") |>
    combine_ae_grades("creatgrd", "creat_bin") |>
    combine_ae_grades("visgrd", "vis_bin")

  return(baseline_df)
}


#' Combine AE grades
#'
#' A custom function to combine AE variables with multiple grades into
#' a binary varaible
#'
#' @param df data frame of baseline characteristics
#' @param target string representing the variable being targetted
#' @param result string name of the resulting variable
#'
combine_ae_grades <- function(df, target, result) {
  # df <- combine_ae_grades(clean, "hbgrd", "hb_bin")
  df[[result]] <- as.character(df[[target]])

  df[[result]][df[[result]] != "None"] <- "Yes"
  df[[result]][df[[result]] == "None"] <- "No"

  df[[result]] <- factor(
    df[[result]],
    c("No", "Yes")
  )
  return(df)
}
