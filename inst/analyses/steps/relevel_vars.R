relevel_vars <- function(dd) {
  relevel_vars <- list(
    age_grp = "35-44",
    prison = "No",
    alcohol = "No",
    prevtb = "No",
    covid = "No",
    hiv = "No",
    diab = "No",
    idu = "No",
    homeless = "No",
    cav = "No cavity",
    hcvab = "Seronegative",
    smear = "Negative",
    smok = "No"
  )

  dd$baseline <- relevel_variables(dd$baseline,
    config = relevel_vars
  )
}
