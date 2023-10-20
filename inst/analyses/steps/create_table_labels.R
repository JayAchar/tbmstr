create_table_labels <- function() {
  mv_fail <- list(
    age ~ "Age (yrs)",
    sex ~ "Sex",
    cav ~ "X-ray cavities",
    bmi_group ~ "Body Mass Index (kg/m^2)",
    hiv ~ "HIV status",
    hcvab ~ "HCV Ab status",
    smear ~ "Baseline smear microscopy status",
    prison ~ "History of incarceration",
    alcohol ~ "Excess alcohol use",
    prevtb ~ "Previous TB episode",
    homeless ~ "Homeless",
    smok ~ "Smoking history",
    idu ~ "History of injecting drug use",
    hbgrd ~ "Baseline anaemia"
  )

  labels <- c(mv_fail, list(
    cntry ~ "Country",
    empl ~ "Employment status",
    sm_fact ~ "Smoking intensity",
    diab ~ "Diabetes",
    covid ~ "Baseline SARS-CoV2 status",
    regimen ~ "Treatment regimen",
    prfneugrd ~ "Baseline peripheral neuropathy",
    creatgrd ~ "Baseline renal dysfunction",
    visgrd ~ "Baseline visual loss",
    ast_alt_grd ~ "Baseline elevated AST/ALT"
  ))

  hiv_labels <- list(
    art ~ "Baseline ART status",
    artreg ~ "ART regimen",
    cd4 ~ "Baseline CD4 count",
    cd4_grp ~ "Baseline CD4 group",
    cpt ~ "Receiving cotrimoxazole"
  )

  list(
    mv = mv_fail,
    tx_desc = labels,
    hiv = hiv_labels
  )
}
