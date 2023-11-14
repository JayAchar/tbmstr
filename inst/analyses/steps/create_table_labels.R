create_table_labels <- function() {
  mv_fail <- list(
    age_grp ~ "Age group (yrs)",
    cav ~ "X-ray cavities",
    bmi_group ~ "Body Mass Index (kg/m^2)",
    hiv ~ "HIV positive status",
    empl_3grp ~ "Employment status",
    smear ~ "Baseline smear microscopy status",
    alcohol ~ "Excess alcohol use",
    hb_bin ~ "Baseline anaemia",
    ast_alt_bin ~ "Baseline elevated AST/ALT",
    sm_fact ~ "Smoking intensity"
  )

  desc <- list(
    age ~ "Age (yrs)",
    age_grp ~ "Age group (yrs)",
    sex ~ "Sex",
    cav ~ "X-ray cavities",
    cohort_bilevel ~ "Inclusion cohort",
    bmi_group ~ "Body Mass Index (kg/m^2)",
    hiv ~ "HIV positive status",
    hcvab ~ "HCV Ab status",
    smear ~ "Baseline smear microscopy status",
    prison ~ "History of incarceration",
    alcohol ~ "Excess alcohol use",
    prevtb ~ "Previous TB episode",
    homeless ~ "Homeless",
    smok ~ "Smoking history",
    idu ~ "History of injecting drug use",
    empl_3grp ~ "Employment status",
    sm_fact ~ "Smoking intensity",
    diab ~ "Diabetes",
    covid ~ "Baseline SARS-CoV2 status",
    regimen ~ "Treatment regimen",
    cntry ~ "Country",
    hbgrd ~ "Baseline anaemia",
    prfneugrd ~ "Baseline peripheral neuropathy",
    creatgrd ~ "Baseline renal dysfunction",
    visgrd ~ "Baseline visual loss",
    ast_alt_grd ~ "Baseline elevated AST/ALT"
  )

  univariable <- list(
    age_grp ~ "Age group (yrs)",
    sex ~ "Sex",
    cav ~ "X-ray cavities",
    bmi_group ~ "Body Mass Index (kg/m^2)",
    cohort_bilevel ~ "Inclusion cohort",
    hiv ~ "HIV positive status",
    hcvab ~ "HCV Ab status",
    smear ~ "Baseline smear microscopy status",
    prison ~ "History of incarceration",
    alcohol ~ "Excess alcohol use",
    prevtb ~ "Previous TB episode",
    homeless ~ "Homeless",
    smok ~ "Smoking history",
    idu ~ "History of injecting drug use",
    empl_3grp ~ "Employment status",
    sm_fact ~ "Smoking intensity",
    diab ~ "Diabetes",
    covid ~ "Baseline SARS-CoV2 status",
    regimen ~ "Treatment regimen",
    prfneu_bin ~ "Baseline peripheral neuropathy",
    creat_bin ~ "Baseline renal dysfunction",
    vis_bin ~ "Baseline visual loss",
    ast_alt_bin ~ "Baseline elevated AST/ALT",
    hb_bin ~ "Baseline anaemia"
  )

  hiv_labels <- list(
    art ~ "Baseline ART status",
    artreg ~ "ART regimen",
    cd4 ~ "Baseline CD4 count",
    cd4_4grp ~ "Baseline CD4 group",
    cpt ~ "Receiving cotrimoxazole"
  )

  list(
    mv = mv_fail,
    tx_desc = desc,
    hiv = hiv_labels,
    uni = univariable
  )
}
