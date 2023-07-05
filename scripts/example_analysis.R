library(tbmstr)
suppressMessages(library(gtsummary))
suppressMessages(library(dplyr))
library(gt)

# change parent_dir as required
parent_dir <- here::here("data", "parent_dir")
output_dir <- here::here("data", "output")

# read in and prepare data
raw <- run_analysis(
  parent_dir = parent_dir,
  file_names = list(
    baseline = "baseline",
    adverse = "adverse"
  ),
  data_only = TRUE
)

raw$baseline |> 
  mutate(had_sae = factor(if_else(
    had_sae == TRUE, "Yes", "No"
  ), levels = c("No", "Yes"))) |> 
  tbl_summary(
    include = c(age, sex, empl,
                smok, hiv, diab,
                cav, hbsag, hcvab,
                outcome, had_sae,
                stat12),
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      empl ~ "Employment status",
      smok ~ "Smoking status",
      hiv ~ "HIV status",
      diab ~ "Diabetes",
      cav ~ "Cavitatory disease",
      hbsag ~ "HBsAg status",
      hcvab ~ "HCV Ab status",
      outcome ~ "End of treatment outcome",
      had_sae ~ "Suffered a SAE",
      stat12 ~ "Status 12m post-treatment completion"
    ),
    type = all_dichotomous() ~ "categorical",
    missing = "ifany",
    missing_text = "Missing"
  ) |> 
  bold_labels() |> 
  as_gt() |> 
  gt::gtsave(
    filename = "table_1.html",
    path = output_dir
  )
  
