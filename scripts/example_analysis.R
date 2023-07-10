library(tbmstr)
suppressMessages(library(gtsummary))
suppressMessages(library(dplyr))
library(gt)
library(broom)

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

df <- raw$baseline |>
  mutate(
    had_sae = factor(if_else(
      had_sae == TRUE, "Yes", "No"
    ), levels = c("No", "Yes")),
    is_employed = empl == "Employed"
  )

df |>
  tbl_summary(
    include = c(
      age, sex, empl, 
      is_employed,
      covid,
      smok, hiv, diab,
      cav, hbsag, hcvab,
      outcome, had_sae,
      stat12
    ),
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      empl ~ "Employment status",
      is_employed ~ "Is the participant employed?",
      smok ~ "Smoking status",
      covid ~ "COVID-19",
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
    filename = "table_1.docx",
    path = output_dir
  )

filtered <- df |>
  filter(hcvab != "Not done")

filtered |>
  tbl_uvregression(
    include = c(
      tx_outcome,
      age,
      sex,
      is_employed,
      smok,
      hiv,
      diab,
      cav,
      hcvab,
      covid
    ),
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      empl ~ "Employment status",
      is_employed ~ "Is the participant employed?",
      smok ~ "Smoking status",
      hiv ~ "HIV status",
      diab ~ "Diabetes",
      cav ~ "Cavitatory disease",
      hcvab ~ "HCV Ab status"
    ),
    method = glm,
    method.args = list(family = binomial(link = "logit")),
    exponentiate = TRUE,
    y = tx_outcome == "Success",
    add_estimate_to_reference_rows = TRUE
  ) |>
  add_global_p() |>
  bold_labels()
  as_gt() |>
  gt::gtsave(
    filename = "table_2.html",
    path = output_dir
  )

m1 <- glm(
    (tx_outcome == "Success") ~ age + smok + is_employed, 
    data = filtered,
    family = binomial(link = "logit")
  ) |> broom::tidy(exponentiate = TRUE)

tbl_regression(m1, exponentiate = TRUE) |>
  bold_labels() |>
  as_gt() |>
  gt::gtsave(
    filename = "table_3.html",
    path = output_dir
  )
