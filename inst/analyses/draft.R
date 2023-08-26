devtools::load_all()
library(flextable)
library(gtsummary)
library(officer)

data_path <- here::here("data", "regional_prepared")
output_dir <- here::here("inst", "analyses", "output")
quality_file <- here::here(output_dir, "quality.html")

# import data and apply labels
raw <- import_data(
  parent_dir = data_path, 
  multi_country = TRUE,
  apply_labels = FALSE,
  file_names = list(
    adverse = "adverse",
    baseline = "baseline",
    myco = "myco",
    dst = "dst"
  )
)

# check data
# remove invalid records from baseline table
raw$baseline <- remove_invalid_records(
  raw$baseline
)

# prepare data for quality check
quality_data <- prepare_quality_data(
  study_data = list(
    baseline = raw$baseline,
    myco = raw$myco,
    adverse = raw$adverse,
    dst = raw$dst
  )
)

render_internal_rmd(
  input_file = "quality.Rmd",
  output_file = quality_file,
  param = list(
    data = quality_data,
    raw = raw
  )
)

# mutate data
labelled <- apply_all_labels(raw)
## add analysis variables
prepared <- prepare_baseline(labelled)

## Add event variables
### Any death
prepared$baseline$event_death <- ifelse(
  (prepared$baseline$outcome != "Died" | is.na(prepared$baseline$outcome)) & 
    (prepared$baseline$eos_outcome != "Died during follow-up" | is.na(prepared$baseline$eos_outcome)),
  0, 1
)

### Failure, Death, LTFU on tx
prepared$baseline$event_ltfu_on_tx <- ifelse(
  prepared$baseline$eos_outcome %in% c("Reoccurance", "Died during follow-up",
                              "Unsuccessful treatment"),
  1, 0
)

### Failure, Death, any LTFU
# TODO: discuss how to create this variable
# There's no LTFU during follow up option in the eos_outcome
# so study sites have only entered NA.


## Add time variable
# TODO: create time variables for each definition of event

# descriptive outcomes table

mt2 <- gtsummary::tbl_summary(
  data = prepared$baseline,
  include = c("outcome", "eos_outcome")
) |> gtsummary::as_flex_table()

## outcomes stratified by HIV status
st1 <- gtsummary::tbl_summary(
  data = prepared$baseline,
  by = "hiv",
  include = c("outcome", "eos_outcome")
) |> 
  modify_spanning_header(
    c("stat_1", "stat_2", "stat_3") ~ "**HIV status**"
  ) |> as_flex_table()


covariates <- c(
  "cntry",
  "age", "sex", "cav", "bmi", "hiv", "hcvab",
  "smear", "prison", "alcohol", "prevtb", "diab",
  "covid",
  "regimen"
)

# output table 1
mt1 <- gtsummary::tbl_summary(
  data = prepared$baseline,
  include = covariates,
) |> gtsummary::as_flex_table()

# output table 2
t3 <- gtsummary::tbl_uvregression(
  data = prepared$baseline,
  method = glm,
  y = event_ltfu_on_tx,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  include = covariates
)

# output table 3
t4<- glm(
  formula = event_ltfu_on_tx ~ age + sex + bmi + hiv+ prison + alcohol + prevtb + 
    cav + hcvab + 
    smear,
  family = binomial, 
  data = prepared$baseline
) |> 
gtsummary::tbl_regression(
  exponentiate = TRUE
)

mt3 <- gtsummary::tbl_merge(
  list(t3, t4),
  tab_spanner = c("**Crude**", "**Adjusted**")
) |> as_flex_table()

tables_doc <- officer::read_docx()  |>
  body_add_par("Main Table 1", style = "heading 1")  |>
  body_add_flextable(value = mt1)  |>
  body_add_break() |>
  body_add_par("Main Table 2", style = "heading 1")  |>
  body_add_flextable(value = mt2)  |>
  body_add_break() |>
  body_add_par("Supplementary Table 1", style = "heading 1")  |>
  body_add_flextable(value = st1)  |>
  body_add_break() |>
  body_end_section_portrait() |>
  body_add_par("Main Table 3", style = "heading 1")  |>
  body_add_par(
    "Table 3 describes bivariable associations with an End of study outcome including
               on treatment LTFU, but not LTFU during post-treatment follow-up",
    style = "Normal"
  ) |>
  body_add_flextable(value = mt3) |>
  body_end_section_landscape() 
  

print(tables_doc, 
      target = here::here(output_dir, "tables.docx"))


