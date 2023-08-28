devtools::load_all()
library(flextable)
library(gtsummary)
library(officer)
library(survival)

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
if (FALSE) {
  render_internal_rmd(
    input_file = "quality.Rmd",
    output_file = quality_file,
    param = list(
      data = quality_data,
      raw = raw
    )
  )
}

# mutate data
labelled <- apply_all_labels(raw)
## add analysis variables
prepared <- prepare_baseline(labelled,
  cohort = "treatment"
)

# remove withdrawn factor level from outcome
prepared$baseline$outcome <- droplevels(
  prepared$baseline$outcome
)

covariates <- c(
  "age", "sex", "cntry", "cav", "bmi", "hiv", "hcvab",
  "smear", "prison", "alcohol", "prevtb", "diab",
  "covid", "regimen"
)

labels <- list(
  age ~ "Age (yrs)",
  sex ~ "Sex",
  cntry ~ "Country",
  cav ~ "X-ray cavities",
  bmi ~ "Body Mass Index",
  hiv ~ "HIV status",
  hcvab ~ "HCV Ab status",
  smear ~ "Baseline smear microscopy status",
  prison ~ "History of incarceration",
  alcohol ~ "Excess alcohol use",
  prevtb ~ "Previous TB episode",
  diab ~ "Diabetes",
  covid ~ "Baseline SARS-CoV2 status",
  regimen ~ "Treatment regimen"
)

# output table 1
mt1 <- gtsummary::tbl_summary(
  data = prepared$baseline,
  include = covariates,
  label = labels,
  # TODO: Reorder variables
  # TODO: `cav` includes two unknown types - combine
  # TODO: Categorise BMI variable
  # TODO: Convert baseline smear to grade rather than positive/negative
) |> gtsummary::as_flex_table()

# descriptive outcomes table
mt2 <- gtsummary::tbl_summary(
  data = prepared$baseline,
  include = c("outcome", "eos_outcome"),
  label = list(
    outcome ~ "End of treatment outcome",
    eos_outcome ~ "End of study outcome"
  ),
) |> gtsummary::as_flex_table()

## outcomes stratified by HIV status
st1 <- gtsummary::tbl_summary(
  data = prepared$baseline,
  by = "hiv",
  # TODO: combine Unknown HIV status with 'No'
  include = c("outcome", "eos_outcome"),
  label = list(
    outcome ~ "End of treatment outcome",
    eos_outcome ~ "End of study outcome"
  )
) |>
  modify_spanning_header(
    c("stat_1", "stat_2", "stat_3") ~ "**HIV status**"
  ) |>
  as_flex_table()

# output table 2
t3 <- gtsummary::tbl_uvregression(
  # TODO: Change base level for relevant varialbes
  data = prepared$baseline,
  label = labels,
  method = survival::coxph,
  y = survival::Surv(fail_days, event_fail),
  exponentiate = TRUE,
  include = all_of(covariates[!covariates == "cntry"])
) |>
  gtsummary::add_n(location = "label") |>
  gtsummary::add_nevent(location = "level")

# TODO: add random effects to account for clustering by country
mv_fail <- survival::coxph(
  survival::Surv(
    prepared$baseline$fail_days,
    prepared$baseline$event_fail
  ) ~ age + sex + bmi +
    hiv + prison + alcohol + prevtb + cav + hcvab + smear,
  data = prepared$baseline
)

mv_fail_labels <- list(
  age ~ "Age (yrs)",
  sex ~ "Sex",
  cav ~ "X-ray cavities",
  bmi ~ "Body Mass Index",
  hiv ~ "HIV status",
  hcvab ~ "HCV Ab status",
  smear ~ "Baseline smear microscopy status",
  prison ~ "History of incarceration",
  alcohol ~ "Excess alcohol use",
  prevtb ~ "Previous TB episode"
)

t4 <- mv_fail |>
  gtsummary::tbl_regression(
    exponentiate = TRUE,
    label = mv_fail_labels,
  )

mt3 <- gtsummary::tbl_merge(
  list(t3, t4),
  tab_spanner = c("**Crude**", "**Adjusted**")
) |> as_flex_table()

p1 <- ggsurvfit::survfit2(
  survival::Surv(fail_days, event_fail) ~ 1,
  data = prepared$baseline
) |>
  ggsurvfit::ggsurvfit() +
  ggsurvfit::add_confidence_interval() +
  ggsurvfit::add_risktable() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Kaplan Meier estimates for time to unsuccessful study outcome",
    x = "Time from treatment start (days)"
  )

p2 <- ggsurvfit::survfit2(
  survival::Surv(
    death_days, event_death
  ) ~ 1,
  data = prepared$baseline
) |>
  ggsurvfit::ggsurvfit() +
  ggsurvfit::add_confidence_interval() +
  ggsurvfit::add_risktable() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Kaplan Meier estimates for time to death",
    x = "Time from treatment start (days)"
  )

p3 <- ggsurvfit::survfit2(
  survival::Surv(
    death_days, event_death
  ) ~ hiv,
  data = prepared$baseline
) |>
  ggsurvfit::ggsurvfit() +
  ggsurvfit::add_confidence_interval() +
  ggsurvfit::add_risktable() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Kaplan Meier estimates for time to death by HIV status",
    x = "Time from treatment start (days)"
  )

ggsave(
  filename = "p1.png",
  path = output_dir,
  plot = p1,
  device = "png",
  width = 10,
  height = 7
)

ggsave(
  filename = "p2.png",
  path = output_dir,
  plot = p2,
  device = "png",
  width = 10,
  height = 7
)

ggsave(
  filename = "p3.png",
  path = output_dir,
  plot = p3,
  device = "png",
  width = 10,
  height = 7
)

tables_doc <- officer::read_docx() |>
  body_add_par("Main Table 1", style = "heading 1") |>
  body_add_flextable(value = mt1) |>
  body_add_break() |>
  body_add_par("Main Table 2", style = "heading 1") |>
  body_add_flextable(value = mt2) |>
  body_add_break() |>
  body_add_par("Supplementary Table 1", style = "heading 1") |>
  body_add_flextable(value = st1) |>
  body_add_break() |>
  body_end_section_portrait() |>
  body_add_par("Main Table 3", style = "heading 1") |>
  body_add_par(
    "Table 3 describes bivariable associations with an End of study outcome including
               on treatment LTFU, but not LTFU during post-treatment follow-up",
    style = "Normal"
  ) |>
  body_add_flextable(value = mt3) |>
  body_add_break() |>
  body_end_section_landscape()


print(tables_doc,
  target = here::here(output_dir, "tables.docx")
)
