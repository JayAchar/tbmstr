render_docx <- function(tables, output_dir) {
  ps_landscape <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape")
  )
  ps_portrait <- officer::prop_section(
    page_size = officer::page_size(orient = "portrait")
  )

  tables_doc <- officer::read_docx() |>
    officer::body_add_toc(level = 1) |>
    officer::body_add_par(
      "Treatment cohort description",
      style = "heading 1"
    ) |>
    officer::body_add_par(
      "Baseline demographic and clinical characteristics of participants",
      style = "heading 2"
    ) |>
    body_add_flextable(value = tables$tx_description) |>
    officer::body_add_break() |>
    officer::body_add_par("Treatment cohort outcomes", style = "heading 1") |>
    officer::body_add_par("Main Table 2", style = "heading 2") |>
    body_add_flextable(value = tables$tx_outcomes) |>
    officer::body_end_block_section(value = officer::block_section(property = ps_portrait)) |>
    officer::body_add_par(
      "Crude and adjusted analysis of independent predictors of unsuccessful study outcome",
      style = "heading 1"
    ) |>
    officer::body_add_par("Main Table 3", style = "heading 2") |>
    body_add_flextable(value = tables$tx_mv_failure) |>
    body_end_block_section(value = block_section(property = ps_landscape)) |>
    body_add_par("Treatment outcomes by baseline HIV status",
      style = "heading 1"
    ) |>
    body_add_par("Supplement Table 1", style = "heading 2") |>
    body_add_flextable(value = tables$tx_outcomes_by_hiv) |>
    body_add_break() |>
    body_add_par("HIV-specific description for People Living with HIV",
      style = "heading 1"
    ) |>
    body_add_par("Supplement Table 2", style = "heading 1") |>
    body_add_flextable(value = tables$hiv_outcomes) |>
    body_end_block_section(value = block_section(property = ps_portrait)) |>
    body_add_par(
      "Crude analysis of predictors of unsuccessful study outcomes and death in People Living with HIV",
      style = "heading 1"
    ) |>
    body_add_par("Supplement Table 3", style = "heading 2") |>
    body_add_flextable(value = tables$hiv_failure_death) |>
    body_add_break() |>
    body_add_par(
      "Kaplan Meier estimates of risk of culture conversion at specified time points",
      style = "heading 1"
    ) |>
    body_add_flextable(value = tables$cc_risk) |>
    body_end_block_section(value = block_section(property = ps_landscape)) |>
    body_add_par("Reasons for study failure", style = "heading 1") |>
    body_add_flextable(value = tables$failure_reasons)

  print(tables_doc, target = paste0(output_dir, "/dev.docx"))
}
