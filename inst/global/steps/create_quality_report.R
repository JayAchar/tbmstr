create_quality_report <- function(dd, raw, template, output_path) {
  quality_data <- tbmstr:::prepare_quality_data(
    study_data = list(
      baseline = dd$baseline,
      myco = dd$myco,
      adverse = dd$adverse,
      dst = dd$dst
    )
  )

  render_internal_rmd(
    input_file = template,
    output_file = file.path(output_path, "quality.html"),
    param = list(
      data = quality_data,
      raw = raw
    )
  )
}
