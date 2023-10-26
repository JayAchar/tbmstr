render <- function(tables, plots, config, template) {
  rmarkdown::render(
    input = template,
    output_dir = config$output_dir,
    output_format = "all",
    params = list(
      tables = tables,
      plots = plots
    )
  )

  return(file.path(
    config$toutput_dir,
    "dev.docx"
  ))
}
