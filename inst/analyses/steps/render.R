render <- function(tables, config, template) {
  stopifnot(is.list(config))
  if (config$output_format == "html_document") {
    rmarkdown::render(
      input = template,
      output_dir = config$output_dir,
      output_format = config$output_format,
      params = list(
        tables = tables
      )
    )
    extension <- ".html"
  }

  if (config$output_format == "word_document") {
    render_docx(tables, config$output_dir)
    extension <- ".docx"
  }

  return(file.path(
    config$toutput_dir,
    paste0("dev", extension)
  ))
}
