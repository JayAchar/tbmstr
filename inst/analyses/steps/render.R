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
    config$output_dir,
    "dev.docx"
  ))
}

render_short_success <- function(df, config, template) {
  rmarkdown::render(
    input = template,
    output_dir = config$output_dir,
    params = list(
      df = df
    )
  )

  return(file.path(
    config$output_dir,
    "short_success.docx"
  ))
}

render_withdrawls <- function(lst, config, template) {
  rmarkdown::render(
    input = template,
    output_dir = config$output_dir,
    params = list(
      lst = lst
    )
  )

  return(file.path(
    config$output_dir,
    "withdrawals.docx"
  ))
}
