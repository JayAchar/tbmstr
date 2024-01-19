render <- function(tables, plots, git, config, template) {
  rmarkdown::render(
    input = template,
    output_dir = config$output_dir,
    output_format = "all",
    params = list(
      tables = tables,
      plots = plots,
      git = git
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

render_follow_up <- function(df, config, template) {
  summary_df <- summarise_follow_up(df,
    end_date = config$end_date
  )

  rmarkdown::render(
    input = template,
    output_dir = config$output_dir,
    params = list(
      cohort = summary_df,
      end_date = config$end_date
    )
  )

  return(file.path(
    config$output_dir,
    "follow_up.docx"
  ))
}

render_sensitivity_analyses <- function(config, template) {
  rmarkdown::render(
    input = template,
    output_dir = config$output_dir,
    params = list(
      git = config$git
    )
  )

  return(file.path(
    config$output_dir,
    "sensivity.html"
  ))
}
