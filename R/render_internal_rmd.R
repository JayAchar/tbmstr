#' Render an internal RMarkdown file
#' 
#' This function renders an internal RMarkdown file located in the inst/rmarkdown/ directory
#' 
#' @param input_file Path to the RMarkdown file relative to the inst/rmarkdown/ directory
#' @param output_file Path to the output file
#' @param output_format Output format for the rendered document
#' @param ... Additional arguments to pass to the `rmarkdown::render()` function
#' 
#' @importFrom rmarkdown render
#' 
#' @return NULL
#'
#' @export
render_internal_rmd <- function(input_file, 
                                output_file, 
                                output_format = "html_document", ...) {
  rmd_file <- system.file("rmd", 
                          input_file, 
                          package = "tbmstr")
  
  if (!file.exists(rmd_file)) {
    cli::cli_abort("RMarkdown file \"{input_file}\" not found in `inst/rmd/` directory")
  }
  
  rmarkdown::render(input = rmd_file, 
                    output_file = output_file, 
                    output_format = output_format, ...)
  
  invisible(NULL)
}