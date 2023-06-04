#' Run internal tutorial
#'
#' @param name a single word defining the name of the tutorial to be loaded
#' @param ... optional arguments to pass to learnr::run_tutorial
#'
#' @importFrom learnr run_tutorial available_tutorials
#' @importFrom cli cli_abort
#' @return NULL
#' @export
#'
run_tutorial <- function(name,
                         ...) {
  is_name_acceptable <- is.character(name) && length(name) == 1

  if (!is_name_acceptable) {
    cli::cli_abort(
      "Please provide a correctly formatted tutorial name."
    )
  }

  all_tutorials <- learnr::available_tutorials("tbmstr")

  is_tutorial_available <- !is.na(pmatch(name, all_tutorials$name))

  if (!is_tutorial_available) {
    cli::cli_abort(
      "The requested tutorial ({name}) is unavailable. Please choose from the following list:
        {all_tutorials$name}: {all_tutorials$description}"
    )
  }

  learnr::run_tutorial(
    name = name,
    package = "tbmstr"
  )
}
