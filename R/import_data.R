#' Import data
#'
#' @inherit read_in
#' @param multi_country Boolean to allow data from multiple
#'   countries to be combined
#' @return list of data frames corresponding to the names
#'   provided in the **file_names** parameter
#' @export

import_data <- function(parent_dir,
                        file_names,
                        multi_country = FALSE) {
  df_list <- read_in(
    parent_dir = parent_dir,
    file_names = file_names
  )

  # if list of data frames only includes one element -> unlist
  # if there are more elements, then recursively rbind

  imported <- merge_imported(
    df_list = df_list,
    multi_country = multi_country
  )

  return(imported)
}

