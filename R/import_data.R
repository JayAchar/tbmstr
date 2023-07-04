#' Import data
#'
#' @inherit read_in
#' @param multi_country Boolean to allow data from multiple
#'   countries to be combined
#' @param apply_labels Boolean to indicate whether variables
#'   which have labels in the package look up table should
#'   have them applied
#' @return list of data frames corresponding to the names
#'   provided in the **file_names** parameter
#' @export

import_data <- function(parent_dir,
                        file_names,
                        multi_country = FALSE,
                        apply_labels = TRUE) {
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
  
  # merge SAE boolean into baseline
  had_sae <- unique(imported$adverse$globalrecordid[which(imported$adverse$sae == 1) ])
  imported$baseline$had_sae <- imported$baseline$globalrecordid %in% had_sae

  if (apply_labels) {
    imported <- apply_all_labels(imported)
  }

  return(imported)
}
