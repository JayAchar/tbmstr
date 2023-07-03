#' Merged imported study data
#'
#' Some study teams will have multiple data files
#' which require merging prior to analysis. This function
#' takes the nested list created by `read_in` and merges
#' it.
#' @param df_list list of named dataframes returned from
#'   `read_in`
#' @param multi_country Boolean to allow data from multiple
#'   countries to be combined
#'
#' @importFrom stats setNames
#'
#' @return Named list of data frames
#'

merge_imported <- function(df_list, multi_country = FALSE) {
  if (length(df_list) == 1) {
    return(
      df_list[[1]]
    )
  }

  df_names <- names(df_list[[1]])

  cli::cli_alert_info("Merging study files")

  merged <- Reduce(
    x = df_list,
    f = \(a, b) {
      lapply(
        df_names,
        \(name) {
          rbind(
            a[[name]],
            b[[name]]
          )
        }
      ) |> setNames(df_names)
    }
  ) |> setNames(df_names)

  has_multiple_countries <- "baseline" %in% names(merged) &&
    is.data.frame(merged$baseline) &&
    "cntry" %in% names(merged$baseline) &&
    length(unique(merged$baseline$cntry)) > 1

  if (has_multiple_countries && multi_country == FALSE) {
    cli::cli_alert_info(
      "Remove study data files or change `multi_country` flag to TRUE"
    )
    cli::cli_abort("Multiple countries are included in these files")
  }

  return(merged)
}
