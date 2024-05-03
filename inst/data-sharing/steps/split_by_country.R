split_by_country <- function(lst) {
  table_names <- names(lst)

  # get vector of country names
  all_countries <- as.character(
    unique(
      lst$baseline$cntry
    )
  )

  # for each counry get list of unique participant IDs
  record_ids <- lapply(
    X = all_countries,
    FUN = \(country_name) {
      lst$baseline[
        which(lst$baseline$cntry == country_name),
        "globalrecordid"
      ]
    }
  ) |> setNames(all_countries)

  # find the identifying variable in each table
  id_column_names <- lapply(
    X = table_names,
    FUN = \(table_name) {
      vec <- vapply(
        seq_len(ncol(lst[[table_name]])),
        \(col_ind) {
          if (any(lst[[table_name]][[col_ind]] %in% record_ids$Armenia)) {
            return(col_ind)
          }
          return(0)
        }, numeric(1)
      )
      id_column_index <- vec[vec > 0]
      names(lst[[table_name]])[
        id_column_index
      ]
    }
  ) |> setNames(table_names)

  # for each country go through each table and keep
  # records which match by country
  lapply(
    X = all_countries,
    FUN = \(country_name) {
      lapply(
        X = names(id_column_names),
        FUN = \(table_name) {
          include <- lst[[table_name]][[id_column_names[[table_name]]]] %in%
            record_ids[[country_name]]
          lst[[table_name]][include, ]
        }
      ) |> setNames(names(id_column_names))
    }
  ) |>
    setNames(all_countries)
}
