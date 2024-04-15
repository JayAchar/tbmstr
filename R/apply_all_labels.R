#' Apply all labels
#'
#' Convert all numeric variables to factors based on
#' meta-data available in the package look up table
#'
#' @param lst List of data frames containing study data
#'
#' @importFrom cli cli_abort
#'
#' @export

apply_all_labels <- function(lst) {
  if (!is.list(lst) || is.data.frame(lst)) {
    cli::cli_abort("Input must be a list of data frames")
  }
  if (length(lst) < 1) {
    cli::cli_abort("Input list must contain at least 1 item")
  }

  are_all_dfs <- all(vapply(
    lst,
    \(df) is.data.frame(df),
    logical(1)
  ))

  if (!are_all_dfs) {
    cli::cli_abort("All items in the input list must be data frames")
  }

  alert_info("Ukrainian withdrawls re-coded")

  lapply(
    lst,
    \(df) {
      # conmpare df names with lut
      included_vars <- names(df)[names(df) %in% internal$lut$name]
      # replace vars in place
      df[included_vars] <- lapply(
        included_vars,
        \(var_name) {
          labelled <- apply_labels(
            df = df,
            variable = var_name,
            convert_to_factor = TRUE
          )

          if (var_name == "outcome") {
            ukr_target_rows <- which(df$cntry == 10 & df$outcome == 6)
            labelled[ukr_target_rows] <- "Withdrawn"
          }
          return(labelled)
        }
      )
      # custom labels for myco results aggregated variable
      if (all(c("test_type", "result") %in% names(df))) {
        df$test_type[which(df$test_type == "culq")] <- "culqd"

        df$result <- vapply(
          seq_along(df$result),
          FUN.VALUE = character(1),
          FUN = \(cnt) {
            if (is.na(df$result[cnt])) {
              return("Not done")
            }
            result <- internal$lut$description[which(
              internal$lut$name == df$test_type[cnt] &
                internal$lut$value == df$result[cnt]
            )]
            return(result)
          }
        )
      }

      # custom labels for change data
      if (all(c("aedrug", "aestop") %in% names(df))) {
        print("change found")
        lut <- internal$lut
        lut <- lut[which(
          grepl(".*[0-9]$", lut$name)
        ), ]
        lut$name <- gsub("[0-9]*$", "", lut$name)
        lut <- lut[!duplicated(lut), ]
        included_vars <- names(df)[names(df) %in% lut$name]
        df[included_vars] <- lapply(
          included_vars,
          \(var_name) {
            labelled <- apply_labels(df,
              var_name,
              lut = lut,
              convert_to_factor = TRUE
            )
            return(labelled)
          }
        )
      }

      return(df)
    }
  )
}
