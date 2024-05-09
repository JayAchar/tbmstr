#' Has positive baseline culture
#' @inheritParams create_conversion_variables
#' @param culture_type string defining eligible culture methods to use
#' @param var_suffix string suffix for returned variable

has_positive_baseline_culture <- function(baseline, myco,
                                          culture_type = "culq",
                                          var_suffix = NULL) {
  # filter myco
  cultures <- myco[which(
    myco$test_type %in% culture_type
  ), ]

  # only keep relevant culture results
  cultures <- cultures[which(
    cultures$result %in% list(1, 3, "MTB complex", "No growh")
  ), ]

  df <- merge(
    cultures,
    baseline[, c("globalrecordid", "trtstdat")],
    by.x = "fkey",
    by.y = "globalrecordid",
    all.x = TRUE
  )

  # split by participant
  sp_lst <- split(
    df,
    df$fkey
  )

  # iterate over each participant's results
  results <- lapply(
    X = sp_lst,
    FUN = \(cultures) {
      is_baseline_culture_positive(cultures)
    }
  )

  # combine partipant results into data frame
  output <- Reduce(
    x = results,
    f = rbind
  )

  stopifnot("is_baseline_culture_positive" %in% names(output))

  if (!is.null(var_suffix)) {
    names(output)[names(output) == "is_baseline_culture_positive"] <- paste0(
      "is_baseline_culture_positive_", var_suffix
    )
  }

  # return data frame with globalcordid and is_baseline_culture_positive
  return(output)
}


#' @noRd

is_baseline_culture_positive <- function(df) {
  stopifnot(
    all(unique(df$result) %in% list(1, 3, "MTB complex", "No growh"))
  )

  # find diff time for each result from treatment start date
  diff_days <- diff_days(
    df$trtstdat,
    df$datespecimen
  )

  # filter to keep only eligible results
  filtered <- df[which(
    diff_days < 7
  ), ]

  # check if any of the results are positive
  result <- any(
    filtered$result %in% c(1, "MTB complex")
  )

  # return result
  return(
    data.frame(
      globalrecordid = unique(df$fkey),
      is_baseline_culture_positive = result
    )
  )
}
