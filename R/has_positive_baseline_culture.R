#' Has positive baseline culture
#' @inheritParams create_conversion_variables
#'

has_positive_baseline_culture <- function(baseline, myco) {
  # filter myco
  cultures <- myco[which(
    myco$test_type == "culq"
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

  # return data frame with globalcordid and is_baseline_culture_positive
  return(output)
}


#' @noRd

is_baseline_culture_positive <- function(df) {
  # find diff time for each result from treatment start date
  diff_days <- difftime(
    df$datespecimen,
    df$trtstdat,
    units = "days"
  )

  # filter to keep only eligible results
  filtered <- df[which(
    diff_days < 7
  ), ]

  # check if any of the results are positive
  result <- any(
    filtered$result %in% c(1)
  )

  # return result
  return(
    data.frame(
      globalrecordid = unique(df$fkey),
      is_baseline_culture_positive = result
    )
  )
}
