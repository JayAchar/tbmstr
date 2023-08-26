#' Reshape EoS outcome variables
#'
#' @param df data frame containing unique ID, EoS outcomes and
#'   EoS evaluation dates
#'
#' @importFrom stats reshape
#' @return data frame of outcome data in long format
#'
prepare_eos_outcomes <- function(df) {
  status_vars <- grep(
    pattern = "^stat[0-9]+$",
    x = names(df),
    value = TRUE
  )

  date_vars <- grep(
    pattern = "^evldat[0-9]+$",
    x = names(df),
    value = TRUE
  )

  output_df <- reshape(
    data = df,
    direction = "long",
    idvar = "globalrecordid",
    timevar = "date_var",
    v.names = "status",
    varying = status_vars,
    times = date_vars
  ) |> as.data.frame()

  dates <- vapply(
    seq_along(output_df$date_var),
    FUN = \(ind) {
      var <- output_df$date_var[ind]
      as.character(output_df[ind, var])
    },
    character(1)
  )

  output_df$fudat <- as.POSIXct(dates)
  output_df <- output_df[, -which(
    names(output_df) %in% c(date_vars, "date_var")
  )]
  row.names(output_df) <- seq_along(output_df$status)

  output_df
}
