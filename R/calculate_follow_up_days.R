#' Calculate follow-up time in days
#'
#' @param df data frame with baseline characteristics
#'
#' @return datafrme with baseline characterists and fu_days
#' numerical variable
#'

calculate_follow_up_days <- function(df) {
  # start
  c("trtstdat")

  # end
  end_dates <- c(
    "trtendat",
    "endat",
    "fuendat",
    "evldat3",
    "evldat6",
    "evldat9",
    "evldat12",
    "deathdat",
    "eos_date",
    "evaldat"
  )

  end_df <- df[, c(
    end_dates
  )]

  end_dates <- do.call(pmax, c(end_df, na.rm = TRUE))

  fu_days <- difftime(
    end_dates,
    df$trtstdat,
    units = "days"
  )

  df$fu_days <- as.numeric(fu_days)

  return(df)
}
