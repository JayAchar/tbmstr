#' Calculate time to culture conversion
#'
#' The mSTR study data contains information about each sputum result
#' and the study team's calculation for culture conversion.
#' This function compares the treatment start date with the study team's
#' recorded culture conversion date to generate a numerical result representing
#' the number of days until culture conversion.
#'
#' @param trtstdat date vector representing the treatment initation date
#' @param convdat date vector representing the study team's recorded culture
#' conversion date.
#' @param baseline  data frame of baseline participant characteristics
#' @param myco data frame of participant mycobacteriology results
#' @param lab boolean to signifiy whether lab results should be used to
#' calculate culture conversion dates
#'
#' @return numeric vector representing the number of days to culture conversion
#'

create_cc_days <- function(
    trtstdat,
    convdat,
    baseline,
    myco,
    lab = FALSE) {
  # TODO: cc_days should be censored at around 4 months based on
  # the failure outcome

  if (lab) {
    # rename variables (id, result, date) - filter
    # to only keep cultures with positive or negative results - record
    # result variable

    cultures <- merge(
      baseline[, c("globalrecordid", "trtstdat", "trtendat")],
      myco[
        which(myco$test_type == "culq"),
        c("fkey", "datespecimen", "specimen", "result")
      ],
      by.x = "globalrecordid",
      by.y = "fkey",
      all.x = TRUE
    )

    filtered <- cultures[which(
      cultures$specimen == "Sputum" &
        cultures$datespecimen > cultures$trtstdat &
        cultures$datespecimen < cultures$trtendat &
        cultures$result %in% c(1, 3)
    ), ]

    fdf <- filtered[, c("globalrecordid", "datespecimen", "result")]
    names(fdf) <- c("id", "date", "result")
    fdf$result[fdf$result == 3] <- 0


    sp_lst <- split(fdf, fdf$id)

    cc_dates_lst <- lapply(
      sp_lst,
      \(cultures) {
        if(unique(cultures$id) == "00b4ad11-4694-404d-9c87-3c223fa72b55") {
          browser()
        }
        calculate_conversion(
          cultures,
          tolerance = 30
        )
      }
    )

    cc_dates <- Reduce(
      x = cc_dates_lst,
      f = rbind
    )

    names(cc_dates) <- c("globalrecordid", "lab_cc_date")

    cc_dates_trt <- merge(
      cc_dates,
      baseline,
      by = "globalrecordid",
      all.y = TRUE
    )

    # calculate culture conversion days
    cc_dates_trt$lab_cc_days <- as.numeric(
      difftime(
        cc_dates_trt$lab_cc_date,
        cc_dates_trt$trtstdat,
        units = "days"
      )
    )

    cc_dates_trt$lab_cc_date <- NULL

    return(cc_dates_trt)
  }



  if (!lab) {
    is_trtstdat_class_ok <- all(class(trtstdat) %in% c("POSIXct", "POSIXt"))
    is_convdat_class_ok <- all(class(convdat) %in% c("POSIXct", "POSIXt"))

    if (!all(is_trtstdat_class_ok, is_convdat_class_ok)) {
      cli::cli_abort("Inputs should be of type `POSIXct`.")
    }

    if (length(trtstdat) != length(convdat)) {
      cli::cli_abort("Input arguments should be the same length.")
    }

    return(as.numeric(difftime(convdat, trtstdat, units = "days")))
  }
}
