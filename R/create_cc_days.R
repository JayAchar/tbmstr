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
#' @param culture_type string vector to define valid culture types
#' @param var_suffix string suffix for returned variable
#'
#' @return numeric vector representing the number of days to culture conversion
#'

create_cc_days <- function(
    trtstdat = NULL,
    convdat = NULL,
    baseline,
    myco,
    lab = FALSE,
    culture_type = "culq",
    var_suffix = "lq") {
  if (lab) {
    # rename variables (id, result, date) - filter
    # to only keep cultures with positive or negative results - record
    # result variable

    cultures <- merge(
      baseline[, c("globalrecordid", "trtstdat", "trtendat")],
      myco[
        which(myco$test_type %in% culture_type),
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
        (cultures$result %in% list(1, 3, "MTB complex", "No growh"))
    ), ]

    fdf <- filtered[, c("globalrecordid", "datespecimen", "result")]
    names(fdf) <- c("id", "date", "result")
    fdf$result[fdf$result == 3] <- 0


    sp_lst <- split(fdf, fdf$id)

    cc_dates_lst <- lapply(
      sp_lst,
      \(cultures) {
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

    cc_var_name <- paste0("lab_cc_date_", var_suffix)

    names(cc_dates) <- c("globalrecordid", cc_var_name)

    cc_dates_trt <- merge(
      cc_dates,
      baseline[, c("globalrecordid", "trtstdat")],
      by = "globalrecordid",
      all.y = TRUE
    )

    # calculate culture conversion days
    cc_days_var <- paste0("lab_cc_days_", var_suffix)

    cc_dates_trt[cc_days_var] <- diff_days(
      cc_dates_trt$trtstdat,
      cc_dates_trt[[cc_var_name]]
    )

    cc_dates_trt$trtstdat <- NULL

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

    return(diff_days(
      trtstdat,
      convdat
    ))
  }
}
