days <- function(days) {
  as.POSIXct(
    days * 60 * 60 * 24,
    tz = "UTC"
  )
}

create_factor_outcome <- function(lvls) {
  return(
    function(str) {
      factor(str,
             levels = lvls
      )
    }
  )
}

custom_eos_outcome <- create_factor_outcome(internal$definitions$eos_levels)
custom_eot_outcome <- create_factor_outcome(internal$definitions$eot_levels)