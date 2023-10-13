days <- function(days) {
  as.POSIXct(
    days * 60 * 60 * 24,
    tz = "UTC"
  )
}
