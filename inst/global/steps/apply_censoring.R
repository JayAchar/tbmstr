apply_censoring <- function(df) {
  max_follow_up <- 22 * 30

  require_censoring <- which(df$eos_days > max_follow_up)
  # if there's an unsuccessful outcome which requires censoring,
  # this outcome should be changed to "No TB"
  df$eos_outcome[require_censoring] <- "No TB"
  df$event_fail[require_censoring] <- FALSE

  # eos_days should be < max_follow_up
  df$eos_days[require_censoring] <- max_follow_up

  cli::cli_alert_info("{length(require_censoring)} records censored
                      at {max_follow_up} days")

  return(df)
}
