eot_failure <- c("Failed", "Died", "Lost to follow-up")

eos_failure <- c(
  "Recurrence", "Died", "Treatment failure",
  "Treatment LTFU"
)

definitions <- list(
  eos_levels = c(
    "No TB", eos_failure,
    "Not evaluated"
  ),
  eot_levels = c(
    "Cured", "Completed", eot_failure,
    "Not evaluted", "Withdrawn"
  ),
  eos_failure = eos_failure,
  eot_failure = eot_failure
)
