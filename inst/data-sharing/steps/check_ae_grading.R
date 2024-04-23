check_ae_grading <- function(lst) {
  # check qtcf grading in baseline dataframe
  # filter baseline
  baseline <- lst$baseline[
    lst$baseline$cntry == "Belarus",
    c("globalrecordid", "qt", "qtgrd")
  ]

  if (setequal(baseline$qtgrd, match_grade(baseline$qt)) == FALSE) {
    cli::cli_alert_danger("Baseline QTcF AEs incorrect for Belaurs")
    stop()
  }

  cli::cli_alert_success("Baseline QTcF AEs checked for Belarus")


  # merge adverse and monitor
  adverse <- lst$adverse[
    which(lst$adverse$globalrecordid %in% baseline$globalrecordid &
      lst$adverse$aeterm == "QT interval prolongation"),
  ]

  monitor <- lst$monitor[
    which(lst$monitor$fkey %in% baseline$globalrecordid),
  ]

  merged <- merge(
    monitor,
    adverse,
    by.x = c("fkey", "cldat"),
    by.y = c("globalrecordid", "aeonsetdt"),
    all.x = TRUE
  )

  cleaned <- merged[!is.na(merged$uniquerowid), ]

  cleaned$calculated_grade <- match_grade(cleaned$qtcffu)

  if (setequal(cleaned$calculated_grade, cleaned$severity) == FALSE) {
    cli::cli_alert_danger("Baseline QTcF AEs incorrect for Belaurs")
    stop()
  }


  lst
}
