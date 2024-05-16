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


  # recalculate AE severity
  blr_ids <- baseline$globalrecordid

  lst$adverse$temp_id <- seq_along(lst$adverse$globalrecordid)

  # filter adverse
  tadv <- lst$adverse[which(lst$adverse$globalrecordid %in% blr_ids), ]
  adv <- tadv[which(tadv$aeterm == "QT interval prolongation"), ]

  # apply comment number to severity
  adv$reassigned_grade <- as.numeric(strcapture(
    pattern = " ([0-9]{1}) ",
    x = adv$aecomment,
    proto = list(key = character())
  )$key)

  adv$updated_severity <- vapply(seq_along(adv$temp_id),
    FUN = \(row) {
      if (is.na(adv$reassigned_grade[row])) {
        return(NA_character_)
      }

      paste0(
        "Grade ",
        paste(rep("I", adv$reassigned_grade[row]), collapse = "")
      )
    },
    character(1)
  )

  adv$reassigned_grade <- NULL

  adv$severity <- as.character(vapply(
    seq_along(adv$temp_id),
    \(row) {
      if (is.na(adv$updated_severity[row])) {
        if (!is.na(adv$aecomment[row])) {
          return("Grade 0")
        }

        return(adv$severity[row])
      }

      adv$updated_severity[row]
    }, character(1)
  ))

  # remove rows from original adverse df
  lst$adverse <- lst$adverse[-(adv$temp_id), ]

  adv$updated_severity <- NULL
  lst$adverse <- rbind(lst$adverse, adv)

  lst$adverse$severity[which(
    lst$adverse$severity == "Grade IIII"
  )] <- "Grade IV"

  remove_rows <- which(lst$adverse$severity == "Grade 0")

  lst$adverse <- lst$adverse[-remove_rows, ]

  is_correct_temp_id <- length(unique(lst$adverse$temp_id)) == nrow(lst$adverse)

  if (!is_correct_temp_id) {
    cli::cli_alert_danger(
      "Recombining adverse data frame didn't work correctly"
    )
    stop()
  }

  lst$adverse$temp_id <- NULL

  lst
}
