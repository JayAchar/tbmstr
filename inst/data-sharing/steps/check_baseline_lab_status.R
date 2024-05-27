check_baseline_lab_status <- function(baseline, myco) {
  # find all particpants with completed outcome - id & trtstdat
  completed <- baseline[
    which(baseline$outcome == "Completed"),
    c("globalrecordid", "trtstdat")
  ]
  # filter myco results by trtstdat and test_type - culture & xpert
  complete_myco <- merge(
    completed,
    myco[, c("fkey", "datespecimen", "test_type", "result")],
    by.x = "globalrecordid",
    by.y = "fkey",
    all.x = TRUE
  )

  cultures <- complete_myco[
    which(complete_myco$test_type %in% c("culsld", "culqd", "xpert1")),
  ]

  cultures$diff <- diff_days(
    cultures$trtstdat,
    cultures$datespecimen
  )

  eligible <- cultures[which(cultures$diff <= 7), ]

  c_lst <- split(
    eligible,
    eligible$globalrecordid
  )

  is_baseline_positive <- lapply(
    c_lst,
    FUN = \(pt) {
      any(
        grep("^MTB \\+|^MTB\\+|^MTB complex", pt$result)
      )
    }
  )

  data.frame(
    globalrecordid = names(is_baseline_positive),
    is_baseline_positive = unlist(is_baseline_positive)
  )
}
