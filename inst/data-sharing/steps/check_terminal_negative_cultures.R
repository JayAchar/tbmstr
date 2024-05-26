check_terminal_negative_cultures <- function(baseline, myco) {
  completed <- baseline[which(
    baseline$outcome == "Completed"
  ), c("globalrecordid", "trtendat")]

  all_myco <- myco[which(
    myco$fkey %in% completed$globalrecordid
  ), c("fkey", "datespecimen", "test_type", "result")]

  cultures <- all_myco[which(
    all_myco$test_type %in% c("culsld", "culqd") &
      all_myco$result %in% c("No growh", "MTB complex")
  ), ]

  df <- merge(
    cultures,
    completed,
    by.x = "fkey",
    by.y = "globalrecordid",
    all.x = TRUE
  )
  df$diff <- diff_days(
    df$trtendat,
    df$datespecimen
  )

  tx_results <- df[which(df$diff >= -150 &
    df$diff <= 0), ]

  results_lst <- split(tx_results, tx_results$fkey)

  has_cultures_lst <- lapply(
    results_lst,
    FUN = function(results) {
      results <- results[rev(order(results$datespecimen)), ]
      has_terminal_negative_cultures(results)
    }
  )

  data.frame(
    globalrecordid = names(has_cultures_lst),
    has_cultures = unlist(has_cultures_lst)
  )
}
