#' Merge and Filter by start date
#'
#' Merge a data frame with the `baseline` data frame then filter entries
#' according to the treatment initaition date
#'
#' @param baseline Data frame of baseline study data.
#' @param target Data frame of target study data.
#' @param target_date_var String representing the date variable in the target.
#'
#' @return Data frame
merge_and_filter_by_start_date <- function(baseline, target, target_date_var) {
  if (!(target_date_var %in% names(target))) {
    cli::cli_abort(
      "The `{target_date_var}` variable was not found in the target data frame."
    )
  }

  merged <- merge(
    baseline,
    target,
    by.x = "globalrecordid",
    by.y = "fkey",
    all.x = TRUE
  )

  merged$trtstdat <- transform_to_date(merged$trtstdat)
  merged[[target_date_var]] <- transform_to_date(merged[[target_date_var]])
  # remove all results that are after treatment initiation
  merged[which(
    merged[[target_date_var]] <= merged$trtstdat
  ), ]
}

#' @noRd
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


#' @noRd
#' @importFrom cli cli_alert_info
alert_info <- function(msg) {
  if (!is_testing()) {
    cli::cli_alert_info(msg)
  }
}

#' @noRd
create_binary_tx_outcome <- function(outcome) {
  var <- factor(ifelse(
    outcome %in% list("Cured", "Completed", 1, 2),
    "Successful", "Unsuccessful"
  ), levels = c("Successful", "Unsuccessful"))
  var[which(is.na(outcome))] <- NA
  var
}

#' @noRd
#' @importFrom stats as.formula
create_formula <- function(outcome, predictors) {
  str <- paste(outcome,
    paste(predictors, collapse = "+"),
    sep = " ~ "
  )
  as.formula(str)
}


#' Relevel variables
#'
#' Choose base level for variables
#' @param df data frame
#' @param config list
#' @importFrom stats relevel
#' @export
relevel_variables <- function(df, config) {
  stopifnot(
    is.list(config),
    is.data.frame(df)
  )

  keys <- names(config)

  df[keys] <- lapply(
    keys,
    \(key) {
      relevel(df[[key]], ref = config[[key]])
    }
  )
  df
}

#' @noRd

apply_manual_adjustments <- function(lst, adjustments) {
  stopifnot(
    is.list(lst),
    is.list(adjustments)
  )

  modified <- Reduce(
    f = \(init, al) {
      id_var <- al[["id"]]
      init[[al$name]] <- Reduce(
        x = al$adjustments,
        f = \(i, aa) {
          target_row <- which(i[[id_var]] == aa$id)
          i[target_row, aa$var] <- aa$value
          i
        },
        init = lst[[al$name]]
      )
      init
    },
    x = adjustments,
    init = lst
  )
  modified
}

#' Calculate days between two dates
#'
#' @param start start date
#' @param end end date
#' @export

diff_days <- function(start, end) {
  as.numeric(difftime(end, start,
    units = "days"
  ))
}

#' @noRd

factor_eos_outcome <- function(outcome_str) {
  # definitions from package data
  defs <- internal$definitions
  factor(outcome_str,
    levels = defs$eos_levels
  )
}


#' @noRd
#' @importFrom scales label_number

p_value_formatter <- function(number, digits) {
  round_to <- digits - ceiling(log10(abs(number)))

  value <- round(number, round_to)

  accuracy_arg <- 1 / 10^round_to

  ## Construct a labeller within the function run
  labeller <- scales::label_number(accuracy = accuracy_arg)

  label <- labeller(value)

  return(label)
}

#' Format p-values for result tables
#'
#' @description The format has been aligned with the request from the Lancet
#'   ID editors.
#'
#' @param x vector of numeric p-values
#' @export

format_p_values <- function(x) {
  vapply(x,
    FUN.VALUE = character(1),
    FUN = \(val) {
      if (is.na(val)) {
        return("")
      }
      if (val < 0.001 && val >= 0.0001) {
        return(
          p_value_formatter(val, 1)
        )
      }
      if (val > 0.0001) {
        return(
          p_value_formatter(val, 2)
        )
      }
      return("<0.0001")
    }
  )
}
