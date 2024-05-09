#' Calculate Likelihood Ratio statistic
#'
#' Calculate the LRT p-value for each predictor variable in a
#' Cox PH model
#'
#' @param model Cox PH model
#' @param df data frame of input daata
#'
#' @importFrom survival coxph
#' @importFrom lmtest lrtest
#'
#' @return named list of p-values - one for each predictor
#' @export
#'
calculate_lr_statistic <- function(model, df) {
  predictors <- names(model$xlevels)
  lapply(
    predictors,
    \(varname) {
      original_predictors <- paste(
        paste(predictors, collapse = " + "),
        " + survival::frailty(cntry, distribution = \"gaussian\")"
      )
      rhs <- paste(original_predictors, varname, sep = " - ")
      mod_form <- as.formula(paste("survival::Surv(eos_days, event_fail)",
        rhs,
        sep = "~"
      ))
      simple <- survival::coxph(mod_form, data = df)
      lt <- lmtest::lrtest(simple, model)
      p_vals <- lt$`Pr(>Chisq)`
      p <- p_vals[which(!is.na(p_vals))]
      format_p_values(p)
    }
  ) |> setNames(predictors)
}
