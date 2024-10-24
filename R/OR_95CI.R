#' Odds ratios and 95 percent confidence intervals
#'
#' Returns odds ratios and 95 percent confidence intervals from a vector of coeﬀicient estimates and a vector of standard errors.
#'
#' @param coef A numeric vector of coefficient estimates (log-odds) from a statistical model.
#' @param se A numeric vector of standard errors corresponding to the coefficient estimates.
#' @param siglevel Significance level (e.g., 0.05 for a 95 percent confidence interval).
#' @param roundto Integer indicating the number of decimal places to round the results.
#'
#' @return A character vector where each element contains the odds ratio along with its confidence interval in the format: "OR (lower CI, upper CI)".
#' @export
#'
#' @examples
#' # load toydata
#' data(toydata)
#'
#' # fit a logistic regression model
#' mod <- glm(y ~ x1 + x2, data = toydata, family = binomial)
#'
#' # extract coefficients and standard errors
#' coef <- coef(mod)
#' se <- sqrt(diag(vcov(mod)))
#'
#' # calculate odds ratios and 95% confidence intervals
#' OR_95CI(coef, se, siglevel = 0.05, roundto = 2)
OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel/2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto), "(",
                     format(round(ORlcl, roundto), nsmall = roundto), ",",
                     format(round(ORucl, roundto), nsmall = roundto), ")")
  return(ORresult)
}
