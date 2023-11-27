#' Title myci
#'
#' This function calculates a 95% confidence interval for the population mean (\mu)
#' from a single sample.
#'
#' @param x A vector representing the sample data
#'
#' @return A vector containing the lower and upper bounds of the 95% confidence interval
#'
#' @importFrom stats qt
#' @importFrom stats sd
#'
#'
#' @export
#'
#' @examples
#' set.seed(23)
#' x <- rnorm(30, mean = 10, sd = 12)
#' myci(x)
#'
myci <- function(x) {
  n <- length(x)      # Sample size
  mean_x <- mean(x)   # Sample mean
  sd_x <- sd(x)       # Sample standard deviation

  # Calculate standard error (SE) of the mean
  SE <- sd_x / sqrt(n)

  # Calculate t-value for a 95% confidence interval
  t_value <- qt(0.975, df = n - 1)

  # Calculate the confidence interval for the population mean
  CI <- mean_x + c(-1, 1) * t_value * SE

  return(CI)
}
