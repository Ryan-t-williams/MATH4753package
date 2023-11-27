
#' Title myncurve
#'
#' @param mu integer for mu
#' @param sigma interger for sigma
#' @param a integer for a
#'
#' @return a graph with a filled in section, and the probability in a list
#' @export
#'
#' @examples
#' myncurve(mu=1, sigma=1, a=1)
myncurve = function(mu, sigma, a) {
  x_values <- seq(mu - 3 * sigma, a, by = 0.01)
  y_values <- dnorm(x_values, mean = mu, sd = sigma)

  # Plot the curve
  plot(x_values, y_values, type = "l", xlim = c(mu - 3 * sigma, mu + 3 * sigma), main = "Normal Distribution Curve", xlab = "X", ylab = "Density")

  # Shade the area under the curve from negative infinity to x=a
  polygon(c(x_values, rev(x_values)), c(rep(0, length(x_values)), rev(y_values)), col = "lightblue")

  # Calculate the probability (P(X <= a))
  probability <- round(pnorm(a, mean = mu, sd = sigma), 4)

  # Return the probability in a list
  result <- list(Probability = probability)
  return(result)
}
