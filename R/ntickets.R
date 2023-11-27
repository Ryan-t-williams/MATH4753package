
#' Title ntickets
#'
#'@importFrom stats density
#'@importFrom stats dnorm
#'@importFrom stats pbinom
#'@importFrom stats pnorm
#'@importFrom stats qnorm
#'@importFrom stats rbinom
#'@importFrom stats quantile
#'@importFrom stats uniroot
#'@importFrom grDevices rainbow
#'@importFrom graphics abline
#'@importFrom graphics barplot
#'@importFrom graphics curve
#'@importFrom graphics hist
#'@importFrom graphics layout
#'@importFrom graphics lines
#'@importFrom graphics mtext
#'@importFrom graphics par
#'@importFrom graphics points
#'@importFrom graphics polygon
#'@importFrom graphics segments
#'@importFrom graphics text
#'
#'
#'
#'
#'
#' @param N int, Number of Seats on the plane
#' @param gamma float, used to calculate confidence interval
#' @param p float, probability that an individual shows up
#'
#' @return plots showing the optimal amount of tickets to be sold for discrete and continuous, named list for nd,nc,N,p,and gamma
#' @export
#'
#' @examples
#' ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {

  # Objective function for calculating the probability of overbooking using binomial distribution
  obj_func_binomial <- function(n, N, p, gamma) {
    1- gamma-pbinom(N, size = n, prob = p)
  }
  # Objective function for calculating the probability of overbooking using normal distribution
  obj_func_norm <- function(n, N, p, gamma) {
    N + 0.5 - qnorm(1 - gamma, mean = p * n, sd = sqrt(n * p * (1 - p)))
  }

  # Function to find the optimal value of n using the appropriate discrete distribution
  find_optimal_n_discrete <- function(N, p, gamma) {
    n_values <- (N):(N + 100)  # Considering a range of n values
    probabilities <- pbinom(N, size = n_values, prob = p, lower.tail = FALSE)
    idx <- min(which(probabilities >= gamma))
    return(n_values[idx])
  }

  # Function to find the optimal value of n using the normal approximation
  find_optimal_n_continuous <- function(N, p, gamma) {
    objective_func <- function(n) {
      obj_func_norm(n, N, p, gamma)
    }
    result <- uniroot(objective_func, interval = c(N, N + 100))
    return(result$root)
  }


  # find optimal values of n
  nd <- find_optimal_n_discrete(N, p, gamma)
  nc <- find_optimal_n_continuous(N, p, gamma)

  # Print result
  named_list <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(named_list)

  # CreatE plot for the objective function
  plot_obj_func_discrete <- function(n) {
    obj_func_binomial(n, N, p, gamma)
  }
  plot_obj_func_continuous <- function(n) {
    1-gamma-pnorm(N, mean = p*(n-0.5), sd=sqrt(N*p*(1-p)))
  }

  n_values <- (N):(N + 100)  # Values of n for plotting the objective function
  obj_values_d <- sapply(n_values, plot_obj_func_discrete)
  #layout(matrix(c(2,1),nr=2,nc=1, byrow=TRUE))
  plot(n_values, obj_values_d, type = 'b',pch = 16, col = 'blue', xlab = 'n', ylab = 'Objective Function Value',
       main = paste('Objective Vs n to find optimal tickets sold (Discrete)', paste("(N =", N, ", gamma =", gamma, ")")),
       sub = paste("Optimal n (Discrete):", nd), xlim = c(N , N+N*0.1), cex.main = 0.8, cex.lab = 0.8)
  abline(h = 0, col = 'red', lty = 2)
  points(nd, 0, col = 'red')
  abline(v = nd, col = 'red', lty = 2)

  obj_values_c <- sapply(n_values, plot_obj_func_continuous)
  plot(n_values, obj_values_c, type = 'l', xlab = 'n', ylab = 'Objective Function Value',
       main = paste('Objective Vs n to find optimal tickets sold (Continuous)', paste("(N =", N, ", gamma =", gamma, ")")),
       sub = paste("Optimal n (Continuous):", nc), xlim = c(N , N + N*0.1),cex.main = 0.8, cex.lab = 0.8)
  abline(h = 0, col = 'red', lty = 2)
  points(nc, 0, col = 'red')
  abline(v = nc, col = 'red', lty = 2)

}
