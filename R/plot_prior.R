#' Plot prior distribution for r (preference) parameter
#'
#' The r (preference) parameter in Young et al. model takes a prior
#' exponential distribution with rate = beta. Use this function to
#' visualise the prior distribution of `r` given the chosen beta.
#'
#' @param beta A number > 0. Rate of the exponential distribution.
#'
#' @return A plot
#' @export
#'
#' @examples
#' plot_prior(beta = 0.01)
#' plot_prior(beta = 0.001)

plot_prior <- function(beta = 0.01) {
  hist(rexp(10000, rate = beta), freq = FALSE, breaks = 100,
       main = paste0("Prior probability for r (preference) parameter\n",
                    "with beta = ", beta))

}
