#' Plot prior distribution for r (preference) parameter
#'
#' The `r` (preference) parameter in Young et al. model takes a prior
#' exponential distribution with rate = beta. Use this function to
#' visualise the prior distribution of `r` given the chosen beta.
#' Alternatively, if providing the fitted model, a plot comparing the
#' prior versus posterior will be produced.
#'
#' @param beta A number > 0. Rate of the exponential distribution.
#' @param fit A fitted model, as obtained from `fit_model()`.
#'
#' @return A plot
#' @export
#'
#' @examples
#' plot_prior(beta = 0.01)
#' plot_prior(beta = 0.001)
#'
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' plot_prior(fit = fit)

plot_prior <- function(beta = NULL, fit = NULL) {

  if (is.null(fit)) {
    prior <- stats::rexp(10000, rate = beta)
    graphics::hist(prior, freq = FALSE, breaks = 100,
         main = paste0("Prior probability for r (preference) parameter\n",
                       "with beta = ", beta))
  }

  if (!is.null(fit)) {
    beta.model <- get_beta(fit)
    if (is.numeric(beta) && !identical(beta, beta.model)) {
      warning("The beta used to fit the model is ", beta.model, ", not ", beta)
    }
    prior <- stats::rexp(10000, rate = beta.model)
    post <- get_posterior(fit, param = "preference")$preference
    post.dens <- graphics::hist(post, breaks = 100, plot = FALSE)
    post.max <- max(post.dens$density)

    graphics::hist(prior, freq = FALSE, breaks = 100, ylim = c(0, post.max),
         main = "Preference (r) parameter: prior (bars) vs posterior (red line)",
         xlab = "")
    graphics::lines(post.dens$mids, post.dens$density, col = "red")
  }

}



get_beta <- function(fit = NULL) {
  datafile <- readLines(fit$data_file())  # reading json
  beta.info <- datafile[grep("beta", datafile)]
  beta <- as.numeric(gsub('.*beta\": ', "", beta.info))
  return(beta)
}
