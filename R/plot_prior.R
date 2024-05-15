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
    if (is.null(beta)) {
      stop("Please provide either a value for beta or a fitted model")
    }
    df <- data.frame(prior = stats::rexp(10000, rate = beta))
    gg <- ggplot2::ggplot(df) +
      ggplot2::geom_density(ggplot2::aes(prior), bounds = c(0, Inf), fill = "grey40") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Prior probability for r (preference) parameter ",
                                   "with beta = ", beta),
                    x = "")
  }

  if (!is.null(fit)) {
    beta.model <- get_beta(fit)
    if (is.numeric(beta) && !identical(beta, beta.model)) {
      warning("The beta used to fit the model is ", beta.model, ", not ", beta)
    }
    df.prior <- data.frame(prior = stats::rexp(10000, rate = beta.model))
    df.post <- get_posterior(fit, param = "preference")[, "preference"]

    gg <- ggplot2::ggplot(df.prior) +
      ggplot2::geom_density(ggplot2::aes(prior), bounds = c(0, Inf), fill = "grey40") +
      ggplot2::geom_density(ggplot2::aes(preference), bounds = c(0, Inf), fill = "grey90",
                            data = df.post, alpha = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Preference (r) parameter: prior (dark grey) vs posterior (light grey) distribution",
                    x = "")

  }

  return(gg)

}



get_beta <- function(fit = NULL) {
  datafile <- readLines(fit$data_file())  # reading json
  beta.info <- datafile[grep("beta", datafile)]
  beta <- as.numeric(gsub('.*beta\": ', "", beta.info))
  return(beta)
}
