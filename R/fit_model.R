#' Fit model
#'
#' @param data A named list containing the required data, as obtained from
#'  [prepare_data()].
#' @param model character. Path to file describing the Stan model. Use in case
#' you want to use a modified Stan model. Otherwise using "default" model provided
#' with the package.
#' @param beta Rate of exponential prior on `r` (preference) parameter.
#' Default beta is 0.01. Increase it if you have large count numbers.
#' (Could use [plot_prior()] to examine the resultant prior.
#' @param ... Further arguments for [cmdstanr::sample()], like `iter_warmup`,
#' `iter_sampling`, or `thin`, among others. It is recommended to increase the
#' number of iterations (e.g. iter_sampling = 10000).
#'
#' @return A fitted model ([cmdstanr::CmdStanMCMC()] object).
#' @export
#' @import cmdstanr
#'
#' @examples
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt)

fit_model <- function(data = NULL, model = "default", beta = 0.01, ...) {

  if (model == "default") {
    mod.stan <- system.file("models/model_effort.stan", package = "BayesianNetworks")
  } else {
    mod.stan <- model
  }

  ## Add beta to data file
  stopifnot(beta > 0)
  data$beta <- beta


  ## compile model
  model.eff <- cmdstanr::cmdstan_model(mod.stan)

  fit <- model.eff$sample(data, ...)

}
