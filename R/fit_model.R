#' Fit model
#'
#' @param data A named list containing the required data, as obtained from
#'  `prepare_data()`.
#' @param ... Further arguments for `cmdstanr::sample()`, like `iter_warmup`,
#' `iter_sampling`, or `thin`, among others.
#'
#' @return A fitted model (`cmdstanr::CmdStanMCMC()` object).
#' @export
#' @import cmdstanr
#'
#' @examples
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt)

fit_model <- function(data = NULL, ...) {

  mod.stan <- system.file("models/model_effort.stan", package = "BayesianNetworks")

  model.eff <- cmdstanr::cmdstan_model(mod.stan)

  fit <- model.eff$sample(data, ...)

}
