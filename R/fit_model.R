#' Fit model
#'
#' @param data A named list containing the required data, as obtained from
#'  [prepare_data()].
#' @param vary_plant_effort Whether to account for varying sampling effort across plants. If FALSE (default), all plants are assumed to be sampled equally. If TRUE, sampling effort for each species must be supplied in the data.
#' @param vary_animal_pref Whether to allow animals to have different preference strengths for linked plants. If FALSE (default), a single preference parameter is estimated for all animals. If TRUE, separate preference parameters are fit for each animal.
#' @param plant_abun How model considers plant abundances. "estimate" (default) estimates the latent relative abundance of each plant type. "relative" allows the user to pass a simplex containing the relative abundances of each plant. "absolute" allows the user to supply the absolute abundances of each plant.
#' @param animal_abun How model considers animal abundances. "estimate" (default) estimates the latent relative abundance of each animal type. "relative" allows the user to pass a simplex containing the relative abundances of each animal. "absolute" allows the user to supply the absolute abundances of each animal.
#' @param beta Rate of exponential prior on `r` (preference) parameter.
#' @param alpha_sigma Concentration parameter of Dirichlet prior on `sigma` (relative abundances of plants). Only used when `plant_abun = "estimate"`.
#' @param alpha_tau Concentration parameter of Dirichlet prior on `tau` (relative abundances of animals). Only used when `animal_abun = "estimate"`.
#' Default beta is 0.01. Increase it if you have large count numbers
#' (can examine the resultant prior using [plot_prior()]).
#' @param ... Further arguments passed to [cmdstanr::sample()], like `iter_warmup`,
#' `iter_sampling`, or `thin`, among others.
#'
#' @return A fitted model ([cmdstanr::CmdStanMCMC()] object).
#' @export
#' @import cmdstanr
#'
#' @examplesIf interactive()
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt)

fit_model <- function(data = NULL,
                      vary_plant_effort = FALSE,
                      vary_animal_pref = FALSE,
                      plant_abun = c("estimate", "relative", "absolute"),
                      animal_abun = c("estimate", "relative", "absolute"),
                      beta = 0.01,
                      alpha_sigma = 1,
                      alpha_tau = 1,
                      ...) {

  if (is.null(data)) {
    stop("Please provide the data as produced by 'prepare_data()'")
  }

  model <- match.arg(model)  # TODO: remove this later to allow for user models

  if (model %in% c("sampling_effort", "Young2021", "varying_preferences")) {
    mod.stan <- system.file(paste0("models/", model, ".stan"), package = "BayesianWebs")
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
