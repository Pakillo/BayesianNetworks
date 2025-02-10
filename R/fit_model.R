#' Fit bipartite network model
#'
#' @param data A named list containing the required data, as obtained from
#'  [prepare_data()].
#' @param plant_effort The sampling effort allocated to each plant. If unknown, `param` (default) estimates a single parameter during the modelling step that applies to all plants. Alternatively, "data" allows measured values of sampling effort to be supplied in `data`.
#' @param animal_pref Controls the relative preference of animals for linked plants. `fixed` (default), fits a single preference parameter for all animals during the modelling step. `varying` fits a separate preference parameter for each animal type.
#' @param plant_abun Controls how the model accounts for plant abundances. `estimate` (default) estimates the latent relative abundance of each plant type. `relative` allows the user to pass a simplex containing the relative abundances of each plant. `absolute` allows the user to supply the absolute abundances of each plant.
#' @param animal_abun Controls how the model accounts for animal abundances. "estimate" (default) estimates the latent relative abundance of each animal type. "relative" allows the user to pass a simplex containing the relative abundances of each animal. `absolute` allows the user to supply the absolute abundances of each animal.
#' @param beta Rate parameter of exponential prior on `r` (preference) parameter. The defaults, 0.01, is suitable for small counts and should be increased for larger counts. Use `plot_prior` to visualize.
#' @param alpha_sigma Concentration parameter of Dirichlet prior on `sigma` (relative abundances of plants). Only used when `plant_abun = "estimate"`. Defaults to 1. Use `plot_prior` to visualize.
#' @param alpha_tau Concentration parameter of Dirichlet prior on `tau` (relative abundances of animals). Only used when `animal_abun = "estimate"`. Defaults to 1.  Use `plot_prior` to visualize.
#' @param ... Further arguments passed to [cmdstanr::sample()], like `iter_warmup`,
#' `iter_sampling`, or `thin`, among others.
#'
#' @return A fitted model ([cmdstanr::CmdStanMCMC()] object).
#' @export
#' @import cmdstanr
#'
#' @details
#' This function allows fits generalized variations of the bipartite network structure model of Young et al. 2021 [<doi:10.1038/s41467-021-24149-x>](https://doi.org/10.1038%2Fs41467-021-24149-x).
#'
#' @examplesIf interactive()
#' data(web)
#' dt <- prepare_data(mat = web, plant_effort = rep(20, nrow(web)))
#' # fit the model used by Young et al. 2021
#' fit <- fit_model(dt, plant_effort = "param", animal_pref = "fixed",
#'                  plant_abun = "estimate", animal_abun = "estimate")

fit_model <- function(data = NULL,
                      plant_effort = c("param", "data"),
                      animal_pref = c("fixed", "varying"),
                      plant_abun = c("estimate", "relative", "absolute"),
                      animal_abun = c("estimate", "relative", "absolute"),
                      beta = 0.01,
                      alpha_sigma = 1,
                      alpha_tau = 1,
                      ...) {

  if (is.null(data)) {
    stop("Please provide the data as produced by 'prepare_data()'")
  }

  plant_effort <- match.arg(plant_effort)
  animal_pref <- match.arg(animal_pref)
  plant_abun <- match.arg(plant_abun)
  animal_abun <- match.arg(animal_abun)

  ## Check that data provided are suitable for the specified model variant
  if(plant_effort == "data" & is.null(data$C)) stop('Sampling effort, C, must be supplied in data when plant_effort = "data"')
  if(plant_abun == "relative") {
    if(is.null(data$sigma)) stop('Plant relative abundances, sigma, must be supplied in data when plant_abun = "relative"')
    if(!all.equal(sum(data$sigma), 1)) stop("Plant relative abundances, sigma, must sum to 1.")
  }
  if(animal_abun == "relative") {
    if(is.null(data$tau)) stop('Animal relative abundances, tau, must be supplied in data when animal_abun = "relative"')
    if(!all.equal(sum(data$tau), 1)) stop("Animal relative abundances, tau, must sum to 1.")
  }
  if(plant_abun == "absolute") {
    if(is.null(data$sigma)) stop('Plant absolute abundances, sigma, must be supplied in data when plant_abun = "absolute"')
    if(all.equal(sum(data$sigma), 1)) warning("Plant absolute abundances, sigma, appear to be relative abundances.")
  }
  if(animal_abun == "absolute") {
    if(is.null(data$tau)) stop('Animal relative abundances, tau, must be supplied in data when animal_abun = "absolute"')
    if(all.equal(sum(data$tau), 1)) warning("Animal absolute abundances, tau, appear to be relative abundances.")
  }
  stopifnot(beta > 0, alpha_sigma > 0, alpha_tau > 0)

  ## Add priors to data list
  data$beta <- beta
  data$alpha_sigma <- alpha_sigma
  data$alpha_tau <- alpha_tau

  ## Make Stan code
  ## TODO: Pre-compile all model variants during package installation?
  mod.stan <- stancode(plant_effort = plant_effort,
                       animal_pref = animal_pref,
                       plant_abun = plant_abun,
                       animal_abun = animal_abun)
  tf <- cmdstanr::write_stan_file(mod.stan)

  ## Compile model
  model.eff <- cmdstanr::cmdstan_model(tf)

  ## Fit model
  fit <- model.eff$sample(data, ...)
}
