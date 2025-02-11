#' Fit bipartite network model
#'
#' @param data A named list containing the required data, as obtained from
#'  [prepare_data()].
#' @param plant_effort How to model the sampling effort allocated to each plant. If unknown, `param` (default) estimates a single parameter during the modelling step that applies to all plants, \eqn{C}. Alternatively, "data" allows measured values of sampling effort to be supplied in `data`, \eqn{C_i}.
#' @param animal_pref How to model the increase in visits of animals to plants with which they share a link relative to unlinked plants. `fixed` (default), fits a single preference parameter for all animals during the modelling step, \eqn{r}. `varying` fits a separate preference parameter for each animal type, \eqn{r_i}.
#' @param plant_abun How the model treats the abundances of interacting plants. `param` (default) treats these abundances as a parameter that the model estimates, \eqn{sigma_i}. `data` treats these abundances as measured and requires the user to supply the absolute abundances of each plant in `data`.
#' @param animal_abun How the model treats the abundances of interacting animals. `param` (default) treats these abundances as a parameter that the model estimates, \eqn{sigma_i}. `data` treats these abundances as measured and requires the user to supply the absolute abundances of each animal in `data`.
#' @param beta Rate parameter of exponential prior on the preference parameter or parameters \eqn{r} or \eqn{r_i}. The default, 0.01, is suitable for small counts and should be increased for larger counts. Use `plot_prior` to visualize.
#' @param alpha_sigma Concentration parameters of Dirichlet prior on the abundances of interacting plants, \eqn{sigma_i}, when `plant_abun = "param"`. Ignored otherwise. Defaults to 1.
#' @param alpha_tau Concentration parameters of Dirichlet prior on the abundances of interacting plants, \eqn{tau_i}, when `animal_abun = "param"`. Ignored otherwise. Defaults to 1.
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
#'                  plant_abun = "param", animal_abun = "param")

fit_model <- function(data = NULL,
                      plant_effort = c("param", "data"),
                      animal_pref = c("fixed", "varying"),
                      plant_abun = c("param", "data"),
                      animal_abun = c("param", "data"),
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
  if(plant_abun == "data") {
    if(is.null(data$abun_p)) stop('Plant abundances, abun_p, must be supplied in `data` when plant_abun = "data"')
  }
  if(animal_abun == "data") {
    if(is.null(data$abun_a)) stop('Animal abundances, abun_a, must be supplied in `data` when animal_abun = "data"')
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
