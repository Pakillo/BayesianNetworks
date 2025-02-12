#' Fit bipartite network model
#'
#' @param data A named list containing the required data, as obtained from
#'  [prepare_data()].
#' @param plant_effort How to model the sampling effort allocated to each plant. If unknown, `param` (default) estimates a single parameter during the modelling step that applies to all plants, \eqn{C}. Alternatively, "data" allows measured values of sampling effort to be supplied in `data`, \eqn{C_i}.
#' @param animal_pref How to model the increase in visits of animals to plants with which they share a link relative to unlinked plants. `fixed` (default), fits a single preference parameter for all animals during the modelling step, \eqn{r}. `varying` fits a separate preference parameter for each animal type, \eqn{r_i}.
#' @param plant_abun How the model treats the abundances of interacting plants. `param` (default) treats these abundances as a parameter that the model estimates, \eqn{sigma_i}. `data` treats these abundances as measured and requires the user to supply the absolute abundances of each plant in `data`.
#' @param animal_abun How the model treats the abundances of interacting animals. `param` (default) treats these abundances as a parameter that the model estimates, \eqn{sigma_i}. `data` treats these abundances as measured and requires the user to supply the absolute abundances of each animal in `data`.
#' @param beta Rate parameter of exponential prior on the preference parameter or parameters \eqn{r} or \eqn{r_i}. The default, 0.01, is suitable for small counts and should be increased for larger counts. Use `plot_prior` to visualize.
#' @param alpha_sigma Vector of concentration parameters for Dirichlet prior on the relative abundances of interacting plants, \eqn{sigma_i}, when `plant_abun = "param"`. Ignored otherwise. Defaults to 1.
#' @param alpha_tau Concentration parameters of Dirichlet prior on the relative abundances of interacting plants, \eqn{tau_i}, when `animal_abun = "param"`. Ignored otherwise. Defaults to 1.
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
#' Briefly, the observed interaction count between each plant and animal pair is treated as a random draw from a Poisson distribution with a mean of:
#' \deqn{\mu_{ij} = C_i\sigma_i\tau_j\left(1 + r_iB_{ij}\right).}
#' Here, the subscripts \eqn{i} and \eqn{j} are indices that correspond to the plant (row) and animal (column) species, respectively. \eqn{C_i} is the observer's sampling effort allocated to each plant species; we expect to see more interactions with plants the longer they are observed. \sigma_i and \tau_i are the relative contributions of each plant and animal to the total pool of interactions. We expect to observe higher interaction counts for species that contribute most to the interactions made by their guild. The observed interaction count should be compounded when species of both guilds are those that are most interactive. Importantly, these terms capture more than just each species' relative abundances in the their guild. An animal that is highly active but numerically rare animal could have a higher \eqn{\tau_i} than a numerically dominant, but less active animal. \eqn{r_i} is the increase in visits of animals to plants with which they share a link relative to unlinked plants, and \eqn{B_{ij}} is the inferred absence (0) or presence (1) of a link between the plant-animal pair.
#'
#' ## Prior recommendations
#' The default priors are very weak to accommodate a wide range of empirical datasets. They may fail for exceptional datasets and they do not capture system-specific prior information that users may have from additional data.
#'
#' ### Animal preference, \eqn{r_i}
#' The animal preference parameter has an exponential prior with a rate parameter `beta`. The default, \eqn{\mathrm{Exponential}(0.01)}, implies a mean of 1/0.01 = 100 with 95% of the probability mass falling between about 2.5-370. Increase `beta` for animal communities that weakly increase visitation in the presence of a link, or decrease for communities that strongly increase visitation in the presence of a link.
#'
#' ### Relative interaction abundances for plants, \eqn{\sigma_i}
#' The relative interaction abundances for plants has a Dirichlet prior that accepts a vector of concentration parameters, `alpha_sigma`, with a length equal to the number of plant species (rows) in the interaction matrix. The default, \eqn{\mathrm{Dir}(1, ..., 1)}, implies a uniform distribution of proportions. Increasing some of the concentration parameters will shift the probability mass to favor higher relative interaction abundances for the corresponding plants. Increasing all concentration parameters while maintaining their relative sizes reduces the variance.
#'
#' ### Relative interaction abundances for animals, \eqn{\tau_j}
#' The relative interaction abundances for animals has a Dirichlet prior that accepts a vector of concentration parameters, `alpha_tau`, with a length equal to the number of animal species (columns) in the interaction matrix. The default, \eqn{\mathrm{Dir}(1, ..., 1)}, implies a uniform distribution of proportions. Increasing some of the concentration parameters will shift the probability mass to favor higher relative interaction abundances for the corresponding animals Increasing all concentration parameters while maintaining their relative sizes reduces the variance.
#'
#' @examplesIf interactive()
#' data(web)
#' dt <- prepare_data(mat = web, plant_effort = rep(20, nrow(web)))
#' # fit the model used by Young et al. 2021
#' fit <- fit_model(dt, plant_effort = "param", animal_pref = "fixed",
#'                  plant_abun = "param", animal_abun = "param")
#' # changing options and priors
#' fit2 <- fit_model(dt, plant_effort = "data", animal_pref = "varying",
#'                   plant_abun = "param", animal_abun = "param",
#'                   beta = rep(0.01, 21),
#'                   alpha_sigma = c(2.5, 3, 4, 3, 1, 1, 5, 2),
#'                   alpha_tau = 2)

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
  if (plant_effort == "data" & is.null(data$C)) stop('Sampling effort, C, must be supplied in data when plant_effort = "data"')
  if (plant_abun == "data") {
    if (is.null(data$abun_p)) stop('Plant abundances, abun_p, must be supplied in `data` when plant_abun = "data"')
  }
  if (animal_abun == "data") {
    if (is.null(data$abun_a)) stop('Animal abundances, abun_a, must be supplied in `data` when animal_abun = "data"')
  }
  stopifnot(beta > 0, alpha_sigma > 0, alpha_tau > 0)

  ## Check prior parameters and compose prior parameter vectors
  if (animal_pref == "fixed") {
    stopifnot(length(beta) == 1L)
  } else if (animal_pref == "varying") {
    if (length(beta) == 1L) {
      message("Single value supplied to 'beta' will be used as the prior parameter for each animal.")
      beta <- rep(beta, times = ncol(data$M))
    } else if (length(beta) != ncol(data$M)) {
      stop("`beta` must have length 1 (same value applied to each animal) or be equal to the number of animal species.")
    }
  }
  if (plant_abun == "param") {
    if (length(alpha_sigma) == 1L) {
      message("Single value supplied to 'alpha_sigma' will be used as the prior parameter for each plant.")
      alpha_sigma <- rep(alpha_sigma, times = nrow(data$M))
    } else if (length(alpha_sigma) != nrow(data$M)) {
      stop("`alpha_sigma` must have length 1 (same value applied to each plant) or be equal to the number of plant species.")
    }
  }
  if (animal_abun == "param") {
    if (length(alpha_tau) == 1L) {
      message("Single value supplied to 'alpha_tau' will be used as the prior parameter for each animal.")
      alpha_tau <- rep(alpha_tau, ncol(data$M))
    } else if (length(alpha_tau) != ncol(data$M)) {
      stop("`alpha_tau` must have length 1 (same value applied to each animal) or be equal to the number of animal species.")
    }
  }


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
