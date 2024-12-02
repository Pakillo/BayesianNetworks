#' Fit model
#'
#' @param data A named list containing the required data, as obtained from
#'  [prepare_data()].
#' @param model character. One of "Young2021", "sampling_effort", or
#' "varying_preferences", or a path to a file describing the Stan model in case
#' you want to use a modified Stan model.
#' @param beta Rate of exponential prior on `r` (preference) parameter.
#' Default beta is 0.01. Increase it if you have large count numbers
#' (can examine the resultant prior using [plot_prior()]).
#' @param ... Further arguments for [cmdstanr::sample()], like `iter_warmup`,
#' `iter_sampling`, or `thin`, among others. It is recommended to increase the
#' number of iterations (e.g. iter_sampling = 10000).
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
                      model = c("sampling_effort", "Young2021", "varying_preferences"),
                      beta = 0.01,
                      ...) {

  if (is.null(data)) {
    stop("Please provide the data as produced by 'prepare_data()'")
  }

  model <- match.arg(model)  # TODO: remove this later to allow for user models

  if (model %in% c("sampling_effort", "Young2021", "varying_preferences")) {
    mod.stan <- system.file(paste0("models/", model, ".stan"), package = "NetBayes")
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
