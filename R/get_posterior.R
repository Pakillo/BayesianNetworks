#' Get posterior values
#'
#' @param fit Fitted model (from [fit_model()])
#' @param data Data list (from [prepare_data()])
#' @param param character. Name of the parameter to retrieve the posterior samples.
#'
#' @return A data frame
#' @importFrom rlang parse_exprs !!!
#' @export
#'
#' @examplesIf interactive()
#' data(web)
#' dt <- prepare_data(mat = web, plant_effort = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' get_posterior(fit, dt, param = "connectance")
#'
#' int.prob <- get_posterior(fit, dt, param = "int.prob")
#' int.prob
#' int.prob |> tidybayes::mean_qi()  # mean edge probability
#'
#' # all posteriors
#' get_posterior(fit, dt, param = "all")

get_posterior <- function(fit = NULL,
                          data = NULL,
                          param = c("all",
                                    "connectance",
                                    "preference",
                                    "plant.abund",
                                    "animal.abund",
                                    "int.prob",
                                    "link")) {
  ## parse arguments & construct parameter set to extract
  param <- match.arg(param)
  all_vars <- tidybayes::get_variables(fit)
  if (sum(grepl("^r(\\[\\d+\\])?$", all_vars)) >= 1) {
    sel_r <- "r[Animal]"
  } else {
    sel_r <- "r"
  }
  if (sum(grepl("^sigma", all_vars)) > 1) {
    sel_sigma <- "sigma[Plant]"
  } else if (sum(grepl("^sigma", all_vars)) == 1) {
    sel_sigma <- "sigma"
  } else {
    sel_sigma <- NULL
  }
  if (sum(grepl("^tau", all_vars)) > 1) {
    sel_tau <- "tau[Animal]"
  } else if (sum(grepl("^tau", all_vars)) == 1) {
    sel_tau <- "tau"
  } else {
    sel_tau <- NULL
  }

  post <- switch(
    param,
    all = tidybayes::spread_draws(fit, !!!rlang::parse_exprs(c("rho", sel_r, sel_sigma,
                                                               sel_tau, "Q[Plant, Animal]"))),
    connectance = tidybayes::spread_draws(fit, rho),
    preference = tidybayes::spread_draws(fit, !!!rlang::parse_exprs(sel_r)),
    plant_abund = tidybayes::spread_draws(fit, !!!rlang::parse_exprs(sel_sigma)),
    animal_abund = tidybayes::spread_draws(fit, !!!rlang::parse_exprs(sel_tau)),
    int_prob = tidybayes::spread_draws(fit, Q[Plant, Animal]),
    link = tidybayes::spread_draws(fit, Q[Plant, Animal]),
  )

  # use more informative names
  param_names <- c(
    connectance = "rho",
    preference = "r",
    plant_abund = "sigma",
    animal_abund = "tau",
    int_prob = "Q")

  post <- dplyr::rename(post, dplyr::any_of(param_names))

  ## generate posteriors of link existence
  if (param == "all" | param == "link") {
    post <- is_there_link(post)
  }

  ## rename plants and animals with original labels
  if ("Animal" %in% names(post)) {
    animals <- data.frame(Animal = 1:ncol(data$M), Animal.name = colnames(data$M))
    post <- post |>
      dplyr::mutate(Animal = animals$Animal.name[match(Animal, animals$Animal)]) |>
      dplyr::relocate(Animal)
  }

  if ("Plant" %in% names(post)) {
    plants <- data.frame(Plant = 1:nrow(data$M), Plant.name = rownames(data$M))
    post <- post |>
      dplyr::mutate(Plant = plants$Plant.name[match(Plant, plants$Plant)]) |>
      dplyr::relocate(Plant)
  }

  return(post)
}
