#' Get posterior values
#'
#' @param fit Fitted model (from `fit_model()`)
#' @param data Data list (from `prepare_data()`)
#' @param param character. Name of the parameter to retrieve the posterior samples.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' connectance <- get_posterior(fit, dt, param = "connectance")
#' connectance
#'
#' int.prob <- get_posterior(fit, dt, param = "int.prob")
#' int.prob
#' int.prob |> tidybayes::mean_qi()  # mean edge probability
#'
#' # plot
#' int.prob |>
#'   tidybayes::mean_qi() |>
#'   dplyr::select(Plant, Animal, int.prob) |>
#'   network.tools::plot_web_heatmap(int.var = "int.prob", sort = FALSE)
#'
#'  # compare with Visits
#'  web |>
#'    network.tools::wide2long() |>
#'    network.tools::plot_web_heatmap(zero.na = FALSE, sort = FALSE)

get_posterior <- function(fit = NULL,
                          data = NULL,
                          param = c("connectance",
                                    "preference",
                                    "plant.abund",
                                    "animal.abund",
                                    "int.prob")) {

  # r = avg. visits from mutualists (preference)
  # rho = connectance
  # sigma = plant.abund
  # tau = animal.abund
  # Q = interaction probability

  param <- match.arg(param)

  post <- switch(
    param,
    connectance = tidybayes::spread_draws(fit, rho),
    preference = tidybayes::spread_draws(fit, r),
    plant.abund = tidybayes::spread_draws(fit, sigma[Plant]),
    animal.abund = tidybayes::spread_draws(fit, tau[Animal]),
    int.prob = tidybayes::spread_draws(fit, Q[Plant, Animal]),
  )

  # use more informative names
  param.names <- c(
    connectance = "rho",
    preference = "r",
    plant.abund = "sigma",
    animal.abund = "tau",
    int.prob = "Q")

  post <- rename(post, any_of(param.names))


  ## rename plants and animals with original labels

  if ("Plant" %in% names(post)) {
    plants <- data.frame(Plant = 1:nrow(data$M), Plant.name = rownames(data$M))
    post <- post |>
      dplyr::mutate(Plant = plants$Plant.name[match(Plant, plants$Plant)])
  }

  if ("Animal" %in% names(post)) {
    animals <- data.frame(Animal = 1:ncol(data$M), Animal.name = colnames(data$M))
    post <- post |>
      dplyr::mutate(Animal = animals$Animal.name[match(Animal, animals$Animal)])
  }

  return(post)


}
