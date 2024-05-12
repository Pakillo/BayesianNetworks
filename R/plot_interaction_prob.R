#' Plot heatmap of interaction probabilities
#'
#' Plot a heatmap of average interaction probabilities
#'
#' @param post Data frame containing the posterior probabilities, as generated
#' from `get_posterior()`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' post <- get_posterior(fit, dt)
#' plot_interaction_prob(post)

plot_interaction_prob <- function(post = NULL) {
  post |>
    dplyr::select(Plant, Animal, int.prob) |>
    tidybayes::mean_qi() |>
    network.tools::plot_web_heatmap(int.var = "int.prob", sort = FALSE) +
    ggplot2::labs(title = "Mean interaction probability",
                  fill = "Probability")
}
