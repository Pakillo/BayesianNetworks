#' Predict interaction counts
#'
#' Generate the posterior predictive distribution of counts for every pairwise
#' interaction.
#'
#' @param fit Fitted model
#' @param data Data list
#'
#' @return A data frame
#' @export
#'
#' @examplesIf interactive()
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' predict_counts(fit, dt)

predict_counts <- function(fit = NULL, data = NULL) {

  ## generate posteriors
  post <- get_posterior(fit, data, param = "all")

  ## add sampling effort per plant
  C <- data.frame(Plant = rownames(data$M), effort = data$C)
  post <- dplyr::left_join(post, C, by = "Plant")

  ## calculate predicted counts
  post <- dplyr::mutate(post,
                        count = (1 - int.prob) * effort * plant.abund * animal.abund +
                          int.prob * effort * plant.abund * animal.abund * (1 + preference))

  return(post)

}
