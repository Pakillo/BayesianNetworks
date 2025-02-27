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
#' dt <- prepare_data(mat = web, plant_effort = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' predict_counts(fit, dt)

predict_counts <- function(fit = NULL, data = NULL) {
  ## generate posteriors
  post <- get_posterior(fit, data, param = "all")

  ## fill in plant sampling effort data if not estimated as a parameter
  if (!"plant_effort" %in% names(post)) {
    df_e <- data.frame(Plant = rownames(data$M),
                       plant_effort = unname(data$C))
    post <- dplyr::left_join(x = post, y = df_e, by = "Plant")
  }

  ## fill abundance data if not estimated as a parameter
  if (!"plant_abund" %in% names(post)) {
    df_p <- data.frame(Plant = rownames(data$M),
                       plant_abund = data$abun_p / sum(data$abun_p))
    post <- dplyr::left_join(x = post, y = df_p, by = "Plant")
  }
  if (!"animal_abund" %in% names(post)) {
    df_a <- data.frame(Animal = colnames(data$M),
                       animal_abund = data$abun_a / sum(data$abun_a))
    post <- dplyr::left_join(x = post, y = df_a, by = "Animal")
  }

  ## calculate predicted counts
  post <- dplyr::mutate(post,
                        count = (1 - int_prob) * plant_effort * plant_abund * animal_abund +
                          int_prob * plant_effort * plant_abund * animal_abund * (1 + preference))
  return(post)
}
