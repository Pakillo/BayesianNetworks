#' Check fitted model
#'
#' @param fit A fitted model, as obtained from `fit_model()`.
#'
#' @return Model checks on console and graphical window.
#' @export
#' @import cmdstanr
#'
#' @examples
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' check_model(fit)

check_model <- function(fit = NULL) {

  fit$diagnostic_summary()

  fit$cmdstan_diagnose()

  plot_prior(fit = fit)

  check_lp(fit)

}


# Check log posterior across chains
check_lp <- function(fit = NULL) {
  lp <- fit$draws("lp__", format = "df")

  gg <- ggplot2::ggplot(lp, ggplot2::aes(.draw, lp__, colour = as.factor(.chain))) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Log posterior across chains")

  gg

}






