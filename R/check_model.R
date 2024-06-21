#' Check fitted model
#'
#' @param fit A fitted model, as obtained from [fit_model()].
#' @param data Data list (from [prepare_data()]).
#'
#' @return Model checks on console and graphical window.
#' @export
#' @import cmdstanr
#'
#' @examples
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' check_model(fit, data = data)

check_model <- function(fit = NULL, data = NULL) {

  fit$diagnostic_summary()

  fit$cmdstan_diagnose()

  print(plot_prior(fit = fit, data = data))

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






