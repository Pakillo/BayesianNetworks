#' Get seed used to fit a model
#'
#' @param fit A fitted model
#'
#' @return A number
#' @export
#'
#' @examplesIf interactive()
#' data(web)
#' dt <- prepare_data(mat = web, sampl.eff = rep(20, nrow(web)))
#' fit <- fit_model(dt, refresh = 0)
#' get_seed(fit)

get_seed <- function(fit = NULL) {
  seed <- fit$metadata()$seed
  stopifnot(is.numeric(seed))
  return(seed)
}
