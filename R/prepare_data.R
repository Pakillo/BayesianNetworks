#' Prepare the data for modelling
#'
#' @param mat An integer matrix containing quantitative (not qualitative or binary)
#' count data con interaction frequency (e.g. visits to flowers, number of fruits
#' consumed per plant or species). Plants must be in rows, Animals must be in columns.
#' @param sampl.eff A numeric vector with the sampling effort (e.g. observation hours)
#' spent on each plant.
#'
#' @return A named list with all the data required to run the model.
#' @export
#'
#' @examples
prepare_data <- function(mat = NULL, sampl.eff = NULL) {

  ## Checks

  stopifnot(is.matrix(mat))
  stopifnot(is.integer(mat))
  if (min(mat) < 0) {
    stop("There cannot be negative counts")
  }
  if (max(mat) == 1) {
    warning("mat must be a quantitative (not qualitative/binary) matrix with count data. Are you sure the maximum observed count is 1?")
  }

  stopifnot(length(sampl.eff) == nrow(mat))
  stopifnot(is.numeric(sampl.eff))
  if (max(sampl.eff) > 100) {
    warning("sampl.eff takes large values, which could hinder model convergence. Consider using different units (e.g. hours instead of minutes) to obtain smaller numbers")
  }

  ##

  ## Data list
  dt <- list(M = mat, n_p = nrow(mat), n_a = ncol(mat), C = sampl.eff)

  dt

}
