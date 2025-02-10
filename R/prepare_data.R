#' Prepare the data for modelling
#'
#' @param mat A matrix containing quantitative counts (not qualitative or binary)
#' of observed interactions (e.g. visits to flowers, number of fruits
#' consumed per plant or species). Plants must be in rows, Animals must be in columns.
#' @param plant_effort A numeric vector with the sampling effort (e.g. observation hours)
#' spent on each plant.
#' @param plant_abun (optional) A numeric vector with the relative or absolute abundances of each plant.
#' @param animal_abun (optional) A numeric vector with the relative or absolute abundances of each animal.
#'
#' @return A named list containing data to be passed to the bipartite network model.
#' @export
#'
#' @examples
#' data(web)
#' prepare_data(web, plant_effort = rep(20, nrow(web)))

prepare_data <- function(mat, plant_effort = NULL, plant_abun = NULL, animal_abun = NULL) {
  ## Checks
  stopifnot(is.matrix(mat))
  if (!is.integer(mat) & all(mat == as.integer(mat))) stop("Elements of 'mat' must be integers.")
  if (min(mat) < 0) stop("Elements of 'mat' must be >=0.")
  if (max(mat) == 1) warning("'mat' should be a quantitative (not qualitative/binary) matrix with count data. Are you sure the maximum observed count is 1?")
  if (!is.null(plant_effort)) {
    stopifnot(length(plant_effort) == nrow(mat))
    stopifnot(is.numeric(plant_effort))
    if (max(plant_effort) > 100) warning("Large values for `plant_effort` may interfere with sampling efficiency and model convergence. Consider rescaling to different units (e.g. hours instead of minutes) to obtain values <100.")
  }
  if (!is.null(plant_abun)) {
    stopifnot(length(plant_abun) == nrow(mat))
    stopifnot(is.numeric(plant_abun))
    stopifnot(all(plant_abun > 0))
  }
  if (!is.null(animal_abun)) {
    stopifnot(length(animal_abun) == ncol(mat))
    stopifnot(is.numeric(animal_abun))
    stopifnot(all(animal_abun > 0))
  }

  ## Ensure interaction counts are stored as integers
  p <- rownames(mat)
  a <- colnames(mat)
  mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat), dimnames = list(p, a))

  ## Prepare data list, dropping NULL elements
  dt <- list(M = mat, n_p = nrow(mat), n_a = ncol(mat), C = plant_effort,
             sigma = plant_abun, tau = animal_abun)
  dt <- Filter(Negate(is.null), dt)
  return(dt)
}
