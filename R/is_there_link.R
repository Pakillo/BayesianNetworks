#' Is there a link?
#'
#' @param df A dataframe containing posterior interaction probabilities, as
#' generated from `get_posterior()`
#'
#' @return A data frame with a new column `link` taking values of 0 (no link)
#' and 1 (link).
#' @export
#'

is_there_link <- function(df = NULL) {

  stopifnot("int.prob" %in% names(df))
  df |>
    dplyr::mutate(link = rbinom(1, size = 1, prob = int.prob))
}
