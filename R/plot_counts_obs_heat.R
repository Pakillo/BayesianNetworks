#' Plot heatmap of observed counts
#'
#' @param mat A matrix with count data reporting interaction frequency
#' (e.g. visits to flowers, number of fruits consumed per plant or species).
#' Plants must be in rows, Animals must be in columns.
#' @param ... Further arguments for [network.tools::plot_web_heatmap()].
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' data(web)
#' plot_counts_obs_heat(web)
#' plot_counts_obs_heat(web, sort = FALSE)

plot_counts_obs_heat <- function(mat = NULL, ...) {

  mat |>
    network.tools::wide2long() |>
    network.tools::plot_web_heatmap(...) +
    ggplot2::labs(title = "Observed counts", fill = "")

}
