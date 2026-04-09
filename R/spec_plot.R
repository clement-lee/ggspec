#' Extract the full plot specification as a master tidy data frame
#'
#' Calls all `spec_*()` functions and joins their results into a single
#' wide-format data frame with one row per layer. Scale, facet, coordinate,
#' and label information are repeated across all layer rows.
#'
#' @param p A ggplot object.
#' @param inherit Passed to [spec_layers()] and [spec_aes()]; controls
#'   global/local aesthetic mapping inheritance.
#'
#' @return A [tibble::tibble()] with one row per layer and columns from
#'   [spec_layers()] plus the following additional list-columns:
#'   \describe{
#'     \item{`aes_long`}{List-column: the [spec_aes()] data frame for that
#'       layer (for convenient access without re-calling [spec_aes()]).}
#'     \item{`scales`}{List-column: the full [spec_scales()] data frame,
#'       repeated for each layer.}
#'     \item{`facets`}{List-column: the [spec_facets()] data frame, repeated
#'       for each layer.}
#'     \item{`coord`}{List-column: the [spec_coord()] data frame, repeated
#'       for each layer.}
#'     \item{`labels`}{List-column: the [spec_labels()] data frame, repeated
#'       for each layer.}
#'   }
#'
#'   If the plot has no layers, a zero-row tibble with the correct schema is
#'   returned.
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = class)) +
#'   ggplot2::geom_smooth(method = "lm") +
#'   ggplot2::facet_wrap(~drv) +
#'   ggplot2::labs(title = "Engine vs MPG")
#' spec_plot(p)
spec_plot <- function(p, inherit = "resolve") {
  assert_ggplot(p)

  layers_tbl <- spec_layers(p, inherit = inherit)
  aes_tbl    <- spec_aes(p, inherit = inherit)
  scales_tbl <- spec_scales(p)
  facets_tbl <- spec_facets(p)
  coord_tbl  <- spec_coord(p)
  labels_tbl <- spec_labels(p)

  if (nrow(layers_tbl) == 0L) {
    return(dplyr::mutate(
      layers_tbl,
      aes_long = list(),
      scales   = list(),
      facets   = list(),
      coord    = list(),
      labels   = list()
    ))
  }

  # Per-layer aes sub-tables
  aes_list <- lapply(layers_tbl$layer_idx, function(i) {
    aes_tbl[aes_tbl$layer_idx == i, , drop = FALSE]
  })

  dplyr::mutate(
    layers_tbl,
    aes_long = aes_list,
    scales   = list(scales_tbl),
    facets   = list(facets_tbl),
    coord    = list(coord_tbl),
    labels   = list(labels_tbl)
  )
}
