#' Extract the full plot specification as a master tidy data frame
#'
#' Calls all `spec_*()` functions and joins their results into a single
#' wide-format data frame with one row per layer (including a row 0 for the
#' global context). Scale, facet, coordinate, and label information are
#' repeated across all layer rows.
#'
#' @param p A ggplot object.
#' @param inherit Passed to [spec_layers()] and [spec_aes()]; controls
#'   global/local aesthetic mapping inheritance.
#'
#' @return A [tibble::tibble()] with one row per layer (plus layer 0) and
#'   columns from [spec_layers()] plus the following additional list-columns:
#'   \describe{
#'     \item{`aes_long`}{List-column: the [spec_aes()] data frame for that
#'       layer (for convenient access without re-calling [spec_aes()]).}
#'     \item{`datasets`}{List-column: the [spec_data()] data frame (repeated
#'       for every row).}
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

  layers_tbl   <- spec_layers(p, inherit = inherit)
  aes_tbl      <- spec_aes(p, inherit = inherit)
  datasets_tbl <- spec_data(p)
  scales_tbl   <- spec_scales(p)
  facets_tbl   <- spec_facets(p)
  coord_tbl    <- spec_coord(p)
  labels_tbl   <- spec_labels(p)

  # Per-layer aes sub-tables (including layer 0)
  aes_list <- lapply(layers_tbl$layer, function(i) {
    aes_tbl[aes_tbl$layer == i, , drop = FALSE]
  })

  dplyr::mutate(
    layers_tbl,
    aes_long = aes_list,
    datasets = list(datasets_tbl),
    scales   = list(scales_tbl),
    facets   = list(facets_tbl),
    coord    = list(coord_tbl),
    labels   = list(labels_tbl)
  )
}
