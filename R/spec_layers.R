#' Extract layer specifications as a tidy data frame
#'
#' Returns one row per layer in the plot, with columns describing the geom,
#' stat, position adjustment, aesthetic mappings, non-aesthetic parameters,
#' and whether the layer inherits the global aesthetic mapping.
#'
#' @param p A ggplot object.
#' @param inherit Controls how global and local aesthetic mappings are combined
#'   in the `mapping` list-column:
#'   - `"resolve"` (default): local mappings override global ones.
#'   - `TRUE`: global mappings are used, with local overrides appended.
#'   - `FALSE`: only layer-local mappings are returned.
#'
#' @return A [tibble::tibble()] with one row per layer and columns:
#'   \describe{
#'     \item{`layer_idx`}{Integer layer index (1-based).}
#'     \item{`geom`}{Geom name, e.g. `"point"`, `"smooth"`.}
#'     \item{`stat`}{Stat name, e.g. `"identity"`, `"smooth"`.}
#'     \item{`position`}{Position adjustment name, e.g. `"identity"`, `"dodge"`.}
#'     \item{`mapping`}{List-column of named character vectors: aesthetic to
#'       variable label.}
#'     \item{`params`}{List-column of named lists of non-aesthetic layer
#'       parameters.}
#'     \item{`inherit_aes`}{Logical: does the layer inherit the global mapping?}
#'   }
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = class)) +
#'   ggplot2::geom_smooth(method = "lm", se = FALSE)
#' spec_layers(p)
spec_layers <- function(p, inherit = "resolve") {
  assert_ggplot(p)
  layers <- p$layers
  if (length(layers) == 0L) {
    return(.empty_layers_tbl())
  }

  rows <- lapply(seq_along(layers), function(i) {
    l <- layers[[i]]
    mapping_chr <- .resolve_aes(l, p$mapping, inherit = inherit)
    params <- .layer_params(l)
    tibble::tibble(
      layer_idx   = i,
      geom        = .geom_name(l),
      stat        = .stat_name(l),
      position    = .position_name(l),
      mapping     = list(mapping_chr),
      params      = list(params),
      inherit_aes = isTRUE(l$inherit.aes)
    )
  })

  dplyr::bind_rows(rows)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract non-aesthetic parameters from a layer
#' @noRd
.layer_params <- function(layer) {
  # layer$aes_params holds aesthetic constants (e.g. colour = "red")
  # layer$geom_params / layer$stat_params hold other params
  # In ggplot2 >= 3.4 the unified store is layer$computed_geom_params but we
  # access the user-supplied ones via layer$geom$parameters() exclusion.
  params <- layer$geom_params %||% list()
  stat_p <- layer$stat_params %||% list()
  aes_p  <- layer$aes_params  %||% list()
  c(params, stat_p, aes_p)
}

#' Return an empty tibble with the correct spec_layers() schema
#' @noRd
.empty_layers_tbl <- function() {
  tibble::tibble(
    layer_idx   = integer(),
    geom        = character(),
    stat        = character(),
    position    = character(),
    mapping     = list(),
    params      = list(),
    inherit_aes = logical()
  )
}
