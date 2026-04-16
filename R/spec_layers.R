#' Extract layer specifications as a tidy data frame
#'
#' Returns one row per layer in the plot (including a row 0 for the global
#' context), with columns describing the geom, stat, position adjustment,
#' aesthetic mappings, non-aesthetic parameters, and data source.
#'
#' @param p A ggplot object.
#' @param inherit Controls how global and local aesthetic mappings are combined
#'   in the `mapping` list-column:
#'   - `"resolve"` (default): local mappings override global ones.
#'   - `TRUE`: global mappings are used, with local overrides appended.
#'   - `FALSE`: only layer-local mappings are returned.
#'
#' @return A [tibble::tibble()] with one row per layer (plus a row 0 for the
#'   global context) and columns:
#'   \describe{
#'     \item{`layer`}{Integer layer index. `0` = global context; `1..N` =
#'       regular layers (1-based).}
#'     \item{`geom`}{Geom name, e.g. `"point"`, `"smooth"`. `NA` for layer 0.}
#'     \item{`stat`}{Stat name, e.g. `"identity"`, `"smooth"`. `NA` for
#'       layer 0.}
#'     \item{`position`}{Position adjustment name. `NA` for layer 0.}
#'     \item{`mapping`}{List-column of named character vectors: aesthetic to
#'       variable label.}
#'     \item{`params`}{List-column of named lists of non-aesthetic layer
#'       parameters. Empty list for layer 0.}
#'     \item{`inherit_aes`}{Logical: does the layer inherit the global
#'       mapping? `NA` for layer 0.}
#'     \item{`data_id`}{Integer: identifier of the dataset used, referencing
#'       [spec_data()]. `NA` for layers that inherit the global data.}
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
  datasets <- .spec_data_internal(p)

  # Layer 0: global context
  global_mapping_chr <- vapply(
    p$mapping %||% rlang::quos(), .deparse_aes, character(1L)
  )
  layer0 <- tibble::tibble(
    layer       = 0L,
    geom        = NA_character_,
    stat        = NA_character_,
    position    = NA_character_,
    mapping     = list(global_mapping_chr),
    params      = list(list()),
    inherit_aes = NA,
    data_id     = .data_id_lookup(p$data, datasets)
  )

  layers <- p$layers
  if (length(layers) == 0L) return(layer0)

  rows <- lapply(seq_along(layers), function(i) {
    l           <- layers[[i]]
    mapping_chr <- .resolve_aes(l, p$mapping, inherit = inherit)
    params      <- .layer_params(l)
    tibble::tibble(
      layer       = i,
      geom        = .geom_name(l),
      stat        = .stat_name(l),
      position    = .position_name(l),
      mapping     = list(mapping_chr),
      params      = list(params),
      inherit_aes = isTRUE(l$inherit.aes),
      data_id     = .data_id_lookup(l$data, datasets)
    )
  })

  dplyr::bind_rows(layer0, dplyr::bind_rows(rows))
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract non-aesthetic parameters from a layer
#' @noRd
.layer_params <- function(layer) {
  params <- layer$geom_params %||% list()
  stat_p <- layer$stat_params %||% list()
  aes_p  <- layer$aes_params  %||% list()
  c(params, stat_p, aes_p)
}

#' Return an empty tibble with the correct spec_layers() schema
#' @noRd
.empty_layers_tbl <- function() {
  tibble::tibble(
    layer       = integer(),
    geom        = character(),
    stat        = character(),
    position    = character(),
    mapping     = list(),
    params      = list(),
    inherit_aes = logical(),
    data_id     = integer()
  )
}
