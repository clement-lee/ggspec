#' Extract aesthetic mappings as a long-format tidy data frame
#'
#' Returns one row per (layer × aesthetic) pair, making it easy to check
#' which variable is mapped to which aesthetic in which layer.  A row with
#' `layer = 0` is always included to document any mapping specified directly
#' in `ggplot()`.
#'
#' @param p A ggplot object.
#' @param layer Integer vector of layer indices to include. `NULL` (default)
#'   returns all layers including layer 0.  Pass `0L` to retrieve only the
#'   global-context row; pass `1L` etc. for specific non-zero layers.
#' @param inherit Controls global/local mapping inheritance for non-zero
#'   layers; same semantics as in [spec_layers()].
#'
#' @return A [tibble::tibble()] with one row per layer × aesthetic and columns:
#'   \describe{
#'     \item{`layer`}{Integer layer index. `0` = global context; `1..N` =
#'       regular layers.}
#'     \item{`geom`}{Geom name for the layer. `NA` for layer 0.}
#'     \item{`aesthetic`}{Aesthetic name, e.g. `"x"`, `"colour"`.}
#'     \item{`variable`}{Variable label mapped to the aesthetic (as a string).}
#'     \item{`source`}{Where the mapping originates: `"global"` (layer-0
#'       rows), `"local"` (layer-specific only), or `"resolved"` (present in
#'       both global and local, local takes precedence).}
#'   }
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = class)) +
#'   ggplot2::geom_smooth()
#' spec_aes(p)
#' spec_aes(p, layer = 1L)
spec_aes <- function(p, layer = NULL, inherit = "resolve") {
  assert_ggplot(p)
  layers <- p$layers

  global_chr <- vapply(p$mapping %||% rlang::quos(), .deparse_aes, character(1L))

  # Layer 0: global mapping
  layer0_rows <- if (length(global_chr) > 0L) {
    tibble::tibble(
      layer     = 0L,
      geom      = NA_character_,
      aesthetic = names(global_chr),
      variable  = unname(global_chr),
      source    = rep("global", length(global_chr))
    )
  } else {
    .empty_aes_tbl()
  }

  # Determine which non-zero layer indices to include
  if (!is.null(layer)) {
    layer_int <- as.integer(layer)
    include_layer0 <- 0L %in% layer_int
    idx <- layer_int[layer_int >= 1L & layer_int <= length(layers)]
    if (!include_layer0) layer0_rows <- .empty_aes_tbl()
  } else {
    include_layer0 <- TRUE
    idx <- seq_along(layers)
  }

  if (length(idx) == 0L) {
    return(layer0_rows)
  }

  rows <- lapply(idx, function(i) {
    l         <- layers[[i]]
    local_chr <- vapply(l$mapping %||% rlang::quos(), .deparse_aes, character(1L))

    all_aes <- union(names(global_chr), names(local_chr))
    if (length(all_aes) == 0L) return(NULL)

    source_vec <- vapply(all_aes, function(a) {
      in_global <- a %in% names(global_chr)
      in_local  <- a %in% names(local_chr)
      if (in_global && in_local) "resolved"
      else if (in_local)         "local"
      else                        "global"
    }, character(1L))

    resolved <- .resolve_aes(l, p$mapping, inherit = inherit)
    var_vec  <- resolved[all_aes]
    var_vec[is.na(var_vec)] <- NA_character_

    tibble::tibble(
      layer     = i,
      geom      = .geom_name(l),
      aesthetic = all_aes,
      variable  = unname(var_vec),
      source    = unname(source_vec)
    )
  })

  dplyr::bind_rows(layer0_rows, dplyr::bind_rows(rows))
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.empty_aes_tbl <- function() {
  tibble::tibble(
    layer     = integer(),
    geom      = character(),
    aesthetic = character(),
    variable  = character(),
    source    = character()
  )
}
