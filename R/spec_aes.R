#' Extract aesthetic mappings as a long-format tidy data frame
#'
#' Returns one row per (layer × aesthetic) pair, making it easy to check
#' which variable is mapped to which aesthetic in which layer.
#'
#' @param p A ggplot object.
#' @param layer Integer vector of layer indices to include. `NULL` (default)
#'   returns all layers.
#' @param inherit Controls global/local mapping inheritance; same semantics as
#'   in [spec_layers()].
#'
#' @return A [tibble::tibble()] with one row per layer × aesthetic and columns:
#'   \describe{
#'     \item{`layer_idx`}{Integer layer index (1-based).}
#'     \item{`geom`}{Geom name for the layer.}
#'     \item{`aesthetic`}{Aesthetic name, e.g. `"x"`, `"colour"`.}
#'     \item{`variable`}{Variable label mapped to the aesthetic (as a string).}
#'     \item{`source`}{Where the mapping originates: `"global"`, `"local"`, or
#'       `"resolved"` (present in both, local takes precedence).}
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
  if (length(layers) == 0L) return(.empty_aes_tbl())

  idx <- if (is.null(layer)) seq_along(layers) else as.integer(layer)
  idx <- idx[idx >= 1L & idx <= length(layers)]
  if (length(idx) == 0L) return(.empty_aes_tbl())

  global_chr <- vapply(p$mapping %||% rlang::quos(), .deparse_aes, character(1L))

  rows <- lapply(idx, function(i) {
    l <- layers[[i]]
    local_chr <- vapply(l$mapping %||% rlang::quos(), .deparse_aes, character(1L))

    # Determine source per aesthetic
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
      layer_idx = i,
      geom      = .geom_name(l),
      aesthetic = all_aes,
      variable  = unname(var_vec),
      source    = unname(source_vec)
    )
  })

  dplyr::bind_rows(rows)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.empty_aes_tbl <- function() {
  tibble::tibble(
    layer_idx = integer(),
    geom      = character(),
    aesthetic = character(),
    variable  = character(),
    source    = character()
  )
}
