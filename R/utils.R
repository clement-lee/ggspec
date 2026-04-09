#' Test whether an object is a ggplot
#'
#' @param p An object to test.
#' @return `TRUE` if `p` inherits from `"gg"`, `FALSE` otherwise.
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy))
#' is_ggplot(p)
#' is_ggplot(list())
is_ggplot <- function(p) {
  inherits(p, "gg")
}

#' Assert that an object is a ggplot
#'
#' Stops with an informative error if `p` is not a ggplot object.
#'
#' @param p An object to check.
#' @param arg_name Character string used in the error message to name the
#'   argument. Defaults to the deparsed expression of `p`.
#' @return `p` invisibly, if it passes the check.
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy))
#' assert_ggplot(p)
assert_ggplot <- function(p, arg_name = deparse(substitute(p))) {
  if (!is_ggplot(p)) {
    rlang::abort(
      paste0("`", arg_name, "` must be a ggplot object, not ",
             paste(class(p), collapse = "/"), "."),
      class = "ggspec_not_ggplot"
    )
  }
  invisible(p)
}

#' Count the number of layers in a ggplot
#'
#' @param p A ggplot object.
#' @return A non-negative integer.
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_smooth()
#' n_layers(p)
n_layers <- function(p) {
  assert_ggplot(p)
  length(p$layers)
}

#' Test whether a plot contains a given layer
#'
#' @param p A ggplot object.
#' @param geom Optional character string: geom suffix to look for (e.g.
#'   `"point"`). Case-insensitive.
#' @param stat Optional character string: stat suffix to look for (e.g.
#'   `"smooth"`). Case-insensitive.
#' @return `TRUE` if at least one layer matches all supplied criteria.
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' has_layer(p, geom = "point")
#' has_layer(p, geom = "smooth")
has_layer <- function(p, geom = NULL, stat = NULL) {
  assert_ggplot(p)
  layers <- p$layers
  if (length(layers) == 0L) return(FALSE)

  matches <- vapply(layers, function(l) {
    geom_ok <- is.null(geom) ||
      identical(tolower(.geom_name(l)), tolower(geom))
    stat_ok <- is.null(stat) ||
      identical(tolower(.stat_name(l)), tolower(stat))
    geom_ok && stat_ok
  }, logical(1L))

  any(matches)
}

# ---------------------------------------------------------------------------
# Internal helpers used across multiple files
# ---------------------------------------------------------------------------

#' Extract the lowercase geom name from a layer object
#' @noRd
.geom_name <- function(layer) {
  cls <- class(layer$geom)[[1L]]
  tolower(sub("^Geom", "", cls))
}

#' Extract the lowercase stat name from a layer object
#' @noRd
.stat_name <- function(layer) {
  cls <- class(layer$stat)[[1L]]
  tolower(sub("^Stat", "", cls))
}

#' Extract the lowercase position name from a layer object
#' @noRd
.position_name <- function(layer) {
  cls <- class(layer$position)[[1L]]
  tolower(sub("^Position", "", cls))
}

#' Deparse a quosure or expression to a plain character string
#' @noRd
.deparse_aes <- function(x) {
  if (rlang::is_quosure(x)) {
    rlang::as_label(x)
  } else if (is.character(x)) {
    x
  } else {
    deparse(x)
  }
}

#' Build a ggplot object once and return the built result
#'
#' Thin wrapper around [ggplot2::ggplot_build()] so callers don't pay the
#' build cost more than once per function call.
#' @noRd
.build <- function(p) {
  ggplot2::ggplot_build(p)
}

#' Resolve aesthetic mappings for a layer, handling inheritance
#'
#' @param layer A ggplot2 layer object (`p$layers[[i]]`).
#' @param global_mapping The plot-level mapping (`p$mapping`).
#' @param inherit One of `TRUE` (merge global into local), `FALSE` (local
#'   only), or `"resolve"` (merge, with local taking precedence).
#' @return A named character vector: aesthetic name -> variable label.
#' @noRd
.resolve_aes <- function(layer, global_mapping, inherit = "resolve") {
  local_raw  <- layer$mapping %||% rlang::quos()
  global_raw <- global_mapping %||% rlang::quos()
  inherit_aes <- isTRUE(layer$inherit.aes)

  local_chr  <- vapply(local_raw,  .deparse_aes, character(1L))
  global_chr <- vapply(global_raw, .deparse_aes, character(1L))

  if (identical(inherit, FALSE) || !inherit_aes) {
    return(local_chr)
  }
  if (identical(inherit, TRUE)) {
    # global only
    return(c(global_chr, local_chr[!names(local_chr) %in% names(global_chr)]))
  }
  # "resolve": local overrides global
  merged <- global_chr
  merged[names(local_chr)] <- local_chr
  merged
}

# re-export rlang's null-coalescing operator for internal use
`%||%` <- rlang::`%||%`
