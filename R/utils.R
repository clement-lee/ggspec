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
    return(c(global_chr, local_chr[!names(local_chr) %in% names(global_chr)]))
  }
  # "resolve": local overrides global
  merged <- global_chr
  merged[names(local_chr)] <- local_chr
  merged
}

# re-export rlang's null-coalescing operator for internal use
`%||%` <- rlang::`%||%`

# ---------------------------------------------------------------------------
# Convenience wrappers
# ---------------------------------------------------------------------------

#' Extract all aesthetic mappings as a flat data frame
#'
#' Returns one row per unique (aesthetic, variable) pair found anywhere in the
#' plot — across all layers and the global mapping — after resolving
#' inheritance. This is a simpler alternative to [spec_aes()] when you only
#' need a flat lookup table.
#'
#' @param p A ggplot object.
#' @return A `data.frame` with columns `aes` (character) and `var` (character).
#'   Rows where the variable is `NA` (e.g. aesthetics set to constants) are
#'   excluded.
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = class))
#' flat_mappings(p)
flat_mappings <- function(p) {
  assert_ggplot(p)
  aes_tbl <- spec_aes(p, inherit = "resolve")
  if (nrow(aes_tbl) == 0L) {
    return(data.frame(aes = character(), var = character(),
                      stringsAsFactors = FALSE))
  }
  result <- data.frame(
    aes = aes_tbl$aesthetic,
    var = aes_tbl$variable,
    stringsAsFactors = FALSE
  )
  unique(result[!is.na(result$var), , drop = FALSE])
}

#' Test whether a specific aesthetic mapping exists in a plot
#'
#' Convenience predicate wrapping [flat_mappings()]. Returns `TRUE` if any
#' layer (or the global mapping) maps `aesthetic` to `variable`.
#'
#' @param p A ggplot object.
#' @param aesthetic Character scalar: name of the aesthetic, e.g. `"x"`,
#'   `"colour"`.
#' @param variable Character scalar: name of the variable, e.g. `"displ"`.
#' @return `TRUE` or `FALSE`.
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = class))
#' mapping_exists(p, "colour", "class")
#' mapping_exists(p, "colour", "drv")
mapping_exists <- function(p, aesthetic, variable) {
  assert_ggplot(p)
  fm <- flat_mappings(p)
  any(fm$aes == aesthetic & fm$var == variable)
}

#' Find which layer of a plot uses a given data frame
#'
#' Returns an integer indicating where `data` appears in the plot:
#' - `0L` — the plot-level (`p$data`) data
#' - `i` (positive integer) — `p$layers[[i]]$data`
#' - `NA_integer_` — not found
#'
#' @param p A ggplot object.
#' @param data A data frame to search for.
#' @return An integer scalar.
#' @export
#' @examples
#' drv_summary <- dplyr::group_by(ggplot2::mpg, drv) |>
#'   dplyr::summarise(mean_displ = mean(displ))
#' p <- ggplot2::ggplot() +
#'   ggplot2::geom_jitter(data = ggplot2::mpg,
#'                        ggplot2::aes(x = drv, y = displ)) +
#'   ggplot2::geom_point(data = drv_summary,
#'                       ggplot2::aes(x = drv, y = mean_displ))
#' layer_data_index(p, drv_summary)
layer_data_index <- function(p, data) {
  assert_ggplot(p)
  if (identical(p$data, data)) return(0L)
  for (i in seq_along(p$layers)) {
    if (identical(p$layers[[i]]$data, data)) return(i)
  }
  NA_integer_
}

# ---------------------------------------------------------------------------
# Internal helpers for canon/compare dispatch
# ---------------------------------------------------------------------------

#' Assert that x is a ggplot or ggspec_canon object
#' @noRd
.assert_gg_input <- function(x, arg_name = deparse(substitute(x))) {
  if (!is_ggplot(x) && !inherits(x, "ggspec_canon")) {
    rlang::abort(
      paste0("`", arg_name, "` must be a ggplot or ggspec_canon object, not ",
             paste(class(x), collapse = "/"), "."),
      class = "ggspec_bad_input"
    )
  }
  invisible(x)
}

#' Extract the spec_layers()-equivalent tibble from a ggplot or ggspec_canon
#' @noRd
.get_layers_tbl <- function(x, arg_name = "x") {
  if (inherits(x, "ggspec_canon")) {
    keep <- intersect(names(x$spec),
                      c("layer", "geom", "stat", "position",
                        "mapping", "params", "inherit_aes", "data_id"))
    x$spec[, keep, drop = FALSE]
  } else {
    assert_ggplot(x, arg_name)
    spec_layers(x)
  }
}

#' Extract the spec_aes()-equivalent tibble from a ggplot or ggspec_canon
#' @noRd
.get_aes_tbl <- function(x, layer = NULL, arg_name = "x") {
  if (inherits(x, "ggspec_canon")) {
    tbl <- dplyr::bind_rows(x$spec$aes_long)
    if (!is.null(layer)) tbl <- tbl[tbl$layer %in% as.integer(layer), , drop = FALSE]
    tbl
  } else {
    assert_ggplot(x, arg_name)
    spec_aes(x, layer = layer)
  }
}

#' Extract the spec_scales()-equivalent tibble from a ggplot or ggspec_canon
#' @noRd
.get_scales_tbl <- function(x, arg_name = "x") {
  if (inherits(x, "ggspec_canon")) {
    if (nrow(x$spec) == 0L) return(.empty_scales_tbl())
    # Use scales from first non-zero row if available
    ref_row <- if (any(x$spec$layer > 0L)) which(x$spec$layer > 0L)[1L] else 1L
    x$spec$scales[[ref_row]]
  } else {
    assert_ggplot(x, arg_name)
    spec_scales(x)
  }
}

#' Extract the spec_facets()-equivalent tibble from a ggplot or ggspec_canon
#' @noRd
.get_facets_tbl <- function(x, arg_name = "x") {
  if (inherits(x, "ggspec_canon")) {
    if (nrow(x$spec) == 0L) {
      return(tibble::tibble(facet_type = "null", rows = NA_character_,
                            cols = NA_character_, scales = NA_character_,
                            space = NA_character_, labeller = NA_character_))
    }
    ref_row <- if (any(x$spec$layer > 0L)) which(x$spec$layer > 0L)[1L] else 1L
    x$spec$facets[[ref_row]]
  } else {
    assert_ggplot(x, arg_name)
    spec_facets(x)
  }
}

#' Extract the spec_labels()-equivalent tibble from a ggplot or ggspec_canon
#' @noRd
.get_labels_tbl <- function(x, arg_name = "x") {
  if (inherits(x, "ggspec_canon")) {
    if (nrow(x$spec) == 0L) return(.empty_labels_tbl())
    ref_row <- if (any(x$spec$layer > 0L)) which(x$spec$layer > 0L)[1L] else 1L
    x$spec$labels[[ref_row]]
  } else {
    assert_ggplot(x, arg_name)
    spec_labels(x)
  }
}

#' Extract the spec_coord()-equivalent tibble from a ggplot or ggspec_canon
#' @noRd
.get_coord_tbl <- function(x, arg_name = "x") {
  if (inherits(x, "ggspec_canon")) {
    if (nrow(x$spec) == 0L) {
      return(tibble::tibble(coord_type = "cartesian",
                            xlim = list(NULL), ylim = list(NULL),
                            expand = TRUE, clip = "on"))
    }
    ref_row <- if (any(x$spec$layer > 0L)) which(x$spec$layer > 0L)[1L] else 1L
    x$spec$coord[[ref_row]]
  } else {
    assert_ggplot(x, arg_name)
    spec_coord(x)
  }
}
