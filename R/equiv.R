#' Compare two ggplot objects across multiple dimensions
#'
#' `equiv_plot()` is the high-level entry point. It runs the requested
#' per-dimension `equiv_*()` checks and returns a combined `ggspec_result`.
#' Accepts either ggplot objects or `ggspec_canon` objects (produced by
#' [canon()]).
#'
#' @param p1 The reference ggplot or `ggspec_canon` object.
#' @param p2 The observed ggplot or `ggspec_canon` object to compare against
#'   `p1`.
#' @param check Character vector of checks to run. Any subset of
#'   `c("layers", "aes", "scales", "facets", "labels", "coord")`.
#' @param ... Additional arguments passed to individual `equiv_*()` functions.
#'
#' @return A `ggspec_result` object. `as.logical()` on the result gives a
#'   single `TRUE`/`FALSE`.
#' @export
#' @examples
#' ref <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' obs <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_smooth()
#' equiv_plot(ref, obs)
#' equiv_plot(ref, obs, check = "layers")
equiv_plot <- function(p1, p2,
                       check = c("layers", "aes", "scales", "facets",
                                 "labels", "coord"),
                       ...) {
  .assert_gg_input(p1, "p1")
  .assert_gg_input(p2, "p2")
  check <- match.arg(check, several.ok = TRUE)

  fns <- list(
    layers = function() equiv_layers(p1, p2, ...),
    aes    = function() equiv_aes(p1, p2, ...),
    scales = function() equiv_scales(p1, p2, ...),
    facets = function() equiv_facets(p1, p2),
    labels = function() equiv_labels(p1, p2, ...),
    coord  = function() equiv_coord(p1, p2)
  )

  results <- lapply(check, function(nm) {
    r <- fns[[nm]]()
    r$check <- nm
    r
  })

  combine_results(results)
}

# ---------------------------------------------------------------------------
# Layer equivalence
# ---------------------------------------------------------------------------

#' Compare layer structure of two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#' @param exact Logical. If `TRUE`, the number and order of layers must match
#'   exactly. If `FALSE` (default), `p2` must contain at least the layers
#'   present in `p1` (as a subset, ignoring order).
#' @param check_order Logical. If `TRUE` and `exact = FALSE`, layer order must
#'   also match. Ignored when `exact = TRUE`.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' p2 <- p1 + ggplot2::geom_smooth()
#' equiv_layers(p1, p2)           # passes (p1's layers are a subset of p2)
#' equiv_layers(p1, p2, exact = TRUE)  # fails (p2 has more layers)
equiv_layers <- function(p1, p2, exact = FALSE, check_order = FALSE) {
  l1 <- .get_layers_tbl(p1, "p1")
  l2 <- .get_layers_tbl(p2, "p2")

  detail <- .compare_layers_tbl(l1, l2)

  if (exact) {
    pass <- nrow(l1) == nrow(l2) &&
      all(l1$geom == l2$geom) &&
      all(l1$stat == l2$stat)
    msg <- if (pass) "Layer structure matches exactly." else
      sprintf("Expected %d layer(s) [%s]; got %d [%s].",
              nrow(l1), paste(l1$geom, collapse = ", "),
              nrow(l2), paste(l2$geom, collapse = ", "))
  } else {
    missing <- setdiff(l1$geom, l2$geom)
    pass    <- length(missing) == 0L
    msg     <- if (pass) "All expected geoms present." else
      sprintf("Missing geom(s): %s.", paste(missing, collapse = ", "))
  }

  new_ggspec_result(pass = pass, message = msg, detail = detail,
                    check = "layers")
}

# ---------------------------------------------------------------------------
# Aesthetic equivalence
# ---------------------------------------------------------------------------

#' Compare aesthetic mappings of two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#' @param layer Integer vector of layer indices to compare. `NULL` (default)
#'   compares all layers present in `p1`.
#' @param exact Logical. If `FALSE` (default), `p2` must contain at least the
#'   mappings present in `p1` (additional mappings are allowed). If `TRUE`,
#'   the mapping sets must be identical.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) +
#'   ggplot2::geom_point()
#' equiv_aes(p1, p2)           # passes (p1's mappings are a subset)
#' equiv_aes(p1, p2, exact = TRUE)  # fails (p2 has extra colour mapping)
equiv_aes <- function(p1, p2, layer = NULL, exact = FALSE) {
  a1 <- .get_aes_tbl(p1, layer = layer, arg_name = "p1")
  a2 <- .get_aes_tbl(p2, layer = layer, arg_name = "p2")

  detail <- .compare_aes_tbl(a1, a2)
  missing_rows  <- detail[detail$status == "missing",  , drop = FALSE]
  extra_rows    <- detail[detail$status == "extra",    , drop = FALSE]
  mismatch_rows <- detail[detail$status == "mismatch", , drop = FALSE]

  if (exact) {
    pass <- nrow(missing_rows) == 0L && nrow(extra_rows) == 0L &&
            nrow(mismatch_rows) == 0L
    msg  <- if (pass) "Aesthetic mappings match exactly." else
      sprintf("%d missing, %d extra, %d mismatched aesthetic mapping(s).",
              nrow(missing_rows), nrow(extra_rows), nrow(mismatch_rows))
  } else {
    pass <- nrow(missing_rows) == 0L && nrow(mismatch_rows) == 0L
    bad  <- rbind(missing_rows, mismatch_rows)
    msg  <- if (pass) "All expected aesthetic mappings present." else
      sprintf("Aesthetic mapping issue(s): %s.",
              paste(sprintf("%s->%s (layer %d)",
                            bad$aesthetic,
                            bad$variable,
                            bad$layer_idx),
                    collapse = ", "))
  }

  new_ggspec_result(pass = pass, message = msg, detail = detail,
                    check = "aes")
}

# ---------------------------------------------------------------------------
# Scale equivalence
# ---------------------------------------------------------------------------

#' Compare scales of two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#' @param aesthetics Character vector of aesthetics to compare. `NULL`
#'   (default) compares all scales present in `p1`.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() + ggplot2::scale_x_log10()
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' equiv_scales(p1, p2)
equiv_scales <- function(p1, p2, aesthetics = NULL) {
  s1 <- .get_scales_tbl(p1, "p1")
  s2 <- .get_scales_tbl(p2, "p2")

  if (!is.null(aesthetics)) {
    s1 <- s1[s1$aesthetic %in% aesthetics, , drop = FALSE]
    s2 <- s2[s2$aesthetic %in% aesthetics, , drop = FALSE]
  }

  missing_aes <- setdiff(s1$aesthetic, s2$aesthetic)
  type_mismatch <- intersect(s1$aesthetic, s2$aesthetic)
  type_mismatch <- type_mismatch[
    s1$scale_type[match(type_mismatch, s1$aesthetic)] !=
    s2$scale_type[match(type_mismatch, s2$aesthetic)]
  ]

  pass <- length(missing_aes) == 0L && length(type_mismatch) == 0L
  msg  <- if (pass) "Scales match." else {
    parts <- character(0L)
    if (length(missing_aes))   parts <- c(parts, sprintf("missing scale(s): %s", paste(missing_aes, collapse = ", ")))
    if (length(type_mismatch)) parts <- c(parts, sprintf("scale type mismatch for: %s", paste(type_mismatch, collapse = ", ")))
    paste(parts, collapse = "; ")
  }

  new_ggspec_result(pass = pass, message = msg, check = "scales")
}

# ---------------------------------------------------------------------------
# Facet equivalence
# ---------------------------------------------------------------------------

#' Compare facet specification of two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() + ggplot2::facet_wrap(~class)
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() + ggplot2::facet_wrap(~drv)
#' equiv_facets(p1, p2)
equiv_facets <- function(p1, p2) {
  f1 <- .get_facets_tbl(p1, "p1")
  f2 <- .get_facets_tbl(p2, "p2")

  diffs <- character(0L)
  if (!identical(f1$facet_type, f2$facet_type))
    diffs <- c(diffs, sprintf("facet_type: '%s' vs '%s'", f1$facet_type, f2$facet_type))
  if (!.na_equal(f1$rows, f2$rows))
    diffs <- c(diffs, sprintf("rows: '%s' vs '%s'", f1$rows %||% "NA", f2$rows %||% "NA"))
  if (!.na_equal(f1$cols, f2$cols))
    diffs <- c(diffs, sprintf("cols: '%s' vs '%s'", f1$cols %||% "NA", f2$cols %||% "NA"))

  pass <- length(diffs) == 0L
  msg  <- if (pass) "Facets match." else paste("Facet mismatch:", paste(diffs, collapse = "; "))

  new_ggspec_result(pass = pass, message = msg, check = "facets")
}

# ---------------------------------------------------------------------------
# Label equivalence
# ---------------------------------------------------------------------------

#' Compare labels of two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#' @param aesthetics Character vector of label names to compare. `NULL`
#'   (default) compares all labels present in `p1`.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() + ggplot2::labs(title = "My plot")
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() + ggplot2::labs(title = "Different title")
#' equiv_labels(p1, p2)
#' equiv_labels(p1, p2, aesthetics = "x")  # passes (x labels same)
equiv_labels <- function(p1, p2, aesthetics = NULL) {
  l1 <- .get_labels_tbl(p1, "p1")
  l2 <- .get_labels_tbl(p2, "p2")

  if (!is.null(aesthetics)) {
    l1 <- l1[l1$aesthetic %in% aesthetics, , drop = FALSE]
  }

  detail <- dplyr::left_join(
    l1, l2, by = "aesthetic", suffix = c("_ref", "_obs")
  )
  detail$match <- mapply(.na_equal, detail$label_ref, detail$label_obs)

  mismatches <- detail[!detail$match, , drop = FALSE]
  missing    <- mismatches[is.na(mismatches$label_obs), , drop = FALSE]
  wrong      <- mismatches[!is.na(mismatches$label_obs), , drop = FALSE]

  pass <- nrow(mismatches) == 0L
  msg  <- if (pass) "Labels match." else {
    parts <- character(0L)
    if (nrow(missing) > 0L)
      parts <- c(parts, sprintf("missing label(s): %s", paste(missing$aesthetic, collapse = ", ")))
    if (nrow(wrong) > 0L)
      parts <- c(parts, sprintf("wrong label(s): %s",
        paste(sprintf("'%s' (expected '%s', got '%s')",
                      wrong$aesthetic, wrong$label_ref, wrong$label_obs),
              collapse = ", ")))
    paste(parts, collapse = "; ")
  }

  new_ggspec_result(pass = pass, message = msg, detail = detail,
                    check = "labels")
}

# ---------------------------------------------------------------------------
# Coordinate equivalence
# ---------------------------------------------------------------------------

#' Compare coordinate systems of two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' p2 <- p1 + ggplot2::coord_flip()
#' equiv_coord(p1, p2)
equiv_coord <- function(p1, p2) {
  c1 <- .get_coord_tbl(p1, "p1")
  c2 <- .get_coord_tbl(p2, "p2")

  pass <- identical(c1$coord_type, c2$coord_type)
  msg  <- if (pass) "Coordinate systems match." else
    sprintf("Coordinate system mismatch: expected '%s', got '%s'.",
            c1$coord_type, c2$coord_type)

  new_ggspec_result(pass = pass, message = msg, check = "coord")
}

# ---------------------------------------------------------------------------
# Layer parameter equivalence
# ---------------------------------------------------------------------------

#' Compare parameters of a specific layer in two ggplot objects
#'
#' @param p1 Reference ggplot or `ggspec_canon` object.
#' @param p2 Observed ggplot or `ggspec_canon` object.
#' @param layer Integer: which layer index to compare (1-based). Compared by
#'   position.
#' @param params Character vector of parameter names to check. `NULL`
#'   (default) checks all parameters present in `p1`'s layer.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_smooth(method = "lm", se = FALSE)
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_smooth(method = "loess", se = TRUE)
#' equiv_params(p1, p2, layer = 1L, params = c("se"))
equiv_params <- function(p1, p2, layer = 1L, params = NULL) {
  layer <- as.integer(layer)
  l1 <- .get_layers_tbl(p1, "p1")
  l2 <- .get_layers_tbl(p2, "p2")

  if (layer > nrow(l1) || layer > nrow(l2)) {
    return(new_ggspec_result(
      pass = FALSE,
      message = sprintf("Layer %d does not exist in both plots.", layer),
      check = "params"
    ))
  }

  p1_params <- l1$params[[layer]]
  p2_params <- l2$params[[layer]]

  keys <- if (!is.null(params)) params else names(p1_params)
  keys <- intersect(keys, names(p1_params))

  diffs <- vapply(keys, function(k) {
    !isTRUE(identical(p1_params[[k]], p2_params[[k]]))
  }, logical(1L))

  mismatched <- keys[diffs]
  pass <- length(mismatched) == 0L
  msg  <- if (pass) sprintf("Layer %d parameters match.", layer) else
    sprintf("Layer %d parameter mismatch: %s.", layer,
            paste(mismatched, collapse = ", "))

  new_ggspec_result(pass = pass, message = msg, check = "params")
}

# ---------------------------------------------------------------------------
# Data equivalence
# ---------------------------------------------------------------------------

#' Compare the data of a layer in two ggplot objects
#'
#' Compares by hashing (using [rlang::hash()]), so large data frames are
#' handled efficiently. Column order is ignored; row order is ignored.
#' Only works with raw ggplot objects (not `ggspec_canon`).
#'
#' @param p1 Reference ggplot object.
#' @param p2 Observed ggplot object.
#' @param layer Integer vector of layer indices to compare. `NULL` (default)
#'   compares the plot-level data.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' p2 <- ggplot2::ggplot(
#'   ggplot2::mpg[ggplot2::mpg$class == "suv", ],
#'   ggplot2::aes(displ, hwy)
#' ) + ggplot2::geom_point()
#' equiv_data(p1, p2)
equiv_data <- function(p1, p2, layer = NULL) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")

  .hash_data <- function(p, idx) {
    d <- if (is.null(idx)) p$data else {
      if (idx > length(p$layers)) return(NA_character_)
      p$layers[[idx]]$data
    }
    if (is.null(d) || (is.data.frame(d) && nrow(d) == 0L)) return("empty")
    rlang::hash(d[order(names(d))])
  }

  h1 <- .hash_data(p1, layer)
  h2 <- .hash_data(p2, layer)

  pass <- identical(h1, h2)
  ctx  <- if (is.null(layer)) "plot-level" else sprintf("layer %d", layer)
  msg  <- if (pass) sprintf("Data matches (%s).", ctx) else
    sprintf("Data differs (%s).", ctx)

  new_ggspec_result(pass = pass, message = msg, check = "data")
}

# ---------------------------------------------------------------------------
# Internal helpers for equiv functions
# ---------------------------------------------------------------------------

.na_equal <- function(x, y) {
  (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && identical(x, y))
}

.compare_layers_tbl <- function(l1, l2) {
  tibble::tibble(
    source    = c(rep("ref", nrow(l1)), rep("obs", nrow(l2))),
    layer_idx = c(l1$layer_idx, l2$layer_idx),
    geom      = c(l1$geom, l2$geom),
    stat      = c(l1$stat, l2$stat),
    position  = c(l1$position, l2$position)
  )
}

.compare_aes_tbl <- function(a1, a2) {
  key1 <- paste(a1$layer_idx, a1$aesthetic, sep = "::")
  key2 <- paste(a2$layer_idx, a2$aesthetic, sep = "::")

  missing_idx <- which(!key1 %in% key2)
  extra_idx   <- which(!key2 %in% key1)

  bind_rows_safe <- function(...) {
    args <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
    if (length(args) == 0L) return(.empty_aes_tbl())
    dplyr::bind_rows(args)
  }

  missing_tbl <- if (length(missing_idx) > 0L)
    dplyr::mutate(a1[missing_idx, ], status = "missing") else NULL
  extra_tbl   <- if (length(extra_idx)   > 0L)
    dplyr::mutate(a2[extra_idx,   ], status = "extra")   else NULL
  matched_idx1 <- which(key1 %in% key2)
  matched_idx2 <- match(key1[matched_idx1], key2)
  match_ok   <- matched_idx1[a1$variable[matched_idx1] == a2$variable[matched_idx2]]
  match_bad  <- matched_idx1[a1$variable[matched_idx1] != a2$variable[matched_idx2]]
  ok_tbl     <- if (length(match_ok)  > 0L) dplyr::mutate(a1[match_ok,  ], status = "match")    else NULL
  bad_tbl    <- if (length(match_bad) > 0L) dplyr::mutate(a1[match_bad, ], status = "mismatch") else NULL

  bind_rows_safe(missing_tbl, extra_tbl, ok_tbl, bad_tbl)
}
