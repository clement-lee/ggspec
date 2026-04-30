# ---------------------------------------------------------------------------
# Visual equivalence pathway
#
# Visual equivalence checks whether two plots produce identical rendered
# output. Unlike structural equivalence (canon()), this pathway calls
# [ggplot2::ggplot_build()] to evaluate plots semantically rather than comparing specs.
#
# Key distinction (CAS analogy):
#   Structural ≡ syntactic equality under a term rewriting system (TRS)
#   Visual     ≡ semantic equality: same evaluation under rendering semantics
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Internal normalisers — operate on ggplot objects, NOT specs
# ---------------------------------------------------------------------------

#' Absorb coord_flip() into a ggplot object by swapping x/y aesthetics
#'
#' If the plot uses coord_flip(), swaps x/y quosures in the global mapping
#' and each layer mapping, then replaces the coordinate system with
#' coord_cartesian(). Returns the plot unchanged if no coord_flip is present.
#' @noRd
.norm_coord_flip <- function(p) {
  if (!inherits(p$coordinates, "CoordFlip")) return(p)

  swap_xy <- function(m) {
    if (is.null(m) || length(m) == 0L) return(m)
    nm <- names(m)
    xi <- match("x", nm, nomatch = 0L)
    yi <- match("y", nm, nomatch = 0L)
    if (xi > 0L && yi > 0L) {
      tmp      <- m[[xi]]
      m[[xi]]  <- m[[yi]]
      m[[yi]]  <- tmp
    } else if (xi > 0L) {
      names(m)[xi] <- "y"
    } else if (yi > 0L) {
      names(m)[yi] <- "x"
    }
    m
  }

  p$mapping <- swap_xy(p$mapping)
  for (i in seq_along(p$layers))
    p$layers[[i]]$mapping <- swap_xy(p$layers[[i]]$mapping)
  p$coordinates <- ggplot2::coord_cartesian()
  p
}

#' Absorb explicit scale name arguments into p$labels
#'
#' For each scale in p$scales$scales that has a non-NULL, non-waiver name,
#' copies that name into the corresponding entry of p$labels and clears the
#' scale name. This makes scale_fill_*(name = "v") and labs(fill = "v")
#' compare identically when checking labels.
#' @noRd
.norm_scale_names <- function(p) {
  if (is.null(p$scales) || length(p$scales$scales) == 0L) return(p)
  for (sc in p$scales$scales) {
    aes_name <- sc$aesthetics[[1L]]
    nm       <- sc$name
    if (is.null(nm) || inherits(nm, "waiver")) next
    p$labels[[aes_name]] <- nm
    sc$name <- ggplot2::waiver()
  }
  p
}

# ---------------------------------------------------------------------------
# equiv_rendered
# ---------------------------------------------------------------------------

#' Compare the rendered (built) layer data of two ggplot objects
#'
#' Calls [ggplot2::ggplot_build()] on both inputs and compares the resulting
#' panel data layer by layer. Useful for detecting visual equivalence between
#' plots that differ structurally (e.g. `geom_bar` on raw data vs `geom_col`
#' on pre-counted data).
#'
#' Comparison is restricted to the key visual columns (position, size, colour,
#' fill, alpha, group, PANEL) that are common to both layers. Rows are sorted
#' by the first shared column before comparison.
#'
#' @param p1 The reference ggplot object.
#' @param p2 The observed ggplot object.
#'
#' @return A `ggspec_result`.
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' p1 <- ggplot(mpg, aes(x = class)) + geom_bar()
#' p2 <- mpg |> count(class) |> ggplot(aes(x = class, y = n)) + geom_col()
#' equiv_rendered(p1, p2)   # TRUE
equiv_rendered <- function(p1, p2) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")

  b1 <- tryCatch(ggplot2::ggplot_build(p1)$data,
                 error = function(e) NULL)
  b2 <- tryCatch(ggplot2::ggplot_build(p2)$data,
                 error = function(e) NULL)

  if (is.null(b1) || is.null(b2)) {
    return(new_ggspec_result(FALSE,
      "ggplot_build() failed on one or both inputs.", check = "rendered"))
  }
  if (length(b1) != length(b2)) {
    return(new_ggspec_result(FALSE,
      sprintf("Different number of rendered layers: %d vs %d.",
              length(b1), length(b2)), check = "rendered"))
  }

  KEY_COLS <- c("x", "y", "xmin", "xmax", "ymin", "ymax",
                "width", "size", "colour", "fill", "alpha", "group", "PANEL")

  for (i in seq_along(b1)) {
    d1     <- b1[[i]]
    d2     <- b2[[i]]
    shared <- intersect(intersect(names(d1), names(d2)), KEY_COLS)
    if (length(shared) == 0L) next

    sort_col <- shared[[1L]]
    ord1 <- order(d1[[sort_col]])
    ord2 <- order(d2[[sort_col]])
    d1s  <- d1[ord1, shared, drop = FALSE]
    d2s  <- d2[ord2, shared, drop = FALSE]
    rownames(d1s) <- NULL
    rownames(d2s) <- NULL

    cmp <- suppressWarnings(
      tryCatch(all.equal(d1s, d2s, tolerance = 1e-9),
               error = function(e) paste("error:", conditionMessage(e)))
    )
    if (!isTRUE(cmp)) {
      return(new_ggspec_result(FALSE,
        sprintf("Rendered layer %d data differ: %s", i,
                if (is.character(cmp)) cmp[[1L]] else "mismatch"),
        check = "rendered"))
    }
  }

  new_ggspec_result(TRUE, "Rendered layer data are identical.",
                    check = "rendered")
}

# ---------------------------------------------------------------------------
# compare_visual
# ---------------------------------------------------------------------------

#' Compare two ggplot objects for visual equivalence
#'
#' `compare_visual()` checks whether two plots produce identical rendered
#' output. It uses [ggplot2::ggplot_build()] for data comparison rather than spec
#' inspection, making it capable of detecting equivalences that structural
#' comparison cannot (e.g. pre-computed vs stat-based geoms, `coord_flip()`
#' with swapped aesthetics).
#'
#' Called internally by [compare_plots()] when `mode = "visual"`.
#'
#' @param p1 The reference ggplot object.
#' @param p2 The observed ggplot object.
#' @param check Character vector of checks to run. Options:
#'   `"rendered"` (compare built layer data), `"labels"` (compare effective
#'   axis/legend labels after absorbing scale names), `"facets"` (compare
#'   facet configuration), `"coord"` (compare coordinate system after
#'   coord_flip normalisation).
#'
#' @return A `ggspec_compare` object.
#' @export
#' @examples
#' library(ggplot2); library(dplyr)
#' p1 <- ggplot(mpg, aes(x = class)) + geom_bar()
#' p2 <- mpg |> count(class) |> ggplot(aes(x = class, y = n)) + geom_col()
#' compare_visual(p1, p2)
compare_visual <- function(p1, p2,
                           check = c("rendered", "labels", "facets", "coord")) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")
  check <- match.arg(check, several.ok = TRUE)

  p1n <- .norm_coord_flip(p1)
  p2n <- .norm_coord_flip(p2)
  q1  <- .norm_scale_names(p1n)
  q2  <- .norm_scale_names(p2n)

  fns <- list(
    rendered = function() equiv_rendered(p1n, p2n),
    labels   = function() equiv_labels(q1, q2),
    facets   = function() equiv_facets(p1n, p2n),
    coord    = function() equiv_coord(p1n, p2n)
  )

  results <- lapply(check, function(nm) {
    r       <- fns[[nm]]()
    r$check <- nm
    r
  })

  result        <- combine_results(results)
  result$mode   <- "visual"
  class(result) <- c("ggspec_compare", class(result))
  result
}
