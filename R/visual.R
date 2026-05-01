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
    if (inherits(nm, "waiver")) next
    # NULL name means "remove label" — propagate into p$labels so it compares
    # identically to labs(aes = NULL)
    p$labels[[aes_name]] <- nm
    sc$name <- ggplot2::waiver()
  }
  p
}

#' Absorb guide titles into p$labels
#'
#' For each entry in p$guides that carries a guide_legend (or guide_colourbar)
#' with an explicit title, copies that title into p$labels so that
#' guides(colour = guide_legend("Title")) and labs(colour = "Title") compare
#' identically.
#' @noRd
.norm_guide_labels <- function(p) {
  if (is.null(p$guides)) return(p)
  # ggplot2 >= 3.5: $guides is a Guides ggproto object; individual guides
  # live in $guides$guides keyed by aesthetic, each with $params$title.
  # Older ggplot2: $guides is a plain list.
  guides_list <- if (inherits(p$guides, "Guides")) {
    p$guides$guides
  } else {
    p$guides
  }
  if (is.null(guides_list) || length(guides_list) == 0L) return(p)
  for (aes_name in names(guides_list)) {
    g <- guides_list[[aes_name]]
    if (is.null(g) || identical(g, "none")) next
    # ggproto guide objects store the title in $params$title
    ttl <- if (inherits(g, "ggproto")) g$params$title else g$title
    if (is.null(ttl)) {
      p$labels[[aes_name]] <- NULL
    } else if (!inherits(ttl, "waiver")) {
      p$labels[[aes_name]] <- ttl
    }
  }
  p
}

#' Absorb theme element_blank label removals into p$labels
#'
#' When axis.title.x / axis.title.y / legend.title is set to element_blank(),
#' the axis/legend title is visually hidden.  This normaliser propagates that
#' intent into p$labels so it compares identically to labs(x = NULL) etc.
#' @noRd
.norm_theme_labels <- function(p) {
  th <- p$theme
  if (is.null(th)) return(p)
  for (aes_axis in c("x", "y")) {
    key <- paste0("axis.title.", aes_axis)
    el  <- th[[key]]
    if (!is.null(el) && inherits(el, "element_blank"))
      p$labels[[aes_axis]] <- NULL
  }
  if (!is.null(th[["legend.title"]]) &&
      inherits(th[["legend.title"]], "element_blank")) {
    legend_aes <- setdiff(names(p$labels),
                          c("x", "y", "title", "subtitle", "caption", "tag"))
    for (a in legend_aes) p$labels[[a]] <- NULL
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
  # Path-type geoms connect points in data order; sorting before comparison
  # would lose that information and produce false positives.
  PATH_GEOMS <- c("path", "line", "area", "ribbon", "step")

  for (i in seq_along(b1)) {
    d1     <- b1[[i]]
    d2     <- b2[[i]]
    shared <- intersect(intersect(names(d1), names(d2)), KEY_COLS)
    if (length(shared) == 0L) next

    # Detect whether this layer is order-sensitive (path-type in p1)
    geom_i <- if (i <= length(p1$layers)) {
      tolower(sub("^Geom", "", class(p1$layers[[i]]$geom)[[1L]]))
    } else "unknown"
    sort_rows <- !geom_i %in% PATH_GEOMS

    if (sort_rows) {
      sort_col  <- shared[[1L]]
      d1s <- d1[order(d1[[sort_col]]), shared, drop = FALSE]
      d2s <- d2[order(d2[[sort_col]]), shared, drop = FALSE]
    } else {
      d1s <- d1[, shared, drop = FALSE]
      d2s <- d2[, shared, drop = FALSE]
    }
    rownames(d1s) <- NULL
    rownames(d2s) <- NULL
    # Strip per-column attributes (e.g. ggplot2 internal 'n' on group) so
    # all.equal() does not fail on implementation details unrelated to values.
    for (col in shared) {
      attributes(d1s[[col]]) <- NULL
      attributes(d2s[[col]]) <- NULL
    }

    cmp <- suppressWarnings(
      tryCatch(all.equal(d1s, d2s, tolerance = 1e-9),
               error = function(e) paste("error:", conditionMessage(e)))
    )
    if (!isTRUE(cmp)) {
      return(new_ggspec_result(FALSE,
        sprintf("Rendered layer %d data differ: %s", i,
                if (is.character(cmp)) cmp[[1L]] else "mismatch"),
        check = "rendered",
        hint = "Rendered output differs; check that the underlying data produces the same values."))
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
  q1  <- .norm_theme_labels(.norm_guide_labels(.norm_scale_names(p1n)))
  q2  <- .norm_theme_labels(.norm_guide_labels(.norm_scale_names(p2n)))

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
