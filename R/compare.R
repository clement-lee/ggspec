#' Compare two ggplot objects
#'
#' `compare_plots()` is the high-level entry point for comparing two ggplot
#' objects. The `mode` argument selects which equivalence pathway to use:
#'
#' - `"strict"` / `"structural"`: spec-level comparison via [canon()].
#'   Plots that are structurally equivalent after canonicalisation
#'   (different data/mapping placement, layer order, `geom_col` vs
#'   `geom_bar(stat="identity")`) are detected as equal.
#' - `"visual"`: rendered-output comparison via [compare_visual()].
#'   Uses [ggplot2::ggplot_build()] rather than spec inspection; detects
#'   equivalences that structural comparison cannot (e.g. pre-computed vs
#'   stat-based geoms, `coord_flip()` with swapped aesthetics).
#' - `"conceptual"`: communicative-intent comparison via
#'   [compare_conceptual()]. Detects plots that convey the same information
#'   using different visual encodings (e.g. histogram vs density).
#'
#' [equiv_plot()] is equivalent to `compare_plots(mode = "strict")`.
#'
#' @param p1 The reference ggplot object.
#' @param p2 The observed ggplot object to compare against `p1`.
#' @param mode One of `"strict"`, `"structural"` (default), `"visual"`, or
#'   `"conceptual"`.
#' @param check Character vector of checks to run. For structural modes:
#'   any of `"layers"`, `"aes"`, `"scales"`, `"facets"`, `"labels"`,
#'   `"coord"`. For visual mode: any of `"rendered"`, `"labels"`, `"facets"`,
#'   `"coord"`. Ignored for conceptual mode.
#' @param ... Additional arguments passed to individual `equiv_*()` functions
#'   (structural modes only).
#'
#' @return A `ggspec_compare` object (extends `ggspec_result`) with the usual
#'   `$pass`, `$message`, and `$detail` fields, plus `$mode`.  For structural
#'   modes, also contains `$canon_p1` and `$canon_p2`.
#'
#' @export
#' @examples
#' # Global vs per-layer data placement: fails at strict, passes at structural
#' library(ggplot2)
#' p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#' p2 <- ggplot(mpg) + geom_point(aes(displ, hwy))
#' as.logical(compare_plots(p1, p2, mode = "strict"))      # FALSE
#' as.logical(compare_plots(p1, p2, mode = "structural"))  # TRUE
#'
#' # Visual equivalence: geom_bar vs geom_col on pre-counted data
#' library(dplyr)
#' pb <- ggplot(mpg, aes(x = class)) + geom_bar()
#' pc <- mpg |> count(class) |> ggplot(aes(x = class, y = n)) + geom_col()
#' as.logical(compare_plots(pb, pc, mode = "structural"))  # FALSE
#' as.logical(compare_plots(pb, pc, mode = "visual"))      # TRUE
compare_plots <- function(p1, p2,
                          mode  = "structural",
                          check = NULL,
                          ...) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")
  mode <- match.arg(mode, c("strict", "structural", "visual", "conceptual"))

  if (mode == "visual") {
    check <- if (is.null(check)) {
      c("rendered", "labels", "facets", "coord")
    } else {
      match.arg(check,
                c("rendered", "labels", "facets", "coord"),
                several.ok = TRUE)
    }
    return(compare_visual(p1, p2, check = check))
  }

  if (mode == "conceptual") {
    return(compare_conceptual(p1, p2))
  }

  check <- if (is.null(check)) {
    c("layers", "aes", "scales", "facets", "labels", "coord")
  } else {
    match.arg(check,
              c("layers", "aes", "scales", "facets", "labels", "coord"),
              several.ok = TRUE)
  }

  c1 <- canon(p1, mode = mode)
  c2 <- canon(p2, mode = mode)

  fns <- list(
    layers = function() equiv_layers(c1, c2, ...),
    aes    = function() equiv_aes(c1, c2, ...),
    scales = function() equiv_scales(c1, c2, ...),
    facets = function() equiv_facets(c1, c2),
    labels = function() equiv_labels(c1, c2, ...),
    coord  = function() equiv_coord(c1, c2)
  )

  results <- lapply(check, function(nm) {
    r        <- fns[[nm]]()
    r$check  <- nm
    r
  })

  result          <- combine_results(results)
  result$canon_p1 <- c1
  result$canon_p2 <- c2
  result$mode     <- mode
  class(result)   <- c("ggspec_compare", class(result))

  result
}

#' @export
print.ggspec_compare <- function(x, ...) {
  icon <- if (isTRUE(x$pass)) "PASS" else "FAIL"
  cat(sprintf("[%s mode=%s] %s\n", icon, x$mode, x$message))
  if (!is.null(x$detail) && nrow(x$detail) > 0L) {
    cat("  Detail:\n")
    print(x$detail, ...)
  }
  invisible(x)
}
