#' Compare two ggplot objects after canonicalisation
#'
#' `compare_plots()` is the high-level, canonicalisation-aware entry point for
#' comparing two ggplot objects. It applies [canon()] to both inputs before
#' running the structural checks, so plots that are semantically equivalent
#' but expressed differently — different data/mapping placement, different
#' layer order, `coord_flip()` vs swapped aesthetics, scale `name` vs
#' `labs()` — are detected as equal.
#'
#' [equiv_plot()] is equivalent to `compare_plots(mode = "strict")`.
#'
#' @param p1 The reference ggplot object.
#' @param p2 The observed ggplot object to compare against `p1`.
#' @param mode Canonicalisation mode passed to [canon()].  One of
#'   `"strict"` (default `"structural"`), `"structural"` (default),
#'   `"visual"`, or `"pedagogical"`.
#' @param check Character vector of checks to run.
#' @param ... Additional arguments passed to individual `equiv_*()` functions.
#'
#' @return A `ggspec_compare` object (extends `ggspec_result`) with the usual
#'   `$pass`, `$message`, and `$detail` fields, plus:
#'   \describe{
#'     \item{`$canon_p1`}{The `ggspec_canon` object for `p1`.}
#'     \item{`$canon_p2`}{The `ggspec_canon` object for `p2`.}
#'     \item{`$mode`}{The canonicalisation mode used.}
#'   }
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
#' # Layer order equivalence in structural mode
#' p3 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
#' p4 <- ggplot(mpg, aes(displ, hwy)) + geom_smooth() + geom_point()
#' as.logical(compare_plots(p3, p4, check = "layers"))  # TRUE
compare_plots <- function(p1, p2,
                          mode  = "structural",
                          check = c("layers", "aes", "scales", "facets",
                                    "labels", "coord"),
                          ...) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")
  mode  <- match.arg(mode, c("strict", "structural", "visual", "pedagogical"))
  check <- match.arg(check, several.ok = TRUE)

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
