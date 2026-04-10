#' Compare two ggplot objects after canonicalisation
#'
#' `compare_plots()` is the high-level, canonicalisation-aware entry point for
#' comparing two ggplot objects. It applies [canon()] to both inputs before
#' running the structural checks, so plots that are semantically equivalent
#' but expressed differently (e.g. different layer order, `coord_flip()` vs
#' swapped aesthetics, scale `name` vs `labs()`) are detected as equal.
#'
#' For direct, non-canonicalising comparison use [equiv_plot()] instead.
#'
#' @param p1 The reference ggplot object.
#' @param p2 The observed ggplot object to compare against `p1`.
#' @param mode Canonicalisation mode passed to [canon()].  One of
#'   `"strict"`, `"structural"` (default), `"visual"`, or `"pedagogical"`.
#' @param check Character vector of checks to run; passed to [equiv_plot()].
#' @param ... Additional arguments passed to [equiv_plot()].
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
#' # Layer order equivalence in structural mode
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() + ggplot2::geom_smooth()
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_smooth() + ggplot2::geom_point()
#' as.logical(compare_plots(p1, p2, check = "layers"))  # TRUE
#'
#' # coord_flip / aes-swap equivalence in visual mode
#' d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
#' pa <- ggplot2::ggplot(d, ggplot2::aes(g, v)) +
#'   ggplot2::geom_col() + ggplot2::coord_flip()
#' pb <- ggplot2::ggplot(d, ggplot2::aes(v, g)) +
#'   ggplot2::geom_col()
#' as.logical(compare_plots(pa, pb, mode = "visual", check = c("layers", "coord")))
compare_plots <- function(p1, p2,
                          mode  = "structural",
                          check = c("layers", "aes", "scales", "facets",
                                    "labels", "coord"),
                          ...) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")
  check <- match.arg(check, several.ok = TRUE)

  canon_p1 <- canon(p1, mode = mode)
  canon_p2 <- canon(p2, mode = mode)

  result        <- equiv_plot(canon_p1, canon_p2, check = check, ...)
  result$canon_p1 <- canon_p1
  result$canon_p2 <- canon_p2
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
