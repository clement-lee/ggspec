#' Framework-agnostic plot check
#'
#' Compares a student/observed plot against a reference plot and calls
#' `fail_fn` if any check fails. By default `fail_fn = stop`, so the function
#' works in any R context. Swap in `gradethis::fail()` for learnr grading or a
#' `testthat` expectation for unit testing.
#'
#' @param p The observed ggplot object (e.g., the student's plot).
#' @param expected The reference ggplot object.
#' @param check Character vector of checks to run; passed to [equiv_plot()] or
#'   [compare_plots()].
#' @param mode Character scalar: comparison mode. If `NULL` (default),
#'   [equiv_plot()] is used (direct comparison). If `"strict"` or
#'   `"structural"`, [compare_plots()] is used with [canon()]. If `"visual"`
#'   or `"conceptual"`, [compare_plots()] dispatches to [compare_visual()] or
#'   [compare_conceptual()] respectively.
#' @param fail_fn A function called with the failure message string when the
#'   check does not pass. Defaults to [stop()]. Useful alternatives:
#'   `gradethis::fail`, `warning`, `message`, or a custom function.
#' @param pass_fn A function called with the success message string when all
#'   checks pass. Defaults to [invisible()].
#' @param ... Additional arguments passed to [equiv_plot()] or
#'   [compare_plots()].
#'
#' @return The `ggspec_result` invisibly. Side effects (calling `fail_fn` or
#'   `pass_fn`) are the primary interface.
#' @export
#' @examples
#' ref <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#'
#' obs_correct <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#'
#' obs_wrong <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_line()
#'
#' # Passes silently (direct comparison)
#' check_plot(obs_correct, ref, check = "layers")
#'
#' # Passes silently (structural mode: layer order ignored)
#' obs_reordered <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' check_plot(obs_reordered, ref, check = "layers", mode = "structural")
#'
#' # Calls stop() with a message
#' tryCatch(
#'   check_plot(obs_wrong, ref, check = "layers"),
#'   error = function(e) message("Caught: ", conditionMessage(e))
#' )
check_plot <- function(p, expected,
                       check   = NULL,
                       mode    = NULL,
                       fail_fn = stop,
                       pass_fn = invisible,
                       ...) {
  assert_ggplot(p,        "p")
  assert_ggplot(expected, "expected")

  result <- if (!is.null(mode)) {
    compare_plots(p1 = expected, p2 = p, mode = mode, check = check, ...)
  } else {
    check_arg <- check %||% c("layers", "aes", "scales", "facets", "labels", "coord")
    equiv_plot(p1 = expected, p2 = p, check = check_arg, ...)
  }

  if (!isTRUE(result$pass)) {
    fail_fn(result$message)
  } else {
    pass_fn(result$message)
  }

  invisible(result)
}

#' testthat expectation for plot equivalence
#'
#' Wraps [check_plot()] as a `testthat` expectation. Requires the `testthat`
#' package (listed in `Suggests`).
#'
#' @param p The observed ggplot object.
#' @param expected The reference ggplot object.
#' @param check Character vector of checks to run; passed to [equiv_plot()].
#' @param ... Additional arguments passed to [equiv_plot()].
#'
#' @return Called for its side effects (a testthat expectation).
#' @export
#' @examples
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   # Inside a testthat test:
#'   testthat::test_that("plot has correct layers", {
#'     ref <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'       ggplot2::geom_point()
#'     obs <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'       ggplot2::geom_point()
#'     expect_equiv_plot(obs, ref, check = "layers")
#'   })
#' }
expect_equiv_plot <- function(p, expected,
                               check = c("layers", "aes", "scales", "facets",
                                         "labels", "coord"),
                               ...) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    rlang::abort(
      "`expect_equiv_plot()` requires the 'testthat' package. Install it with install.packages('testthat').",
      class = "ggspec_missing_pkg"
    )
  }

  result <- equiv_plot(p1 = expected, p2 = p, check = check, ...)

  testthat::expect(
    ok       = isTRUE(result$pass),
    failure_message = result$message
  )

  invisible(result)
}
