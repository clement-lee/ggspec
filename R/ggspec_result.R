#' Constructor for ggspec comparison results
#'
#' `ggspec_result` objects are returned by all `equiv_*()` and `check_plot()`
#' functions. They carry a pass/fail flag, a human-readable message, and
#' (optionally) a structured diff data frame for programmatic inspection.
#'
#' @param pass Logical scalar.
#' @param message Character scalar: human-readable summary.
#' @param detail Optional data frame with per-item comparison details, or
#'   `NULL`.
#' @param check Character scalar naming the check that produced this result
#'   (e.g. `"layers"`, `"aes"`).
#' @param hint Optional character scalar: a prescriptive suggestion for what to
#'   change to make the comparison pass. `NA_character_` when no specific
#'   guidance is available.
#'
#' @return An object of class `ggspec_result` (a named list).
#' @keywords internal
new_ggspec_result <- function(pass, message, detail = NULL,
                               check = NA_character_, hint = NA_character_) {
  structure(
    list(pass = pass, message = message, detail = detail,
         check = check, hint = hint),
    class = "ggspec_result"
  )
}

#' @export
print.ggspec_result <- function(x, ...) {
  icon <- if (isTRUE(x$pass)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", icon, x$message))
  if (!isTRUE(x$pass) && !is.na(x$hint))
    cat(sprintf("  Hint: %s\n", x$hint))
  if (!is.null(x$detail) && nrow(x$detail) > 0L) {
    cat("  Detail:\n")
    print(x$detail, ...)
  }
  invisible(x)
}

#' @export
as.logical.ggspec_result <- function(x, ...) x$pass

#' Combine multiple `ggspec_result` objects into one
#'
#' The combined result passes only if all sub-results pass. The message
#' summarises how many checks passed.
#'
#' @param results A list of `ggspec_result` objects.
#' @return A single `ggspec_result`.
#' @keywords internal
combine_results <- function(results) {
  pass    <- all(vapply(results, function(r) isTRUE(r$pass), logical(1L)))
  n_pass  <- sum(vapply(results, function(r) isTRUE(r$pass), logical(1L)))
  n_total <- length(results)
  msg     <- sprintf("%d/%d checks passed", n_pass, n_total)

  if (!pass) {
    failed_msgs <- vapply(
      results[!vapply(results, function(r) isTRUE(r$pass), logical(1L))],
      function(r) r$message,
      character(1L)
    )
    msg <- paste0(msg, ": ", paste(failed_msgs, collapse = "; "))
  }

  detail <- dplyr::bind_rows(lapply(results, function(r) {
    if (is.null(r$detail)) return(NULL)
    dplyr::mutate(r$detail, check = r$check, .before = 1L)
  }))

  # Special hint: rendered passes but labels fail → bar-height-vs-label mismatch
  checks_pass <- vapply(results, function(r) r$check, character(1L))[
    vapply(results, function(r) isTRUE(r$pass), logical(1L))]
  checks_fail <- vapply(results, function(r) r$check, character(1L))[
    !vapply(results, function(r) isTRUE(r$pass), logical(1L))]
  hint <- if (!pass && "rendered" %in% checks_pass && "labels" %in% checks_fail) {
    "Rendered output matches but labels differ. Add labs() to align axis/legend titles."
  } else if (!pass) {
    failed_hints <- vapply(
      results[!vapply(results, function(r) isTRUE(r$pass), logical(1L))],
      function(r) if (!is.na(r$hint)) r$hint else NA_character_,
      character(1L)
    )
    non_na <- failed_hints[!is.na(failed_hints)]
    if (length(non_na) > 0L) paste(non_na, collapse = " | ") else NA_character_
  } else NA_character_

  new_ggspec_result(pass = pass, message = msg,
                    detail = if (nrow(detail) == 0L) NULL else detail,
                    check = "combined", hint = hint)
}
