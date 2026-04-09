#' Extract plot labels as a tidy data frame
#'
#' Returns one row per label currently set on the plot (title, subtitle,
#' caption, tag, and any aesthetic labels such as x, y, colour, fill, etc.).
#'
#' @param p A ggplot object.
#'
#' @return A [tibble::tibble()] with one row per label and columns:
#'   \describe{
#'     \item{`aesthetic`}{The name of the label slot, e.g. `"title"`, `"x"`,
#'       `"colour"`.}
#'     \item{`label`}{The label string. `NA` if the slot is `NULL` or a
#'       `waiver()`.}
#'   }
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Engine vs highway MPG", x = "Displacement (L)")
#' spec_labels(p)
spec_labels <- function(p) {
  assert_ggplot(p)
  labs <- p$labels
  if (length(labs) == 0L) return(.empty_labels_tbl())

  nms <- names(labs)
  vals <- vapply(labs, function(l) {
    if (is.null(l) || inherits(l, "waiver")) NA_character_
    else as.character(l)
  }, character(1L))

  tibble::tibble(aesthetic = nms, label = unname(vals))
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.empty_labels_tbl <- function() {
  tibble::tibble(aesthetic = character(), label = character())
}
