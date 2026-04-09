#' Extract coordinate system specification as a tidy data frame
#'
#' Returns a single-row data frame describing the plot's coordinate system.
#'
#' @param p A ggplot object.
#'
#' @return A [tibble::tibble()] with one row and columns:
#'   \describe{
#'     \item{`coord_type`}{Short name of the coordinate system, e.g.
#'       `"cartesian"`, `"flip"`, `"polar"`, `"sf"`.}
#'     \item{`xlim`}{List-column: numeric vector of x limits, or `NULL` if not
#'       set.}
#'     \item{`ylim`}{List-column: numeric vector of y limits, or `NULL` if not
#'       set.}
#'     \item{`expand`}{Logical: whether axes are expanded beyond the data
#'       range.}
#'     \item{`clip`}{Whether clipping is applied: `"on"`, `"off"`, or
#'       `NA`.}
#'   }
#'
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' spec_coord(p1)
#'
#' p2 <- p1 + ggplot2::coord_flip()
#' spec_coord(p2)
#'
#' p3 <- p1 + ggplot2::coord_polar()
#' spec_coord(p3)
spec_coord <- function(p) {
  assert_ggplot(p)
  coord <- p$coordinates
  cls   <- class(coord)

  type  <- tolower(sub("^Coord", "", cls[[1L]]))
  xlim  <- tryCatch(coord$limits$x, error = function(e) NULL)
  ylim  <- tryCatch(coord$limits$y, error = function(e) NULL)
  exp   <- tryCatch(isTRUE(coord$expand), error = function(e) NA)
  clip  <- tryCatch(as.character(coord$clip), error = function(e) NA_character_)

  tibble::tibble(
    coord_type = type,
    xlim       = list(xlim),
    ylim       = list(ylim),
    expand     = exp,
    clip       = clip
  )
}
