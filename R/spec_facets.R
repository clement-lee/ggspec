#' Extract facet specification as a tidy data frame
#'
#' Returns a single-row data frame describing the plot's faceting. When no
#' faceting is applied, `facet_type` is `"null"` and all other columns are
#' `NA`.
#'
#' @param p A ggplot object.
#'
#' @return A [tibble::tibble()] with one row and columns:
#'   \describe{
#'     \item{`facet_type`}{One of `"null"`, `"wrap"`, or `"grid"`.}
#'     \item{`rows`}{For `facet_grid()`: character representation of the row
#'       faceting variable(s). `NA` otherwise.}
#'     \item{`cols`}{Character representation of the column faceting
#'       variable(s). For `facet_wrap()` this holds the wrap variable(s).}
#'     \item{`scales`}{Scale freedom: `"fixed"`, `"free"`, `"free_x"`, or
#'       `"free_y"`.}
#'     \item{`space`}{Space freedom (grid only): `"fixed"`, `"free"`, etc.}
#'     \item{`labeller`}{Labeller as a string, e.g. `"label_value"`.}
#'   }
#'
#' @export
#' @examples
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(~class)
#' spec_facets(p1)
#'
#' p2 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_grid(drv ~ cyl)
#' spec_facets(p2)
spec_facets <- function(p) {
  assert_ggplot(p)
  facet <- p$facet
  cls   <- class(facet)

  if (any(grepl("FacetNull", cls))) {
    return(tibble::tibble(
      facet_type = "null",
      rows       = NA_character_,
      cols       = NA_character_,
      scales     = NA_character_,
      space      = NA_character_,
      labeller   = NA_character_
    ))
  }

  params <- facet$params

  if (any(grepl("FacetWrap", cls))) {
    cols_str <- .facet_vars_str(params$facets)
    tibble::tibble(
      facet_type = "wrap",
      rows       = NA_character_,
      cols       = cols_str,
      scales     = .facet_scales(params),
      space      = NA_character_,
      labeller   = .labeller_str(params$labeller)
    )
  } else if (any(grepl("FacetGrid", cls))) {
    rows_str <- .facet_vars_str(params$rows)
    cols_str <- .facet_vars_str(params$cols)
    tibble::tibble(
      facet_type = "grid",
      rows       = rows_str,
      cols       = cols_str,
      scales     = .facet_scales(params),
      space      = .facet_space(params),
      labeller   = .labeller_str(params$labeller)
    )
  } else {
    # Unknown custom facet type
    tibble::tibble(
      facet_type = cls[[1L]],
      rows       = NA_character_,
      cols       = NA_character_,
      scales     = NA_character_,
      space      = NA_character_,
      labeller   = NA_character_
    )
  }
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.facet_vars_str <- function(vars) {
  if (is.null(vars) || length(vars) == 0L) return(NA_character_)
  nms <- if (is.list(vars)) names(vars) else vapply(vars, rlang::as_label, character(1L))
  paste(nms, collapse = ", ")
}

.facet_scales <- function(params) {
  s <- params$free
  if (is.null(s)) {
    sc <- params$scales
    if (is.null(sc)) return("fixed") else return(sc)
  }
  if (isTRUE(s$x) && isTRUE(s$y)) "free"
  else if (isTRUE(s$x))            "free_x"
  else if (isTRUE(s$y))            "free_y"
  else                               "fixed"
}

.facet_space <- function(params) {
  s <- params$space_free
  if (is.null(s)) {
    sp <- params$space
    if (is.null(sp)) return("fixed") else return(sp)
  }
  if (isTRUE(s$x) && isTRUE(s$y)) "free"
  else if (isTRUE(s$x))            "free_x"
  else if (isTRUE(s$y))            "free_y"
  else                               "fixed"
}

.labeller_str <- function(labeller) {
  if (is.null(labeller)) return(NA_character_)
  if (is.function(labeller)) return(deparse(substitute(labeller)))
  as.character(labeller)[[1L]]
}
