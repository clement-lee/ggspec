#' Extract scale specifications as a tidy data frame
#'
#' Returns one row per explicitly added scale. Scales that were never added
#' by the user (i.e. default scales inferred at render time) are not included
#' because they do not exist in the ggplot object before `ggplot_build()` is
#' called; use [spec_plot()] to access built scales.
#'
#' @param p A ggplot object.
#'
#' @return A [tibble::tibble()] with one row per scale and columns:
#'   \describe{
#'     \item{`aesthetic`}{The aesthetic the scale controls, e.g. `"x"`,
#'       `"colour"`.}
#'     \item{`scale_class`}{Full S3 class string of the scale object.}
#'     \item{`scale_type`}{Short type label, e.g. `"continuous"`,
#'       `"discrete"`, `"binned"`, `"manual"`, `"identity"`.}
#'     \item{`name`}{Scale name / axis label (`waiver()` reported as `NA`).}
#'     \item{`transform`}{Name of the transformation applied (continuous scales
#'       only; `NA` otherwise).}
#'     \item{`guide`}{Guide type as a string, e.g. `"legend"`, `"colourbar"`,
#'       `"none"`.}
#'   }
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_colour_brewer(palette = "Set1") +
#'   ggplot2::scale_x_log10()
#' spec_scales(p)
spec_scales <- function(p) {
  assert_ggplot(p)
  scales <- p$scales$scales
  if (length(scales) == 0L) return(.empty_scales_tbl())

  rows <- lapply(scales, function(s) {
    aes_str <- paste(s$aesthetics, collapse = ", ")
    cls     <- class(s)
    type    <- .scale_type(cls)
    nm      <- .scale_name(s)
    trans   <- .scale_transform(s)
    guide   <- .scale_guide(s)

    tibble::tibble(
      aesthetic   = aes_str,
      scale_class = cls[[1L]],
      scale_type  = type,
      name        = nm,
      transform   = trans,
      guide       = guide
    )
  })

  dplyr::bind_rows(rows)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.scale_type <- function(cls) {
  types <- c("continuous", "discrete", "binned", "manual", "identity",
             "gradient", "gradient2", "gradientn", "viridis", "brewer",
             "distiller", "fermenter", "steps", "steps2", "stepsn")
  matched <- types[vapply(types, function(t) any(grepl(t, cls, ignore.case = TRUE)), logical(1L))]
  if (length(matched)) matched[[1L]] else "unknown"
}

.scale_name <- function(s) {
  nm <- s$name
  if (inherits(nm, "waiver") || is.null(nm)) NA_character_ else as.character(nm)
}

.scale_transform <- function(s) {
  # transformation is stored differently across ggplot2 versions
  tr <- tryCatch(s$trans$name %||% s$transform$name, error = function(e) NULL)
  if (is.null(tr) || identical(tr, "identity")) NA_character_ else as.character(tr)
}

.scale_guide <- function(s) {
  g <- s$guide
  if (is.character(g))      g
  else if (is.null(g))      NA_character_
  else if (is.list(g) && !is.null(g$guide_class)) g$guide_class[[1L]]
  else                      class(g)[[1L]]
}

.empty_scales_tbl <- function() {
  tibble::tibble(
    aesthetic   = character(),
    scale_class = character(),
    scale_type  = character(),
    name        = character(),
    transform   = character(),
    guide       = character()
  )
}
