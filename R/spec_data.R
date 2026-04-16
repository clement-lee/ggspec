#' Extract the dataset table for a ggplot object
#'
#' Returns one row per unique data frame used in the plot — either at the
#' global level (`ggplot(data, ...)`) or attached to individual layers
#' (`geom_*(data = ...)`). Datasets are deduplicated by their [rlang::hash()].
#'
#' @param p A ggplot object.
#'
#' @return A [tibble::tibble()] with columns:
#'   \describe{
#'     \item{`data_id`}{Integer: sequential identifier starting at 1.}
#'     \item{`label`}{Character: `"hash_<first8chars>"` of the data frame's
#'       hash.  The original symbol or expression is unavailable after
#'       evaluation; the hash is unique and consistent across calls.}
#'   }
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point()
#' spec_data(p)
spec_data <- function(p) {
  assert_ggplot(p)
  tbl <- .spec_data_internal(p)
  tbl[, c("data_id", "label")]
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build dataset table including the hash column for internal lookups
#' @noRd
.spec_data_internal <- function(p) {
  candidates <- list()
  if (!is.null(p$data) && !inherits(p$data, "waiver") && !is.function(p$data))
    candidates <- c(candidates, list(p$data))
  for (lyr in p$layers) {
    d <- lyr$data
    if (!is.null(d) && !inherits(d, "waiver") && !is.function(d))
      candidates <- c(candidates, list(d))
  }

  if (length(candidates) == 0L) {
    return(tibble::tibble(
      data_id = integer(),
      label   = character(),
      hash    = character()
    ))
  }

  hashes     <- vapply(candidates, rlang::hash, character(1L))
  unique_idx <- !duplicated(hashes)
  unique_hashes <- hashes[unique_idx]

  tibble::tibble(
    data_id = seq_along(unique_hashes),
    label   = paste0("hash_", substr(unique_hashes, 1L, 8L)),
    hash    = unique_hashes
  )
}

#' Look up the data_id for a data object using a datasets tibble
#' Returns NA_integer_ if d is NULL, a waiver, a function, or not found.
#' @noRd
.data_id_lookup <- function(d, datasets) {
  if (is.null(d) || inherits(d, "waiver") || is.function(d))
    return(NA_integer_)
  if (nrow(datasets) == 0L) return(NA_integer_)
  h   <- rlang::hash(d)
  idx <- match(h, datasets$hash)
  if (is.na(idx)) NA_integer_ else datasets$data_id[idx]
}
