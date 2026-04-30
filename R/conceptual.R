# ---------------------------------------------------------------------------
# Conceptual similarity pathway
#
# Conceptual similarity checks whether two plots communicate the same
# information using potentially different visual encodings.  Unlike
# structural or visual equivalence, conceptual similarity is not an
# equivalence relation in the strict mathematical sense; it is a
# domain-specific claim that must be qualified with a WHEN condition.
#
# Each detector implements one bespoke rule:
#
#   .detect_distribution_1d        density / histogram / dotplot /
#                                  freqpoly / area(stat="bin")
#   .detect_distribution_1d_1cat   boxplot / violin / jitter
#   .detect_count_2d               geom_count vs geom_point(aes(size=n))
#
# compare_conceptual() runs all detectors in turn and passes if any one
# fires.  If none fires it falls back to visual comparison.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.geom_name <- function(layer) {
  cls <- class(layer$geom)
  # "GeomPoint" -> "point", "GeomBar" -> "bar", etc.
  tolower(sub("^Geom", "", cls[[1L]]))
}

.stat_name <- function(layer) {
  cls <- class(layer$stat)
  tolower(sub("^Stat", "", cls[[1L]]))
}

.global_aes <- function(p) {
  as.list(p$mapping)
}

.layer_aes <- function(layer, global) {
  utils::modifyList(global, as.list(layer$mapping))
}

.aes_var <- function(aes_list, nm) {
  q <- aes_list[[nm]]
  if (is.null(q)) return(NULL)
  rlang::as_label(q)
}

# ---------------------------------------------------------------------------
# Detector: 1-D distribution family
#
# density | histogram | dotplot | freqpoly | area(stat="bin")
# WHEN: both plots map the same variable to x (or y), no explicit y mapping
#
# Identification is by stat, not geom, because:
#   geom_histogram() → geom="bar",  stat="bin"
#   geom_freqpoly()  → geom="path", stat="bin"
#   geom_area(stat="bin") → geom="area", stat="bin"
#   geom_density()   → geom="density", stat="density"
#   geom_dotplot()   → geom="dotplot", stat="bindot"
# ---------------------------------------------------------------------------

.detect_distribution_1d <- function(p1, p2) {
  FAMILY_GEOMS <- c("density", "dotplot")
  FAMILY_STATS <- c("density", "bin", "bindot")

  .is_1d_dist <- function(p) {
    if (length(p$layers) != 1L) return(FALSE)
    lyr   <- p$layers[[1L]]
    gname <- .geom_name(lyr)
    sname <- .stat_name(lyr)
    is_family <- gname %in% FAMILY_GEOMS || sname %in% FAMILY_STATS
    if (!is_family) return(FALSE)
    aes <- .layer_aes(lyr, .global_aes(p))
    xv  <- .aes_var(aes, "x")
    yv  <- .aes_var(aes, "y")
    !is.null(xv) && is.null(yv)
  }

  if (!.is_1d_dist(p1) || !.is_1d_dist(p2)) {
    return(new_ggspec_result(NA_real_,
      "Not both in 1-D distribution family.", check = "conceptual_1d_dist"))
  }

  aes1 <- .layer_aes(p1$layers[[1L]], .global_aes(p1))
  aes2 <- .layer_aes(p2$layers[[1L]], .global_aes(p2))
  x1   <- .aes_var(aes1, "x")
  x2   <- .aes_var(aes2, "x")

  if (!identical(x1, x2)) {
    return(new_ggspec_result(FALSE,
      sprintf("Same family but different x variables: '%s' vs '%s'.", x1, x2),
      check = "conceptual_1d_dist"))
  }

  s1 <- .stat_name(p1$layers[[1L]])
  s2 <- .stat_name(p2$layers[[1L]])
  new_ggspec_result(TRUE,
    sprintf(
      "Conceptually similar: both show 1-D distribution of '%s' (stat: %s vs %s). ",
      x1, s1, s2),
    check = "conceptual_1d_dist")
}

# ---------------------------------------------------------------------------
# Detector: 1-D distribution by 1 categorical variable
#
# boxplot | violin | jitter (geom_jitter or geom_point with position="jitter")
# WHEN: both plots map the same continuous variable to y, same discrete to x
# ---------------------------------------------------------------------------

.detect_distribution_1d_1cat <- function(p1, p2) {
  FAMILY <- c("boxplot", "violin", "jitter", "point")

  .is_1cat_dist <- function(p) {
    if (length(p$layers) != 1L) return(FALSE)
    lyr   <- p$layers[[1L]]
    gname <- .geom_name(lyr)
    if (!(gname %in% FAMILY)) return(FALSE)
    if (gname == "point") {
      pos <- class(lyr$position)[[1L]]
      if (!grepl("Jitter", pos, fixed = FALSE)) return(FALSE)
    }
    aes <- .layer_aes(lyr, .global_aes(p))
    !is.null(.aes_var(aes, "x")) && !is.null(.aes_var(aes, "y"))
  }

  if (!.is_1cat_dist(p1) || !.is_1cat_dist(p2)) {
    return(new_ggspec_result(NA_real_,
      "Not both in 1-cat distribution family.", check = "conceptual_1d_1cat"))
  }

  aes1 <- .layer_aes(p1$layers[[1L]], .global_aes(p1))
  aes2 <- .layer_aes(p2$layers[[1L]], .global_aes(p2))
  x1   <- .aes_var(aes1, "x"); x2 <- .aes_var(aes2, "x")
  y1   <- .aes_var(aes1, "y"); y2 <- .aes_var(aes2, "y")

  if (!identical(x1, x2) || !identical(y1, y2)) {
    return(new_ggspec_result(FALSE,
      sprintf("Same family but different variables: x '%s'/'%s', y '%s'/'%s'.",
              x1, x2, y1, y2),
      check = "conceptual_1d_1cat"))
  }

  g1 <- .geom_name(p1$layers[[1L]])
  g2 <- .geom_name(p2$layers[[1L]])
  new_ggspec_result(TRUE,
    sprintf(
      "Conceptually similar: both show distribution of '%s' by '%s' (%s vs %s). ",
      y1, x1, g1, g2),
    check = "conceptual_1d_1cat")
}

# ---------------------------------------------------------------------------
# Detector: 2-D discrete counts
#
# geom_count (stat="sum") vs geom_point with explicit size aesthetic
# WHEN: both plots map the same x and y variables
# ---------------------------------------------------------------------------

.detect_count_2d <- function(p1, p2) {
  .is_count_2d <- function(p) {
    if (length(p$layers) != 1L) return(FALSE)
    lyr   <- p$layers[[1L]]
    gname <- .geom_name(lyr)
    sname <- .stat_name(lyr)
    is_count  <- gname == "count"
    is_sized  <- gname == "point" && !is.null(
                   (.layer_aes(lyr, .global_aes(p)))[["size"]])
    is_count || is_sized
  }

  if (!.is_count_2d(p1) || !.is_count_2d(p2)) {
    return(new_ggspec_result(NA_real_,
      "Not both in 2-D count family.", check = "conceptual_count_2d"))
  }

  aes1 <- .layer_aes(p1$layers[[1L]], .global_aes(p1))
  aes2 <- .layer_aes(p2$layers[[1L]], .global_aes(p2))
  x1   <- .aes_var(aes1, "x"); x2 <- .aes_var(aes2, "x")
  y1   <- .aes_var(aes1, "y"); y2 <- .aes_var(aes2, "y")

  if (!identical(x1, x2) || !identical(y1, y2)) {
    return(new_ggspec_result(FALSE,
      sprintf("Same family but different variables: x '%s'/'%s', y '%s'/'%s'.",
              x1, x2, y1, y2),
      check = "conceptual_count_2d"))
  }

  g1 <- .geom_name(p1$layers[[1L]])
  g2 <- .geom_name(p2$layers[[1L]])
  new_ggspec_result(TRUE,
    sprintf(
      "Conceptually similar: both show 2-D counts of '%s' x '%s' (%s vs %s). ",
      x1, y1, g1, g2),
    check = "conceptual_count_2d")
}

# ---------------------------------------------------------------------------
# compare_conceptual
# ---------------------------------------------------------------------------

#' Compare two ggplot objects for conceptual similarity
#'
#' `compare_conceptual()` checks whether two plots communicate the same
#' information using potentially different visual encodings.  It runs a
#' sequence of bespoke detectors; if any detector fires, the plots are
#' declared conceptually similar.  If none fires, it falls back to
#' [compare_visual()].
#'
#' Conceptual similarity is always qualified by a WHEN condition, which is
#' described in the `$message` field of the returned object.
#'
#' Called internally by [compare_plots()] when `mode = "conceptual"`.
#'
#' @param p1 The reference ggplot object.
#' @param p2 The observed ggplot object.
#'
#' @return A `ggspec_compare` object.
#' @export
#' @examples
#' library(ggplot2)
#' p_box  <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()
#' p_viol <- ggplot(mpg, aes(x = class, y = hwy)) + geom_violin()
#' compare_conceptual(p_box, p_viol)   # TRUE
compare_conceptual <- function(p1, p2) {
  assert_ggplot(p1, "p1")
  assert_ggplot(p2, "p2")

  detectors <- list(
    .detect_distribution_1d,
    .detect_distribution_1d_1cat,
    .detect_count_2d
  )

  for (det in detectors) {
    r <- det(p1, p2)
    if (isTRUE(r$pass)) {
      r$mode  <- "conceptual"
      class(r) <- c("ggspec_compare", class(r))
      return(r)
    }
  }

  result       <- compare_visual(p1, p2)
  result$mode  <- "conceptual"
  result
}
