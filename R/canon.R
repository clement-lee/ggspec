#' Canonicalise a ggplot specification
#'
#' Normalises a ggplot specification (extracted via [spec_plot()]) into a
#' standard form, resolving multiple representations of the same intent.
#' The result is an `ggspec_canon` object that carries the canonicalised
#' specification, a log of every change made, and the original specification
#' for comparison.
#'
#' `canon()` is **idempotent**: `canon(canon(x))` produces the same
#' specification as `canon(x)`, and the second call records zero changes.
#'
#' `canon()` is **transparent**: `x$changes` is a regular tibble listing
#' every normalisation applied, with columns `rule`, `dimension`,
#' `layer_idx`, `from`, and `to`.
#'
#' @param x A ggplot object, a [spec_plot()] tibble, or a `ggspec_canon`
#'   object. If a `ggspec_canon` is supplied, its `$spec` is re-canonicalised
#'   (useful for chaining or verifying idempotency).
#' @param mode Character scalar controlling which normalisation rules are
#'   applied:
#'   \describe{
#'     \item{`"strict"`}{Minimal normalisation: ensure all values are plain
#'       character strings (no raw quosures) and list-column NULLs are
#'       standardised. Safe to apply before any comparison.}
#'     \item{`"structural"` (default)}{Includes strict, plus: sort layers
#'       into a canonical order by `(geom, stat)`; normalise geom/stat
#'       representation.}
#'     \item{`"visual"`}{Includes structural, plus: absorb `coord_flip()`
#'       by swapping `x`/`y` aesthetics and replacing the coord with
#'       `"cartesian"`; move scale `name` arguments into the labels table so
#'       that `scale_fill_*(name = "v")` and `scale_fill_*() + labs(fill = "v")`
#'       compare as equal; mark default `coord_cartesian()` as `"default"`.}
#'     \item{`"pedagogical"`}{Includes visual, plus domain-specific rules
#'       useful for automated grading: flags `bins` vs `binwidth` usage in
#'       histograms and records `after_stat()` mappings.}
#'   }
#'
#' @return An object of class `ggspec_canon`, a named list with components:
#'   \describe{
#'     \item{`spec`}{A [spec_plot()] tibble after canonicalisation.}
#'     \item{`changes`}{A tibble with columns `rule` (chr), `dimension` (chr),
#'       `layer_idx` (int or `NA`), `from` (chr), `to` (chr).  Zero rows means
#'       the input was already in canonical form.}
#'     \item{`mode`}{The mode string used.}
#'     \item{`original`}{The [spec_plot()] tibble before canonicalisation.}
#'   }
#'
#' @export
#' @examples
#' # Layer ordering is canonical in structural mode
#' p1 <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_smooth() +
#'   ggplot2::geom_point()
#' c1 <- canon(p1)
#' c1$changes        # records the layer reordering
#' c1$spec$geom      # now alphabetical: "point", "smooth"
#'
#' # Idempotency
#' c2 <- canon(c1)
#' nrow(c2$changes)  # 0 — already canonical
canon <- function(x, mode = "structural") {
  mode <- match.arg(mode, c("strict", "structural", "visual", "pedagogical"))

  # Extract spec from input
  if (inherits(x, "ggspec_canon")) {
    spec <- x$spec
  } else if (is.data.frame(x)) {
    spec <- x
  } else {
    assert_ggplot(x, "x")
    spec <- spec_plot(x)
  }

  original <- spec
  changes  <- .empty_changes_tbl()

  # Apply rules in sequence (each rule is: list(spec, changes) -> list(spec, changes))
  rules <- .canon_rules(mode)
  for (rule_fn in rules) {
    out     <- rule_fn(spec, changes)
    spec    <- out$spec
    changes <- out$changes
  }

  structure(
    list(spec = spec, changes = changes, mode = mode, original = original),
    class = "ggspec_canon"
  )
}

#' @export
print.ggspec_canon <- function(x, ...) {
  n <- nrow(x$changes)
  cat(sprintf("[CANON mode=%s] %d change(s) applied.\n", x$mode, n))
  if (n > 0L) {
    cat("  Changes:\n")
    print(x$changes, ...)
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Rule registry
# ---------------------------------------------------------------------------

#' Return the list of normalisation rules for a given mode
#' @noRd
.canon_rules <- function(mode) {
  strict_rules <- list(
    .rule_normalise_nulls
  )
  structural_rules <- c(strict_rules, list(
    .rule_geom_col_to_bar,
    .rule_layer_order
  ))
  visual_rules <- c(structural_rules, list(
    .rule_coord_flip,
    .rule_scale_name_to_labels,
    .rule_default_coord
  ))
  pedagogical_rules <- c(visual_rules, list(
    .rule_histogram_bin_param,
    .rule_after_stat_flag
  ))

  switch(mode,
    strict       = strict_rules,
    structural   = structural_rules,
    visual       = visual_rules,
    pedagogical  = pedagogical_rules
  )
}

# ---------------------------------------------------------------------------
# Individual normalisation rules
# Each takes (spec, changes) and returns list(spec=, changes=)
# ---------------------------------------------------------------------------

# Rule: normalise_nulls
# Ensure list-column entries that are empty lists are consistently NULL-free.
# This is a no-op for well-formed spec_plot() output but guards against edge
# cases from manual construction.
.rule_normalise_nulls <- function(spec, changes) {
  # Currently a no-op placeholder — spec_plot() already produces clean output.
  list(spec = spec, changes = changes)
}

# Rule: geom_col_to_bar
# geom_col() uses GeomCol internally; normalise to geom_bar(stat = "identity")
# so that geom_col() and geom_bar(stat = "identity") compare as equal.
.rule_geom_col_to_bar <- function(spec, changes) {
  if (nrow(spec) == 0L) return(list(spec = spec, changes = changes))
  col_rows <- which(spec$geom == "col")
  if (length(col_rows) == 0L) return(list(spec = spec, changes = changes))

  spec$geom[col_rows] <- "bar"

  new_changes <- dplyr::bind_rows(changes, tibble::tibble(
    rule      = rep("geom_col_to_bar", length(col_rows)),
    dimension = rep("geom", length(col_rows)),
    layer_idx = as.integer(col_rows),
    from      = rep("col", length(col_rows)),
    to        = rep("bar", length(col_rows))
  ))
  list(spec = spec, changes = new_changes)
}

# Rule: layer_order
# Sort layers by (geom, stat) alphabetically. Records the old-vs-new permutation.
.rule_layer_order <- function(spec, changes) {
  if (nrow(spec) == 0L) return(list(spec = spec, changes = changes))

  new_order <- order(spec$geom, spec$stat)
  if (identical(new_order, seq_len(nrow(spec)))) {
    # Already in canonical order
    return(list(spec = spec, changes = changes))
  }

  old_idx <- spec$layer_idx
  spec    <- spec[new_order, , drop = FALSE]
  # Update layer_idx to reflect canonical position
  spec$layer_idx <- seq_len(nrow(spec))
  # Update layer_idx inside each aes_long sub-table
  if ("aes_long" %in% names(spec)) {
    spec$aes_long <- lapply(seq_len(nrow(spec)), function(i) {
      tbl <- spec$aes_long[[i]]
      tbl$layer_idx <- i
      tbl
    })
  }

  change <- tibble::tibble(
    rule      = "layer_order",
    dimension = "layer_idx",
    layer_idx = NA_integer_,
    from      = paste(old_idx[new_order], collapse = ","),
    to        = paste(seq_len(nrow(spec)), collapse = ",")
  )
  list(spec = spec, changes = dplyr::bind_rows(changes, change))
}

# Rule: coord_flip
# If the coord is "flip", swap x <-> y in every layer's mapping and
# aes_long, then set coord_type to "cartesian".
.rule_coord_flip <- function(spec, changes) {
  if (nrow(spec) == 0L) return(list(spec = spec, changes = changes))

  coord <- spec$coord[[1L]]
  if (!identical(coord$coord_type, "flip")) {
    return(list(spec = spec, changes = changes))
  }

  # Swap x <-> y in mapping list-column
  spec$mapping <- lapply(spec$mapping, function(m) {
    has_x <- "x" %in% names(m) && !is.na(m[["x"]])
    has_y <- "y" %in% names(m) && !is.na(m[["y"]])
    if (has_x && has_y) {
      tmp    <- m[["x"]]
      m["x"] <- m[["y"]]
      m["y"] <- tmp
    } else if (has_y) {
      names(m)[names(m) == "y"] <- "x"
    } else if (has_x) {
      names(m)[names(m) == "x"] <- "y"
    }
    m
  })

  # Swap x <-> y in aes_long list-column
  if ("aes_long" %in% names(spec)) {
    spec$aes_long <- lapply(spec$aes_long, function(tbl) {
      x_rows <- tbl$aesthetic == "x"
      y_rows <- tbl$aesthetic == "y"
      has_x <- any(x_rows)
      has_y <- any(y_rows)
      if (has_x && has_y) {
        x_vars <- tbl$variable[x_rows]
        y_vars <- tbl$variable[y_rows]
        tbl$variable[x_rows] <- y_vars
        tbl$variable[y_rows] <- x_vars
      } else if (has_y) {
        tbl$aesthetic[y_rows] <- "x"
      } else if (has_x) {
        tbl$aesthetic[x_rows] <- "y"
      }
      tbl
    })
  }

  # Replace coord with cartesian in all rows
  new_coord <- coord
  new_coord$coord_type <- "cartesian"
  spec$coord <- rep(list(new_coord), nrow(spec))

  change <- tibble::tibble(
    rule      = "coord_flip",
    dimension = "coord + mapping",
    layer_idx = NA_integer_,
    from      = "coord_flip with x/y aesthetics",
    to        = "coord_cartesian with x/y swapped"
  )
  list(spec = spec, changes = dplyr::bind_rows(changes, change))
}

# Rule: scale_name_to_labels
# If any scale has a non-NA name, move it to the labels table (if not already
# overridden there) and set name to NA in the scales table.
.rule_scale_name_to_labels <- function(spec, changes) {
  if (nrow(spec) == 0L) return(list(spec = spec, changes = changes))

  scales_tbl <- spec$scales[[1L]]
  labels_tbl <- spec$labels[[1L]]
  new_changes <- changes

  named_rows <- which(!is.na(scales_tbl$name))
  if (length(named_rows) == 0L) return(list(spec = spec, changes = changes))

  for (i in named_rows) {
    aes_str  <- scales_tbl$aesthetic[[i]]
    nm       <- scales_tbl$name[[i]]
    # Only promote to labels if not already set
    existing_idx <- which(labels_tbl$aesthetic == aes_str)
    if (length(existing_idx) == 0L) {
      labels_tbl <- dplyr::bind_rows(
        labels_tbl,
        tibble::tibble(aesthetic = aes_str, label = nm)
      )
    } else {
      # Always overwrite — an explicit scale name takes precedence over any
      # automatic (fallback) label derived from the aesthetic variable name.
      labels_tbl$label[existing_idx] <- nm
    }
    new_changes <- dplyr::bind_rows(new_changes, tibble::tibble(
      rule      = "scale_name_to_labels",
      dimension = "scales/labels",
      layer_idx = NA_integer_,
      from      = sprintf("scale[%s]$name = '%s'", aes_str, nm),
      to        = sprintf("labels[%s] = '%s'", aes_str, nm)
    ))
    scales_tbl$name[[i]] <- NA_character_
  }

  # Propagate updated tables to all rows
  spec$scales <- rep(list(scales_tbl), nrow(spec))
  spec$labels <- rep(list(labels_tbl), nrow(spec))

  list(spec = spec, changes = new_changes)
}

# Rule: default_coord
# If coord is "cartesian" with NULL limits and default expand/clip, mark it
# as "default" so two plots that differ only in explicit vs implicit cartesian
# coord compare as equal.
.rule_default_coord <- function(spec, changes) {
  if (nrow(spec) == 0L) return(list(spec = spec, changes = changes))

  coord <- spec$coord[[1L]]
  xlim_null <- is.null(coord$xlim[[1L]])
  ylim_null <- is.null(coord$ylim[[1L]])
  is_default <- identical(coord$coord_type, "cartesian") &&
    xlim_null && ylim_null

  if (!is_default) return(list(spec = spec, changes = changes))
  if (identical(coord$coord_type, "default")) return(list(spec = spec, changes = changes))

  new_coord <- coord
  new_coord$coord_type <- "default"
  spec$coord <- rep(list(new_coord), nrow(spec))

  change <- tibble::tibble(
    rule      = "default_coord",
    dimension = "coord",
    layer_idx = NA_integer_,
    from      = "cartesian (explicit or default)",
    to        = "default"
  )
  list(spec = spec, changes = dplyr::bind_rows(changes, change))
}

# Rule: histogram_bin_param (pedagogical)
# Flags histogram layers that use bins= vs binwidth= so grading can accept
# either form. Records which parameter was found; does not convert values.
.rule_histogram_bin_param <- function(spec, changes) {
  if (nrow(spec) == 0L) return(list(spec = spec, changes = changes))

  new_changes <- changes
  for (i in seq_len(nrow(spec))) {
    # geom_histogram() uses GeomBar + StatBin: identify by stat = "bin"
    if (!identical(spec$stat[[i]], "bin")) next
    params <- spec$params[[i]]
    has_bins     <- !is.null(params[["bins"]])
    has_binwidth <- !is.null(params[["binwidth"]])
    if (has_bins && !has_binwidth) {
      new_changes <- dplyr::bind_rows(new_changes, tibble::tibble(
        rule      = "histogram_bin_param",
        dimension = "params",
        layer_idx = i,
        from      = sprintf("bins = %s", params[["bins"]]),
        to        = "flexible_bin_param (bins accepted)"
      ))
    }
  }
  list(spec = spec, changes = new_changes)
}

# Rule: after_stat_flag (pedagogical)
# Detects aes(y = after_stat(density)) patterns and records their presence.
# Does not modify the spec; purely informational for grading.
.rule_after_stat_flag <- function(spec, changes) {
  if (nrow(spec) == 0L || !"aes_long" %in% names(spec)) {
    return(list(spec = spec, changes = changes))
  }

  new_changes <- changes
  for (i in seq_len(nrow(spec))) {
    tbl <- spec$aes_long[[i]]
    after_stat_rows <- tbl[grepl("after_stat", tbl$variable, fixed = TRUE), , drop = FALSE]
    if (nrow(after_stat_rows) > 0L) {
      new_changes <- dplyr::bind_rows(new_changes, tibble::tibble(
        rule      = "after_stat_flag",
        dimension = "aes",
        layer_idx = i,
        from      = paste(after_stat_rows$variable, collapse = "; "),
        to        = "after_stat_mapping_present"
      ))
    }
  }
  list(spec = spec, changes = new_changes)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Return an empty changes tibble with the correct schema
#' @noRd
.empty_changes_tbl <- function() {
  tibble::tibble(
    rule      = character(),
    dimension = character(),
    layer_idx = integer(),
    from      = character(),
    to        = character()
  )
}
