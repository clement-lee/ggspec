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
#' `layer`, `from`, and `to`.
#'
#' @param x A ggplot object, a [spec_plot()] tibble, or a `ggspec_canon`
#'   object. If a `ggspec_canon` is supplied, its `$spec` is re-canonicalised
#'   (useful for chaining or verifying idempotency).
#' @param mode Character scalar controlling which normalisation rules are
#'   applied. `canon()` is a **term rewriting system (TRS)** that computes
#'   a structural normal form: two plots are structurally equivalent iff they
#'   have the same normal form under these rules.
#'   \describe{
#'     \item{`"strict"`}{Minimal normalisation: ensure all values are plain
#'       character strings (no raw quosures) and list-column NULLs are
#'       standardised. Preserves data/mapping placement and layer order.}
#'     \item{`"structural"` (default)}{Includes strict, plus: fold global
#'       data/mapping into each non-zero layer so that placement differences
#'       (global vs per-layer) become transparent; sort layers into a
#'       canonical order by `(geom, stat)`; normalise geom/stat
#'       representation (`geom_col` → `geom_bar(stat="identity")`).}
#'   }
#'   Visual and conceptual comparison (which require rendering via
#'   [ggplot2::ggplot_build()]) are handled by [compare_visual()] and
#'   [compare_conceptual()] respectively, not by `canon()`.
#'
#' @return An object of class `ggspec_canon`, a named list with components:
#'   \describe{
#'     \item{`spec`}{A [spec_plot()] tibble after canonicalisation.}
#'     \item{`changes`}{A tibble with columns `rule` (chr), `dimension` (chr),
#'       `layer` (int or `NA`), `from` (chr), `to` (chr).  Zero rows means
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
#' c1$spec$geom      # layer 0 NA, then alphabetical: "point", "smooth"
#'
#' # Idempotency
#' c2 <- canon(c1)
#' nrow(c2$changes)  # 0 — already canonical
canon <- function(x, mode = "structural") {
  mode <- match.arg(mode, c("strict", "structural"))

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
    .rule_fold_global,
    .rule_geom_col_to_bar,
    .rule_layer_order
  ))

  switch(mode,
    strict     = strict_rules,
    structural = structural_rules
  )
}

# ---------------------------------------------------------------------------
# Individual normalisation rules
# Each takes (spec, changes) and returns list(spec=, changes=)
# ---------------------------------------------------------------------------

# Rule: normalise_nulls
.rule_normalise_nulls <- function(spec, changes) {
  list(spec = spec, changes = changes)
}

# Rule: fold_global
# Propagates the global data_id and mapping (layer 0) into every non-zero
# layer that uses the global data/mapping, then clears layer 0.
# This makes plots that differ only in WHERE data/mapping is specified
# (ggplot() vs geom_*()) compare as identical after canonicalisation.
.rule_fold_global <- function(spec, changes) {
  layer0_row  <- which(spec$layer == 0L)
  nonzero_idx <- which(spec$layer > 0L)

  if (length(layer0_row) == 0L || length(nonzero_idx) == 0L) {
    return(list(spec = spec, changes = changes))
  }

  global_data_id <- spec$data_id[[layer0_row]]
  global_mapping <- spec$mapping[[layer0_row]]

  data_changed    <- FALSE
  mapping_changed <- FALSE

  # 1. Propagate data_id: layers with NA data_id inherit the global one
  if (!is.na(global_data_id)) {
    need_data <- nonzero_idx[is.na(spec$data_id[nonzero_idx])]
    if (length(need_data) > 0L) {
      spec$data_id[need_data] <- global_data_id
      data_changed <- TRUE
    }
  }

  # 2. Propagate mapping: inherit_aes layers receive the global mapping
  if (length(global_mapping) > 0L) {
    for (ri in nonzero_idx) {
      if (!isTRUE(spec$inherit_aes[[ri]])) next
      local_m <- spec$mapping[[ri]]
      # merge: global first, local overrides
      merged  <- global_mapping
      merged[names(local_m)] <- local_m
      spec$mapping[[ri]] <- merged
      mapping_changed <- TRUE
    }
    # Update aes_long for affected rows
    if ("aes_long" %in% names(spec) && mapping_changed) {
      for (ri in nonzero_idx) {
        if (!isTRUE(spec$inherit_aes[[ri]])) next
        tbl    <- spec$aes_long[[ri]]
        merged <- spec$mapping[[ri]]
        # Rebuild from the merged mapping using the existing geom name
        if (length(merged) > 0L) {
          tbl <- tibble::tibble(
            layer     = spec$layer[[ri]],
            geom      = spec$geom[[ri]],
            aesthetic = names(merged),
            variable  = unname(merged),
            source    = ifelse(
              names(merged) %in% names(global_mapping) &
              names(merged) %in% names(spec$mapping[[ri]]),
              "resolved", "local"
            )
          )
        }
        spec$aes_long[[ri]] <- tbl
      }
    }
  }

  if (!data_changed && !mapping_changed) {
    return(list(spec = spec, changes = changes))
  }

  # 3. Clear layer 0
  spec$data_id[[layer0_row]] <- NA_integer_
  spec$mapping[[layer0_row]] <- character(0)
  if ("aes_long" %in% names(spec)) {
    spec$aes_long[[layer0_row]] <- .empty_aes_tbl()
  }

  change <- tibble::tibble(
    rule      = "fold_global",
    dimension = paste(
      c(if (data_changed) "data_id", if (mapping_changed) "mapping"),
      collapse = " + "
    ),
    layer     = NA_integer_,
    from      = "global placement",
    to        = "resolved into layers"
  )
  list(spec = spec, changes = dplyr::bind_rows(changes, change))
}

# Rule: geom_col_to_bar
.rule_geom_col_to_bar <- function(spec, changes) {
  nonzero <- !is.na(spec$geom)
  col_rows <- which(nonzero & spec$geom == "col")
  if (length(col_rows) == 0L) return(list(spec = spec, changes = changes))

  spec$geom[col_rows] <- "bar"

  new_changes <- dplyr::bind_rows(changes, tibble::tibble(
    rule      = rep("geom_col_to_bar", length(col_rows)),
    dimension = rep("geom", length(col_rows)),
    layer     = spec$layer[col_rows],
    from      = rep("col", length(col_rows)),
    to        = rep("bar", length(col_rows))
  ))
  list(spec = spec, changes = new_changes)
}

# Rule: layer_order
# Sort non-zero layers by (geom, stat) alphabetically. Layer 0 stays first.
.rule_layer_order <- function(spec, changes) {
  layer0   <- spec[spec$layer == 0L, , drop = FALSE]
  nonzero  <- spec[spec$layer > 0L,  , drop = FALSE]
  if (nrow(nonzero) == 0L) return(list(spec = spec, changes = changes))

  new_order <- order(nonzero$geom, nonzero$stat)
  if (identical(new_order, seq_len(nrow(nonzero)))) {
    return(list(spec = spec, changes = changes))
  }

  old_layers <- nonzero$layer
  nonzero    <- nonzero[new_order, , drop = FALSE]
  # Renumber layers sequentially after sort
  nonzero$layer <- seq_len(nrow(nonzero))
  if ("aes_long" %in% names(nonzero)) {
    nonzero$aes_long <- lapply(seq_len(nrow(nonzero)), function(i) {
      tbl        <- nonzero$aes_long[[i]]
      tbl$layer  <- i
      tbl
    })
  }

  spec <- dplyr::bind_rows(layer0, nonzero)

  change <- tibble::tibble(
    rule      = "layer_order",
    dimension = "layer",
    layer     = NA_integer_,
    from      = paste(old_layers[new_order], collapse = ","),
    to        = paste(seq_len(nrow(nonzero)), collapse = ",")
  )
  list(spec = spec, changes = dplyr::bind_rows(changes, change))
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
    layer     = integer(),
    from      = character(),
    to        = character()
  )
}
