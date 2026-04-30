#' Enrich a plot's layer specification with build-derived default information
#'
#' Calls [ggplot2::ggplot_build()] and compares the result against the declared
#' spec to classify each parameter and aesthetic as **explicit** (set by the
#' user) or **default** (filled in by ggplot2).
#'
#' The three-way classification follows directly from the spec/build comparison:
#' * **explicit** — quantity appears in both spec and build (user-specified).
#' * **default** — quantity is in build but absent from spec, or its value
#'   matches the ggplot2 default (ggplot2 filled it in).
#' * **transformed** — quantity is in spec but its build value differs
#'   (e.g. a colour name resolved to a hex code by a scale); detectable by
#'   comparing `built_aes$value` with the original aesthetic variable.
#'
#' **How explicit detection works**:
#' ggplot2 stores all parameters — both user-supplied and defaulted — in
#' `layer$geom_params` / `layer$stat_params`.  To distinguish them, `enrich_spec()`
#' constructs a reference layer (via `geom_*()`  with no arguments) and compares
#' each stored value against the reference default.  A value that differs from
#' its default is classed as explicit; one that matches is classed as default.
#' The one edge case this cannot detect is a user explicitly setting a param to
#' its own default value (e.g. `geom_smooth(se = TRUE)` when `TRUE` is the
#' default) — such cases are conservatively reported as default.
#' Aesthetic constants set via fixed values (e.g. `geom_point(colour = "red")`)
#' are always reported as explicit regardless of value.
#'
#' @param p A ggplot object.
#' @return A [tibble::tibble()] extending [spec_layers()] with two additional
#'   list-columns:
#'   \describe{
#'     \item{`params_tbl`}{One row per non-aesthetic parameter stored in the
#'       layer.  Columns: `param` (chr), `value` (list), `explicit` (lgl),
#'       `source` (chr — `"geom"`, `"stat"`, or `"aes"`).
#'       `explicit = TRUE` means the value differs from the ggplot2 default (or
#'       is a fixed aesthetic constant); `explicit = FALSE` means ggplot2 would
#'       have used the same value without any user input.}
#'     \item{`built_aes`}{One row per aesthetic resolved during build.  Columns:
#'       `aesthetic` (chr), `value` (list), `explicit` (lgl).
#'       `explicit = TRUE` means the aesthetic was mapped or set as a constant
#'       by the user; `explicit = FALSE` means ggplot2 applied a default
#'       (e.g. `colour = "black"` for `geom_point()` when no colour mapping is
#'       present).}
#'   }
#' @seealso [spec_layers()], [spec_aes()]
#' @export
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = class))
#' es <- enrich_spec(p)
#' # Which params are explicit vs default for layer 1?
#' es$params_tbl[[1]]
#' # Which aesthetics are explicit vs default for layer 1?
#' es$built_aes[[1]]
enrich_spec <- function(p) {
  assert_ggplot(p)
  layers_tbl   <- spec_layers(p)
  nonzero_rows <- which(layers_tbl$layer > 0L)

  # Initialise list-columns (layer 0 gets empty tables)
  empty_params <- tibble::tibble(param = character(), value = list(),
                                 explicit = logical(), source = character())
  empty_aes    <- tibble::tibble(aesthetic = character(), value = list(),
                                 explicit  = logical())
  layers_tbl$params_tbl <- rep(list(empty_params), nrow(layers_tbl))
  layers_tbl$built_aes  <- rep(list(empty_aes),    nrow(layers_tbl))

  if (length(nonzero_rows) == 0L) return(layers_tbl)

  b <- .build(p)

  # Resolved aesthetic names per non-zero layer (global + local merged; names only)
  resolved_names <- lapply(p$layers, function(l) {
    names(.resolve_aes(l, p$mapping, inherit = "resolve"))
  })

  for (i in seq_along(nonzero_rows)) {
    ri <- nonzero_rows[i]
    layers_tbl$params_tbl[[ri]] <- .enrich_params(p$layers[[i]])
    layers_tbl$built_aes[[ri]]  <- .extract_built_aes(
      p$layers[[i]], b$data[[i]], resolved_names[[i]]
    )
  }

  layers_tbl
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Tidy params tibble with explicit flag for one layer
#'
#' ggplot2 stores ALL params (user-supplied and defaulted) in
#' `layer$geom_params` / `layer$stat_params`.  To detect which were explicitly
#' set, we compare against a reference layer constructed with no arguments.
#' A value that differs from the reference default is explicit.
#' @noRd
.enrich_params <- function(layer) {
  stored_geom <- layer$geom_params %||% list()
  stored_stat <- layer$stat_params %||% list()
  stored_aes  <- layer$aes_params  %||% list()

  # Reference defaults for this geom+stat combination
  geom_name <- .geom_name(layer)
  stat_name <- .stat_name(layer)
  ref <- .reference_layer_params(geom_name, stat_name)

  # All param names present in the layer
  all_names <- unique(c(names(stored_geom), names(stored_stat), names(stored_aes)))

  if (length(all_names) == 0L) {
    return(tibble::tibble(
      param    = character(),
      value    = list(),
      explicit = logical(),
      source   = character()
    ))
  }

  # Unified value lookup (aes_params > geom_params > stat_params)
  .val <- function(nm) {
    if (nm %in% names(stored_aes))  return(stored_aes[[nm]])
    if (nm %in% names(stored_geom)) return(stored_geom[[nm]])
    stored_stat[[nm]]
  }

  .source <- function(nm) {
    if (nm %in% names(stored_aes))  return("aes")
    if (nm %in% names(stored_geom)) return("geom")
    if (nm %in% names(stored_stat)) return("stat")
    "unknown"
  }

  .is_explicit <- function(nm) {
    # Fixed aesthetic constants (colour = "red" etc.) are always explicit
    if (nm %in% names(stored_aes)) return(TRUE)
    # Compare stored value with the reference default
    val <- .val(nm)
    if (!nm %in% names(ref)) return(TRUE)   # unknown param → assume explicit
    !identical(val, ref[[nm]])
  }

  tibble::tibble(
    param    = all_names,
    value    = lapply(all_names, .val),
    explicit = vapply(all_names, .is_explicit, logical(1L)),
    source   = vapply(all_names, .source, character(1L))
  )
}

#' Build a reference set of default params for a geom+stat pair
#'
#' Constructs a throwaway layer via `geom_*()` (no user arguments) to read off
#' the default values for all params.  This is compared against the actual
#' layer to detect explicitly-set values.
#' @noRd
.reference_layer_params <- function(geom_name, stat_name) {
  # Map (geom_name, stat_name) to the canonical user-facing function
  fn_name <- if (geom_name == "bar" && stat_name == "bin") {
    "geom_histogram"
  } else if (geom_name == "col" ||
             (geom_name == "bar" && stat_name == "identity")) {
    "geom_col"
  } else {
    paste0("geom_", geom_name)
  }

  fn <- tryCatch(
    utils::getFromNamespace(fn_name, "ggplot2"),
    error = function(e) NULL
  )
  if (is.null(fn)) return(list())

  ref_layer <- tryCatch(fn(), error = function(e) NULL)
  if (is.null(ref_layer)) return(list())

  ref <- c(
    ref_layer$geom_params %||% list(),
    ref_layer$stat_params %||% list(),
    ref_layer$aes_params  %||% list()
  )
  ref[!duplicated(names(ref))]
}

#' Extract built aesthetic values for one layer with explicit flag
#'
#' Columns in the built data frame that correspond to valid geom aesthetics are
#' matched against what the user explicitly mapped or set as constants.
#' [ggplot2::ggplot_build()] is the source of truth for the resolved values.
#' @noRd
.extract_built_aes <- function(layer, built_data, resolved_aes_names = NULL) {
  # Aesthetic names valid for this geom
  valid_aes <- tryCatch(
    layer$geom$aesthetics(),
    error = function(e) {
      c(
        unlist(strsplit(layer$geom$required_aes %||% character(0L),
                        "|", fixed = TRUE)),
        names(layer$geom$default_aes %||% list()),
        layer$geom$optional_aes %||% character(0L)
      )
    }
  )

  # Restrict to columns present in the built data
  built_aes_cols <- intersect(names(built_data), valid_aes)

  if (length(built_aes_cols) == 0L) {
    return(tibble::tibble(
      aesthetic = character(),
      value     = list(),
      explicit  = logical()
    ))
  }

  # Aesthetics set explicitly by the user: mapped (global or local) or aes constants
  user_aes <- unique(c(
    resolved_aes_names %||% character(0L),
    names(layer$aes_params %||% list())
  ))

  tibble::tibble(
    aesthetic = built_aes_cols,
    value     = lapply(built_aes_cols, function(nm) {
      vals <- built_data[[nm]]
      uniq <- unique(vals)
      if (length(uniq) == 1L) uniq else vals
    }),
    explicit  = built_aes_cols %in% user_aes
  )
}
