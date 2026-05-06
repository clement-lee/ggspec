#' ggspec: Extract and Compare ggplot2 Plot Specifications as Tidy Data Frames
#'
#' @description
#' ggspec provides three tiers of API for working with ggplot2 objects
#' programmatically:
#'
#' **Extraction tier (`spec_*`)**: functions that take a ggplot object and
#' return tidy data frames describing its full specification — layers, aesthetic
#' mappings, scales, facets, coordinate system, and labels.
#'
#' **Comparison tier (`equiv_*`, `compare_*`)**: functions that compare two
#' ggplot objects at four levels of strictness (see below), returning
#' informative `ggspec_result` objects suitable for automated testing, plot
#' auditing, or grading workflows.
#'
#' **Check / assertion tier (`check_*`, `expect_*`)**: wrappers that call
#' `fail_fn` on failure, making comparison results actionable in any grading
#' or testing framework.
#'
#' @section Four comparison modes:
#' `compare_plots()` selects one of four pathways via its `mode` argument:
#'
#' - **strict**: identical specs, same layer order, same random seed.
#'   Implemented via [canon()] with only null-normalisation applied.
#' - **structural**: identical specs after canonicalisation by [canon()].
#'   Rules: fold_global (placement transparency), geom_col_to_bar (shorthand
#'   expansion), layer_order (non-spanning geom sorting).
#' - **visual**: identical rendered output, checked via [ggplot2::ggplot_build()].
#'   Handled by [compare_visual()]; detects equivalences that structural
#'   comparison cannot (pre-counted vs stat-computed, coord_flip vs swapped
#'   aesthetics, scale name vs labs).
#' - **conceptual**: same communicative intent with different visual encodings.
#'   Handled by [compare_conceptual()]; each rule is qualified by a WHEN
#'   condition (e.g. boxplot ~ violin WHEN 1 continuous + 1 discrete variable).
#'
#' @section Extraction functions:
#' - [spec_layers()] — one row per layer (geom, stat, position, mappings, params)
#' - [spec_aes()] — one row per layer-aesthetic pair (long format)
#' - [spec_scales()] — one row per scale
#' - [spec_facets()] — facet type and variables
#' - [spec_labels()] — one row per label
#' - [spec_coord()] — coordinate system properties
#' - [spec_data()] — one row per unique dataset (data_id, label)
#' - [spec_plot()] — master summary joining all of the above
#' - [enrich_spec()] — spec_layers plus explicit/default flags from [ggplot2::ggplot_build()]
#'
#' @section Structural comparison functions:
#' - [canon()] — reduce a ggplot to its canonical spec (strict or structural mode)
#' - [equiv_layers()] — compare layer structure (geom, stat, data_id)
#' - [equiv_aes()] — compare aesthetic mappings
#' - [equiv_scales()] — compare scales
#' - [equiv_facets()] — compare facet specification
#' - [equiv_labels()] — compare labels
#' - [equiv_coord()] — compare coordinate system
#' - [equiv_params()] — compare layer parameters
#' - [equiv_data()] — compare layer data
#' - [equiv_plot()] — run all structural checks in one call
#' - [compare_plots()] — four-mode entry point; dispatches to the appropriate pathway
#'
#' @section Visual comparison functions:
#' - [compare_visual()] — high-level visual equivalence via [ggplot2::ggplot_build()]
#' - [equiv_rendered()] — compare built layer data frame by frame
#'
#' @section Conceptual similarity functions:
#' - [compare_conceptual()] — run all conceptual similarity detectors
#'
#' @section Check / assertion functions:
#' - [check_plot()] — framework-agnostic assertion (swap in any `fail_fn`)
#' - [expect_equiv_plot()] — testthat expectation
#'
#' @section Utility functions:
#' - [is_ggplot()], [assert_ggplot()]
#' - [n_layers()], [has_layer()]
#'
#' @keywords internal
"_PACKAGE"

## quiets R CMD check NOTE about dplyr's .data pronoun
utils::globalVariables(".data")
