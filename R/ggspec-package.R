#' ggspec: Extract and Compare ggplot2 Plot Specifications as Tidy Data Frames
#'
#' @description
#' ggspec provides a two-tier API for working with ggplot2 objects
#' programmatically:
#'
#' **Extraction tier (`spec_*`)**: functions that take a ggplot object and
#' return tidy data frames describing its full specification — layers, aesthetic
#' mappings, scales, facets, coordinate system, and labels.
#'
#' **Comparison tier (`equiv_*`, `check_*`)**: functions that compare two
#' ggplot objects structurally, returning informative results suitable for
#' automated testing, plot auditing, or grading workflows.
#'
#' @section Extraction functions:
#' - [spec_layers()] — one row per layer (geom, stat, position, mappings, params)
#' - [spec_aes()] — one row per layer × aesthetic (long format)
#' - [spec_scales()] — one row per scale
#' - [spec_facets()] — facet type and variables
#' - [spec_labels()] — one row per label
#' - [spec_coord()] — coordinate system properties
#' - [spec_data()] — one row per unique dataset (data_id, label)
#' - [spec_plot()] — master summary joining all of the above
#' - [enrich_spec()] — spec_layers plus explicit/default flags from ggplot_build()
#'
#' @section Comparison functions:
#' - [equiv_layers()] — compare layer structure
#' - [equiv_aes()] — compare aesthetic mappings
#' - [equiv_scales()] — compare scales
#' - [equiv_facets()] — compare facet specification
#' - [equiv_labels()] — compare labels
#' - [equiv_coord()] — compare coordinate system
#' - [equiv_params()] — compare layer parameters
#' - [equiv_data()] — compare layer data
#' - [equiv_plot()] — run all checks in one call
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
