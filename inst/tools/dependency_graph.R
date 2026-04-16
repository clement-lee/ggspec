## dependency_graph.R
## Visualise the function call dependency graph for the ggspec package.
##
## Run with:
##   source("inst/tools/dependency_graph.R")
## or from the package root:
##   Rscript inst/tools/dependency_graph.R
##
## Requires: igraph

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Package 'igraph' is required. Install with: install.packages('igraph')")
}
library(igraph)

# ---------------------------------------------------------------------------
# 1. Edge list  (caller -> callee)
# ---------------------------------------------------------------------------

edges <- rbind(
  # --- is_ggplot / assert_ggplot -----------------------------------------
  c("assert_ggplot",          "is_ggplot"),
  c(".assert_gg_input",       "is_ggplot"),

  # --- spec_* extraction tier --------------------------------------------
  c("spec_layers",            "assert_ggplot"),
  c("spec_layers",            ".resolve_aes"),
  c("spec_layers",            ".layer_params"),
  c("spec_layers",            ".geom_name"),
  c("spec_layers",            ".stat_name"),
  c("spec_layers",            ".position_name"),
  c("spec_layers",            ".layer_data_source"),
  c("spec_layers",            ".empty_layers_tbl"),

  c("spec_aes",               "assert_ggplot"),
  c("spec_aes",               ".deparse_aes"),
  c("spec_aes",               ".resolve_aes"),
  c("spec_aes",               ".geom_name"),
  c("spec_aes",               ".empty_aes_tbl"),

  c("spec_scales",            "assert_ggplot"),
  c("spec_scales",            ".scale_type"),
  c("spec_scales",            ".scale_name"),
  c("spec_scales",            ".scale_transform"),
  c("spec_scales",            ".scale_guide"),
  c("spec_scales",            ".empty_scales_tbl"),

  c("spec_facets",            "assert_ggplot"),
  c("spec_facets",            ".facet_vars_str"),
  c("spec_facets",            ".facet_scales"),
  c("spec_facets",            ".facet_space"),
  c("spec_facets",            ".labeller_str"),

  c("spec_labels",            "assert_ggplot"),
  c("spec_labels",            ".empty_labels_tbl"),

  c("spec_coord",             "assert_ggplot"),

  c("spec_plot",              "assert_ggplot"),
  c("spec_plot",              "spec_layers"),
  c("spec_plot",              "spec_aes"),
  c("spec_plot",              "spec_scales"),
  c("spec_plot",              "spec_facets"),
  c("spec_plot",              "spec_coord"),
  c("spec_plot",              "spec_labels"),

  # --- enrich_spec --------------------------------------------------------
  c("enrich_spec",            "assert_ggplot"),
  c("enrich_spec",            "spec_layers"),
  c("enrich_spec",            ".enrich_params"),
  c("enrich_spec",            ".extract_built_aes"),

  c(".enrich_params",         ".layer_params"),
  c(".enrich_params",         ".geom_name"),
  c(".enrich_params",         ".stat_name"),
  c(".enrich_params",         ".reference_layer_params"),

  # --- utility exports ----------------------------------------------------
  c("flat_mappings",          "assert_ggplot"),
  c("flat_mappings",          "spec_aes"),

  c("mapping_exists",         "assert_ggplot"),
  c("mapping_exists",         "flat_mappings"),

  c("n_layers",               "assert_ggplot"),
  c("layer_data_index",       "assert_ggplot"),

  c("has_layer",              "assert_ggplot"),
  c("has_layer",              ".geom_name"),
  c("has_layer",              ".stat_name"),

  # --- internal getter helpers (.get_*_tbl) --------------------------------
  c(".get_layers_tbl",        "assert_ggplot"),
  c(".get_layers_tbl",        "spec_layers"),

  c(".get_aes_tbl",           "assert_ggplot"),
  c(".get_aes_tbl",           "spec_aes"),

  c(".get_scales_tbl",        "assert_ggplot"),
  c(".get_scales_tbl",        "spec_scales"),
  c(".get_scales_tbl",        ".empty_scales_tbl"),

  c(".get_facets_tbl",        "assert_ggplot"),
  c(".get_facets_tbl",        "spec_facets"),

  c(".get_labels_tbl",        "assert_ggplot"),
  c(".get_labels_tbl",        "spec_labels"),
  c(".get_labels_tbl",        ".empty_labels_tbl"),

  c(".get_coord_tbl",         "assert_ggplot"),
  c(".get_coord_tbl",         "spec_coord"),

  # --- equiv_* comparison tier --------------------------------------------
  c("equiv_layers",           ".get_layers_tbl"),
  c("equiv_layers",           "new_ggspec_result"),

  c("equiv_aes",              ".get_aes_tbl"),
  c("equiv_aes",              "new_ggspec_result"),

  c("equiv_scales",           ".get_scales_tbl"),
  c("equiv_scales",           "new_ggspec_result"),

  c("equiv_facets",           ".get_facets_tbl"),
  c("equiv_facets",           "new_ggspec_result"),

  c("equiv_labels",           ".get_labels_tbl"),
  c("equiv_labels",           "new_ggspec_result"),

  c("equiv_coord",            ".get_coord_tbl"),
  c("equiv_coord",            "new_ggspec_result"),

  c("equiv_params",           ".get_layers_tbl"),
  c("equiv_params",           "new_ggspec_result"),

  c("equiv_data",             "assert_ggplot"),
  c("equiv_data",             "new_ggspec_result"),

  c("equiv_plot",             ".assert_gg_input"),
  c("equiv_plot",             "equiv_layers"),
  c("equiv_plot",             "equiv_aes"),
  c("equiv_plot",             "equiv_scales"),
  c("equiv_plot",             "equiv_facets"),
  c("equiv_plot",             "equiv_labels"),
  c("equiv_plot",             "equiv_coord"),
  c("equiv_plot",             "combine_results"),

  c("combine_results",        "new_ggspec_result"),

  # --- canonicalisation tier ----------------------------------------------
  c("canon",                  "assert_ggplot"),
  c("canon",                  "spec_plot"),
  c("canon",                  ".canon_rules"),

  c(".canon_rules",           ".rule_geom_col_to_bar"),
  c(".canon_rules",           ".rule_layer_order"),
  c(".canon_rules",           ".rule_coord_flip"),
  c(".canon_rules",           ".rule_scale_name_to_labels"),
  c(".canon_rules",           ".rule_default_coord"),
  c(".canon_rules",           ".rule_histogram_bin_param"),
  c(".canon_rules",           ".rule_after_stat_flag"),

  # --- comparison with canonicalisation -----------------------------------
  c("compare_plots",          "assert_ggplot"),
  c("compare_plots",          "canon"),
  c("compare_plots",          "equiv_plot"),

  # --- grading / check tier -----------------------------------------------
  c("check_plot",             "assert_ggplot"),
  c("check_plot",             "compare_plots"),
  c("check_plot",             "equiv_plot"),

  c("expect_equiv_plot",      "equiv_plot"),

  # --- internal spec helpers (leaf nodes) ---------------------------------
  c(".resolve_aes",           ".deparse_aes")
)

edges_df <- as.data.frame(edges, stringsAsFactors = FALSE)
colnames(edges_df) <- c("from", "to")

# ---------------------------------------------------------------------------
# 2. Build igraph object
# ---------------------------------------------------------------------------

g <- graph_from_data_frame(edges_df, directed = TRUE)

# ---------------------------------------------------------------------------
# 3. Node categories
# ---------------------------------------------------------------------------

exported_spec <- c(
  "spec_layers", "spec_aes", "spec_scales", "spec_facets",
  "spec_labels", "spec_coord", "spec_plot", "enrich_spec"
)
exported_equiv <- c(
  "equiv_layers", "equiv_aes", "equiv_scales", "equiv_facets",
  "equiv_labels", "equiv_coord", "equiv_params", "equiv_data",
  "equiv_plot"
)
exported_grading <- c(
  "check_plot", "expect_equiv_plot", "compare_plots"
)
exported_utils <- c(
  "flat_mappings", "mapping_exists", "n_layers",
  "layer_data_index", "has_layer", "is_ggplot", "assert_ggplot"
)
infra <- c(
  "canon", "new_ggspec_result", "combine_results",
  ".assert_gg_input", ".canon_rules"
)
getter_helpers <- c(
  ".get_layers_tbl", ".get_aes_tbl", ".get_scales_tbl",
  ".get_facets_tbl", ".get_labels_tbl", ".get_coord_tbl"
)
canon_rules <- c(
  ".rule_geom_col_to_bar", ".rule_layer_order", ".rule_coord_flip",
  ".rule_scale_name_to_labels", ".rule_default_coord",
  ".rule_histogram_bin_param", ".rule_after_stat_flag"
)
# Everything else is an internal spec helper
all_nodes   <- V(g)$name
categorised <- c(exported_spec, exported_equiv, exported_grading,
                 exported_utils, infra, getter_helpers, canon_rules)
spec_helpers <- setdiff(all_nodes, categorised)

category <- rep("spec_helper", length(all_nodes))
names(category) <- all_nodes
category[exported_spec]    <- "spec"
category[exported_equiv]   <- "equiv"
category[exported_grading] <- "grading"
category[exported_utils]   <- "utils"
category[infra]            <- "infra"
category[getter_helpers]   <- "getter"
category[canon_rules]      <- "rule"

V(g)$category <- category[V(g)$name]

# ---------------------------------------------------------------------------
# 4. Visual attributes
# ---------------------------------------------------------------------------

palette <- c(
  spec         = "#4E79A7",   # blue
  equiv        = "#F28E2B",   # orange
  grading      = "#E15759",   # red
  utils        = "#76B7B2",   # teal
  infra        = "#59A14F",   # green
  getter       = "#EDC948",   # yellow
  rule         = "#B07AA1",   # purple
  spec_helper  = "#BAB0AC"    # grey
)

V(g)$color       <- palette[V(g)$category]
V(g)$frame.color <- "white"
V(g)$shape       <- ifelse(startsWith(V(g)$name, "."), "square", "circle")
V(g)$label.cex   <- 0.65
V(g)$label.color <- "black"
V(g)$size        <- 8 + 2 * degree(g, mode = "out")

E(g)$arrow.size  <- 0.4
E(g)$color       <- "#AAAAAA"

# ---------------------------------------------------------------------------
# 5. Layout
# ---------------------------------------------------------------------------

set.seed(42)
layout <- layout_with_sugiyama(g)$layout

# ---------------------------------------------------------------------------
# 6. Plot
# ---------------------------------------------------------------------------

op <- par(mar = c(1, 1, 2, 1))
plot(
  g,
  layout        = layout,
  vertex.label  = V(g)$name,
  main          = "ggspec function dependency graph"
)

legend(
  "bottomleft",
  legend = c(
    "spec_*  (extraction)",
    "equiv_* (comparison)",
    "grading/check",
    "utilities (exported)",
    "infrastructure / canon",
    ".get_*_tbl (getter helpers)",
    ".rule_* (canon rules)",
    "spec helpers (internal)"
  ),
  fill   = unname(palette),
  border = "white",
  bty    = "n",
  cex    = 0.75
)
par(op)

invisible(g)
