# Next steps for ggspec

## 1. Multi-dataset semantic equivalence

### 1.1 Current state

| Capability | Status |
|---|---|
| `spec_layers()$data_source` marks `"global"` vs `"local"` per layer | ✓ done |
| `layer_data_index(p, data)` locates a data frame within a plot | ✓ done |
| `equiv_data(p1, p2, layer = i)` compares a layer's data by hash | ✓ done |
| `equiv_layers()` / `equiv_aes()` ignore `data_source` by design | ✓ done |
| Plots differing only in where data is supplied (global vs per-layer) pass | ✓ done |

### 1.2 What is missing

#### a) Matched layer-data verification

For a two-dataset plot (e.g. a base map layer + a data overlay), there is no
function that confirms the i-th layer of p1 uses a semantically equivalent data
frame to the i-th layer of p2. `equiv_data()` exists but takes a single layer
index and operates within one plot, not across two plots.

**Concrete example** (`examples/ncovr_layers_global_local.R`):

```r
# Ground truth
ggplot() +
  geom_sf(data = usa_map) +
  geom_sf(aes(fill = POL60), data = ncovr)

# Variant: ncovr global, usa_map as layer 1's local data
ggplot(ncovr) +
  geom_sf(data = usa_map) +
  geom_sf(aes(fill = POL60))
```

Layer/aes comparison passes already (data_source ignored). What is not checked:
that layer 1 of the reference uses `usa_map` AND layer 1 of the variant also
effectively draws from `usa_map`. A user calling `equiv_data(p1, p2, layer = 1)`
would need to know which index to pass, and would need to account for global vs
local lookup.

**Proposed addition**: `equiv_layer_data(p1, p2, layer_map = c(1L, 1L, 2L, 2L))`
— matches layers by position and compares each pair's effective data (resolving
global vs local). The `layer_map` argument handles non-1:1 correspondences.

#### b) Cross-geom stat equivalence

A plot using a stat-computing geom on raw data is not currently comparable to a
plot that pre-computes the same statistic and uses a simpler geom. Examples:

| Stat-computing form | Pre-computed form |
|---|---|
| `geom_count()` (stat = "sum") | `count() + geom_point(aes(size = n))` |
| `geom_bar()` (stat = "count") | `count() + geom_bar(stat = "identity")` |
| `geom_histogram()` (stat = "bin") | `cut() / tabulate() + geom_col()` |
| `stat_summary()` | pre-aggregated data + `geom_point()` / `geom_line()` |

For the `geom_bar` / `geom_bar(stat="identity")` + `count()` case, `equiv_layers`
and `equiv_aes` already pass (same geom name "bar"; aes subset check holds).
Only the data differs, and `equiv_data()` would fail. This is the "soft" variant.

For `geom_count()` vs `geom_point(aes(size=n))`, the geom names differ ("count"
vs "point"), so `equiv_layers()` fails outright. This is the "hard" variant
requiring a new canonicalisation rule.

**Proposed rule** (to be added at mode `"semantic"` or `"pedagogical"`):
`.rule_stat_geom_to_precomputed`:

1. Detect layers where `stat ∈ {"sum", "count", "bin"}`.
2. For each such layer, check whether the comparison plot has a corresponding
   layer whose data frame contains the computed variable (e.g. a column `n` for
   `stat_sum`/`stat_count`, or `count`/`after_stat(count)` for `stat_count`).
3. If the computed column matches, rewrite the spec to normalise both plots to
   the same geom/stat/aes form and record the equivalence in `$changes`.

This requires that the data frames involved in the comparison be accessible
(they are, via `p$data` and `p$layers[[i]]$data`), but involves non-trivial
matching logic. A phased approach:
- Phase 1: flag the pattern in `$changes` without normalising (detection only).
- Phase 2: full normalisation with data verification.

#### c) `examples/penguins_pairwise_count.R` — reference for cross-geom gap

This file is structured into two separate equivalence groups precisely because of
the cross-geom boundary: see the "Ground truth A / B" comments. Once a
`.rule_stat_geom_to_precomputed` rule exists, A and B can be unified under a
single comparison call.

---

## 2. Pedagogical mode extensions

### 2.1 `geom_histogram` `bins` ↔ `binwidth` numeric normalisation

Currently `.rule_histogram_bin_param` only *flags* which parameter was used.
A full normalisation would convert `bins = 30` to an approximate `binwidth` (or
vice versa) given the data range, allowing `equiv_params()` to pass for
"equivalent" binning choices. Requires the data to be available at canon time.

### 2.2 `after_stat()` mapping checks

`.rule_after_stat_flag` records `after_stat(density)` mappings in `$changes` but
does not yet expose a grading API. A function
`has_after_stat(p, aesthetic, stat_var)` (e.g. `has_after_stat(p, "y", "density")`)
would complement `mapping_exists()` for density-overlay histogram grading.

---

## 3. CRAN / package hygiene

### 3.1 `examples/` directory

The `examples/` directory at the package root is non-standard and invisible to
`R CMD check`. The content has been subsumed into:

- `vignettes/equivalence-patterns.Rmd` — pattern catalogue, working examples
- `vignettes/grading-with-ggspec.Rmd` — canonicalisation section added

**Recommended action**: move `examples/` to `inst/examples/` so the scripts
remain accessible via `system.file("examples", package = "ggspec")` but are
clearly non-executable reference material from CRAN's perspective. Alternatively
remove if vignettes cover all patterns.

### 3.2 Spatial / non-CRAN data (`geodaData`, `rnaturalearth`)

`examples/ncovr_layers_global_local.R` depends on `geodaData` (GitHub only) and
`rnaturalearth`. These cannot appear in `eval = TRUE` vignette chunks on CRAN.
Any vignette coverage of multi-dataset geom_sf patterns must use `eval = FALSE`
code blocks until this data dependency is resolved (e.g. by embedding a tiny
reproducible sf fixture in `inst/testdata/`).

---

## 4. Additional `spec_*` coverage

- `spec_theme()`: extract selected theme elements (text sizes, axis settings)
  for style-aware grading.
- `spec_guides()`: extract legend/guide configuration.
- `equiv_theme()`: compare theme elements, with tolerance for numeric values.
