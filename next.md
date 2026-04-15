# Next steps for ggspec

## Status relative to ggcheck and gginnards

ggspec was motivated by two gaps in the existing ecosystem:

| Limitation | ggcheck | gginnards | ggspec |
|---|---|---|---|
| Hard dependency on learnr/gradethis | ✓ (required) | N/A | ✗ (fail_fn injection) |
| Returns tidy data frames | ✗ | ✗ | ✓ |
| Full declarative spec extraction | ✗ | partial | ✓ |
| Framework-agnostic grading assertion | ✗ | ✗ | ✓ |
| Canonicalisation (coord_flip, geom_col, etc.) | ✗ | ✗ | ✓ |
| Build-enriched default/explicit detection | ✗ | partial | ✓ (`enrich_spec`) |
| Theme extraction + comparison | ✗ | partial | ✗ pending |
| Guide/legend extraction + comparison | ✗ | ✗ | ✗ pending |
| Cross-geom stat equivalence | ✗ | ✗ | ✗ pending |

The core extraction, comparison, and grading-workflow tiers are feature-complete.
The remaining work is in three clusters: **new spec dimensions** (theme, guides),
**enrich_spec() integration**, and **advanced equivalence patterns**.

---

## 1. Multi-dataset semantic equivalence

### 1.1 Current state

| Capability | Status |
|---|---|
| `spec_layers()$data_source` marks `"global"` vs `"local"` per layer | ✓ done |
| `layer_data_index(p, data)` locates a data frame within a plot | ✓ done |
| `equiv_data(p, layer = i)` compares a single layer's data by hash | ✓ done |
| `equiv_layers()` / `equiv_aes()` ignore `data_source` by design | ✓ done |
| Plots differing only in where data is supplied (global vs per-layer) pass | ✓ done |

### 1.2 What is missing

#### a) Cross-plot layer data matching — `equiv_layer_data()`

`equiv_data()` takes a single layer index and operates **within one plot** (it
hashes the effective data for layer `i` of `p`). There is no function that
verifies that the i-th layer of `p1` draws from a semantically equivalent data
frame to the i-th layer of `p2`.

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

Layer/aes comparison passes (data_source ignored). What is not checked: that
layer 1 of the reference uses `usa_map` AND layer 1 of the variant also draws
from `usa_map`.

**Proposed addition**: `equiv_layer_data(p1, p2, layer_map = c(1L, 1L, 2L, 2L))`
— matches layers by position and compares each pair's effective data (resolving
global vs local). The `layer_map` argument handles non-1:1 correspondences.

---

## 2. Cross-geom stat equivalence

A plot using a stat-computing geom on raw data is not currently comparable to a
plot that pre-computes the same statistic and uses a simpler geom. Examples:

| Stat-computing form | Pre-computed form |
|---|---|
| `geom_count()` (stat = "sum") | `count() + geom_point(aes(size = n))` |
| `geom_bar()` (stat = "count") | `count() + geom_bar(stat = "identity")` |
| `geom_histogram()` (stat = "bin") | `cut()` / `tabulate()` + `geom_col()` |
| `stat_summary()` | pre-aggregated data + `geom_point()` / `geom_line()` |

For `geom_bar()` / `geom_bar(stat="identity")` + `count()`, `equiv_layers`
and `equiv_aes` already pass (same geom name "bar"; aes subset check holds).
Only the data differs, and `equiv_data()` would fail. This is the "soft" variant.

For `geom_count()` vs `geom_point(aes(size=n))`, the geom names differ ("count"
vs "point"), so `equiv_layers()` fails outright. This is the "hard" variant
requiring a new canonicalisation rule.

**Proposed rule** `.rule_stat_geom_to_precomputed` at mode `"semantic"` or
`"pedagogical"`:

1. Detect layers where `stat ∈ {"sum", "count", "bin"}`.
2. For each such layer, check whether the comparison plot has a corresponding
   layer whose data frame contains the computed variable (e.g. a column `n` for
   `stat_sum`/`stat_count`, or `count`/`after_stat(count)` for `stat_count`).
3. If the computed column matches, rewrite the spec to normalise both plots to
   the same geom/stat/aes form and record the equivalence in `$changes`.

This requires `enrich_spec()` / `equiv_data()` to be accessible from within the
canonicalisation pipeline — a non-trivial integration step. A phased approach:

- **Phase 1**: flag the pattern in `$changes` without normalising (detection only).
- **Phase 2**: full normalisation with data verification.

`examples/penguins_pairwise_count.R` is structured into two separate equivalence
groups precisely because of this cross-geom boundary; once
`.rule_stat_geom_to_precomputed` exists, A and B can be unified.

---

## 3. `enrich_spec()` integration

`enrich_spec()` (added) compares the spec against `ggplot_build()` output to
classify each param and aesthetic as **explicit** (user-specified) or **default**
(ggplot2 filled in). Currently it is a standalone query function. Three
integration tasks remain:

### 3.1 `equiv_explicit_params(p1, p2, layer = NULL)`

A comparison function that checks whether `p2` explicitly sets the same
non-default params that `p1` explicitly sets. Useful for grading calls like
`geom_smooth(se = FALSE)` — where the reference explicitly overrides a default
and the student must too.

Signature sketch:
```r
equiv_explicit_params(p_ref, p_student, layer = 1L)
# Passes iff all params with explicit=TRUE in p_ref's enrich_spec() are also
# explicit=TRUE in p_student's enrich_spec() with matching values.
```

### 3.2 Numeric tolerance in `equiv_params()`

`equiv_params()` currently uses `identical()` for value comparison, which fails
for floating-point params (e.g. `alpha = 0.5` vs `alpha = 0.5000001`). Add a
`tol` argument, defaulting to a small epsilon, for numeric params.

### 3.3 `explicit` flag propagation into `compare_plots()` / `canon()`

The `$changes` tibble from `canon()` records spec-level transformations. A
natural extension is to also record whether a transformed value was user-explicit
or a resolved default, making the diff more interpretable for instructors.

---

## 4. Pedagogical mode extensions

### 4.1 `geom_histogram` `bins` ↔ `binwidth` numeric normalisation

`.rule_histogram_bin_param` only *flags* which parameter was used (records in
`$changes`). A full normalisation would convert `bins = 30` to an approximate
`binwidth` (or vice versa) given the data range, allowing `equiv_params()` to
pass for "equivalent" binning choices. Requires the data to be available at
canon time — feasible via `enrich_spec()` which already calls `ggplot_build()`.

### 4.2 `has_after_stat()` grading API

`.rule_after_stat_flag` records `after_stat(density)` mappings in `$changes` but
does not expose a user-callable grading API. A function

```r
has_after_stat(p, aesthetic, stat_var)
# e.g. has_after_stat(p, "y", "density")
```

would complement `mapping_exists()` for density-overlay histogram grading.

---

## 5. New spec dimensions: theme and guides

Neither ggcheck nor gginnards provides a tidy extraction of theme elements or
guide configuration. Adding these to ggspec would complete the declarative spec
surface.

### 5.1 `spec_theme(p, elements = NULL)`

Extract selected theme elements as a tidy data frame. Columns: `element` (chr,
e.g. `"axis.title"`, `"panel.grid.major"`), `property` (chr, e.g. `"size"`,
`"colour"`, `"linetype"`), `value` (list).

Scope: start with the elements most commonly set in teaching contexts —
`axis.title`, `axis.text`, `plot.title`, `legend.position`, `panel.background`,
`panel.grid.*`.

### 5.2 `equiv_theme(p1, p2, elements = NULL, tol = 1e-6)`

Compare theme elements between two plots, with numeric tolerance for size values
(points vs mm ambiguity). The `elements` argument mirrors the `aesthetics`
argument in `equiv_labels()`.

### 5.3 `spec_guides(p)`

Extract the guide (legend/colourbar/axis-guide) configuration per aesthetic as a
tidy data frame. Columns: `aesthetic`, `guide_type` (chr), `title` (chr),
`position` (chr or NA), `nrow` / `ncol` (for legend guides).

---

## 6. CRAN / package hygiene

### 6.1 `examples/` directory

The `examples/` directory at the package root is non-standard and invisible to
`R CMD check`. Content has been subsumed into vignettes. The directory should be
moved to `inst/examples/` so the scripts remain accessible via
`system.file("examples", package = "ggspec")`.

**Action**: `git mv examples/ inst/examples/` — the `~` (backup) files already
present in `inst/examples/` should be removed.

### 6.2 Spatial / non-CRAN data (`geodaData`, `rnaturalearth`)

`examples/ncovr_layers_global_local.R` depends on `geodaData` (GitHub only) and
`rnaturalearth`. Any vignette coverage of multi-dataset `geom_sf` patterns must
use `eval = FALSE` code blocks until this data dependency is resolved (e.g. by
embedding a tiny reproducible `sf` fixture in `inst/testdata/`).

### 6.3 `.gitignore` / backup files

`inst/examples/` contains `*.R~` editor backup files that should be added to
`.gitignore` or deleted before any CRAN submission.
