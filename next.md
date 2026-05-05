# Next steps for ggspec

## Status relative to ggcheck and gginnards

ggspec was motivated by two gaps in the existing ecosystem:

| Limitation | ggcheck | gginnards | ggspec |
|---|---|---|---|
| Hard dependency on learnr/gradethis | ✓ (required) | N/A | ✗ (fail_fn injection) |
| Returns tidy data frames | ✗ | ✗ | ✓ |
| Full declarative spec extraction | ✗ | partial | ✓ |
| Framework-agnostic grading assertion | ✗ | ✗ | ✓ |
| Canonicalisation (coord_flip, geom_col, etc.) | ✗ | ✗ | ✓ (structural) |
| Build-enriched default/explicit detection | ✗ | partial | ✓ (`enrich_spec`) |
| Visual equivalence (ggplot_build comparison) | ✗ | ✗ | ✓ (`compare_visual`) |
| Conceptual similarity detection | ✗ | ✗ | ✓ (`compare_conceptual`) |
| `..var..` / `after_stat()` normalisation | ✗ | ✗ | ✗ pending |
| Scale-transform vs mapping-transform equivalence | ✗ | ✗ | ✗ pending |
| Theme extraction + comparison | ✗ | partial | ✗ pending |
| Guide/legend extraction + comparison | ✗ | ✗ | ✗ pending |
| Cross-geom stat equivalence (structural) | ✗ | ✗ | ✗ pending (visual ✓) |

The core extraction, comparison, and grading-workflow tiers are feature-complete.
The following edge cases have been verified as already handled:
- **Positional vs named `aes()`** (`aes(displ, hwy)` vs `aes(x=displ, y=hwy)`): ggplot2
  normalises to named form at construction time; `spec_aes()` sees identical output.
- **`colour` vs `color` aliases**: ggplot2 normalises `color` → `colour` at construction
  time; `p$mapping` stores only `colour`.
- **`group` aesthetic**: treated as a regular aesthetic by `spec_aes()`; global vs local
  group mapping is transparent via the standard inheritance resolution.
- **Facets**: captured by `spec_facets()` and compared by `equiv_facets()`;
  `compare_plots()` includes them in all check vectors.
- **Multiple datasets (global vs per-layer)**: `spec_layers()$data_source` records the
  placement but all `equiv_*` functions ignore it by design.
- **coord_flip vs swapped aesthetics**: `compare_visual()` via `.norm_coord_flip()` handles
  this at the ggplot-object level before rendering; not available in structural mode.
- **scale name vs labs()**: `compare_visual()` via `.norm_scale_names()` absorbs scale
  `name` arguments into `p$labels` before comparison.
- **guide title vs scale name vs labs()**: `.norm_guide_labels()` absorbs
  `guides(aes = guide_legend("Title"))` into `p$labels`; visual mode only.
- **theme(axis.title.* = element_blank()) vs labs(x = NULL)**: `.norm_theme_labels()`
  propagates element_blank label removals into `p$labels`; visual mode only.
- **scale_x_continuous(name = NULL) vs labs(x = NULL)**: `.norm_scale_names()` propagates
  NULL scale names into `p$labels`; visual mode only.
- **Path-order sensitivity in equiv_rendered()**: path-type geoms (path, line, area,
  ribbon, step) are compared in data-frame row order rather than sorted order, so
  `geom_path` (connects in data order) and `geom_line` (sorts by x) are distinguished.
- **Non-spanning layer reordering**: `compare_visual()` sorts both plots' layers
  alphabetically by geom name when no spanning layer (geom_smooth with se=TRUE) is
  present, making `geom_smooth(se=FALSE) + geom_point` visually equivalent to its
  reverse.
- **Prescriptive `$hint` field**: all `equiv_*()` failure results carry a `$hint`
  character scalar naming the smallest change that would make the comparison pass.

**Design constraint — plot rendering required for visual mode**: All normalisation rules
in `compare_visual()` (`.norm_coord_flip`, `.norm_scale_names`, `.norm_guide_labels`,
`.norm_theme_labels`, `.sort_layers_by_geom`) and the data comparison in
`equiv_rendered()` call `ggplot_build()`, which evaluates the plot's stat pipeline and
renders all layers.  This imposes three constraints:

1. **Mode restriction**: any canonicalisation rule that requires rendering can only
   appear in `compare_visual()` / visual mode or higher.  Strict and structural modes
   operate solely on the stored spec (data-free, no `ggplot_build()` call).
2. **Buildability**: the plots being compared must be buildable — their datasets must
   be accessible in the current R session and all required packages loaded.
3. **Performance**: visual comparison is substantially slower than structural
   comparison because `ggplot_build()` must be called twice.  For high-throughput
   grading pipelines, prefer structural mode when it covers the needed equivalences.

The remaining work is in clusters covered in sections 1–7:
- **§1** Multi-dataset layer verification (`equiv_layer_data`, `equiv_layer_order_visual`)
- **§2** Cross-geom stat equivalence at structural level
- **§3** `enrich_spec()` integration (`equiv_explicit_params`, numeric tolerance, default params rule)
- **§4** Pedagogical extensions (`has_after_stat`, `..var..` normalisation)
- **§5** Scale-transform vs mapping-transform equivalence
- **§6** New spec dimensions (theme, guides)
- **§7** CRAN hygiene

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

### 1.3 Visual equivalence of differently-ordered layers (`equiv_layer_order_visual`)

For multi-dataset plots (e.g. two `geom_sf` layers with different datasets),
switching layer order is:

- **Visually equivalent** when the rendered regions do not overlap (neither
  layer occludes the other — typical of non-overlapping spatial datasets).
- **NOT visually equivalent** when the rendered regions overlap — z-ordering
  determines what is visible on top.

Detection requires comparing the rendered bounding boxes or point positions of
each layer after `ggplot_build()`. This cannot be determined from the spec alone.

**Proposed function** `equiv_layer_order_visual(p1, p2)`:
1. Call `ggplot_build()` to get rendered data per layer.
2. For each pair of layers (one from p1, one from p2), check whether their
   rendered regions overlap:
   - **Point geoms**: check if any (x, y) pairs coincide in both layers.
   - **Polygon/SF geoms**: check bounding-box intersection; for exact overlap
     detection, require `sf::st_intersects()`.
3. If no overlap: affirm visual equivalence of the reordered plots.
4. If overlap detected: visual equivalence fails.

**Implementation notes**:
- Phase 1: implement for point geoms with exact (x, y) coordinate matching.
- Phase 2: polygon/SF support via `sf::st_intersects()`; wrap in
  `skip_if_not_installed("sf")` in tests.
- Only the simple (point) case is data-independent of geom type; polygon
  intersection testing needs `sf` as a soft dependency.

---

## 2. Cross-geom stat equivalence

### 2.0 What is now handled

`compare_visual()` / `equiv_rendered()` now handles cross-geom stat equivalence
at the **visual level**: `ggplot_build()` evaluates both the stat-computing and
pre-computed forms, and `equiv_rendered()` compares the resulting panel data
frame by frame. The following pairs are therefore already visually equivalent:

| Stat-computing form | Pre-computed form | Status |
|---|---|---|
| `geom_count()` | `count() + geom_point(aes(size = n))` | ✓ visual |
| `geom_bar()` (stat="count") | `count() + geom_col()` | ✓ visual |
| `geom_bar()` (stat="count") | `count() + geom_bar(stat="identity")` | ✓ visual |

**Variable-name mismatch caveat**: when the pre-computed plot uses a column name
that differs from the stat-computed default (e.g. `count(species, name="count")`
→ `y = count` instead of `y = n`), the rendered bar heights are identical but the
axis labels differ. Visual equivalence then requires **both** plots to have
explicitly set the same axis label (via `labs(y=...)` or `scale_y_continuous(name=...)`).
Without explicit matching labels, `equiv_labels()` will report a difference.

### 2.1 What remains: structural-level cross-geom equivalence

Cross-geom equivalence is NOT yet handled at the structural level (`canon()`).
The remaining gap is a spec-level rewrite rule:

A plot using a stat-computing geom on raw data is not currently comparable to a
plot that pre-computes the same statistic and uses a simpler geom. Examples that
structural comparison fails on today:

| Stat-computing form | Pre-computed form |
|---|---|
| `geom_count()` (stat = "sum") | `count() + geom_point(aes(size = n))` |
| `geom_bar()` (stat = "count") | `count() + geom_bar(stat = "identity")` |
| `geom_histogram()` (stat = "bin") | `cut()` / `tabulate()` + `geom_col()` |
| `stat_summary()` | pre-aggregated data + `geom_point()` / `geom_line()` |

Known limitation in current `equiv_layers()` (non-exact mode): the subset check
matches geom name only and does not verify that the stat matches. This means
`geom_bar(stat="count")` and `geom_bar(stat="identity")` are incorrectly reported
as structurally equivalent. Fix: non-exact mode should also verify stat matches
when comparing layers of the same geom.

For `geom_count()` vs `geom_point(aes(size=n))`, the geom names differ ("count"
vs "point"), so `equiv_layers()` fails outright. This is the "hard" variant
requiring a new canonicalisation rule.

**Proposed rule** `.rule_stat_geom_to_precomputed` at a new `"semantic"` mode:

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

`inst/examples/2d-count.R` illustrates the two separate equivalence groups
(Group A: `geom_count()` variants; Group B: `count() + geom_point(aes(size=n))`
variants) precisely because of this cross-geom boundary.  Once
`.rule_stat_geom_to_precomputed` exists, A and B can be unified.

Note: any such rule requires access to the plot's built data and therefore can
only appear in visual mode or higher — not in structural mode (see design
constraint in the header of this file).

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

### 3.4 `.rule_explicit_default_params` — normalise away ggplot2 defaults

A canonical rule that removes parameters whose value equals the ggplot2 default,
determined via `enrich_spec()`. This would make:

```r
geom_histogram()          # bins param absent  →  canonical: params = {}
geom_histogram(bins = 30) # bins = 30 = default → canonical: params = {}
```

structurally equivalent. Currently they differ at spec level because the second
explicitly sets `bins = 30`.

**Design notes**:
- Requires `enrich_spec()` (calls `ggplot_build()`) to identify which param values
  are equal to the ggplot2 defaults — hence this rule can only appear in a mode
  that allows building, NOT in strict/structural.
- Appropriate mode: a future `"semantic"` mode, or as an optional flag on
  `canon(mode = "structural", remove_explicit_defaults = TRUE)`.
- Applies to: `bins = 30` (histogram), `se = TRUE` (smooth), `alpha = 1` (most
  geoms), `size = 0.5` (line geoms), etc.
- **Not** safe at structural level without data, because the default for some
  params is data-dependent (e.g. `binwidth` default depends on `diff(range(x))/30`).

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

### 4.3 `..var..` → `after_stat(var)` normalisation

ggplot2 supports two syntaxes for computed-variable mappings:

```r
geom_histogram(aes(y = ..density..))      # old dot-dot syntax
geom_histogram(aes(y = after_stat(density)))  # current syntax
```

Both are semantically identical; ggplot2 deprecated `..density..` in favour of
`after_stat(density)`. However, `spec_aes()` stores whichever string the user
wrote, so `equiv_aes()` fails when one plot uses the old syntax and the other
uses the new:

```r
spec_aes(p_dot)$variable   # "..density.."
spec_aes(p_ast)$variable   # "after_stat(density)"
equiv_aes(p_dot, p_ast)    # FAIL — string mismatch
```

**Proposed rule** `.rule_dot_dot_to_after_stat` at mode `"structural"` (or
`"strict"`):

1. For every aesthetic variable string of the form `..varname..`, replace it
   with `after_stat(varname)` using a simple regex substitution.
2. Record each substitution in `$changes`.

This rule is a pure string rewrite with no data dependency and should be
promoted to `"structural"` mode since it is a purely cosmetic syntactic
difference with no semantic content.

---

## 5. Scale-transform vs mapping-transform equivalence

`scale_x_log10()` and `aes(x = log10(displ))` produce identical rendered output
but are stored differently:

```r
# Scale-transform form
spec_aes(p_scale)     # variable = "displ"
spec_scales(p_scale)  # transform = "log-10"

# Mapping-transform form
spec_aes(p_map)       # variable = "log10(displ)"
spec_scales(p_map)    # (no scale row)
```

`equiv_aes()` therefore fails: `"displ"` ≠ `"log10(displ)"`.

`enrich_spec()` can *detect* the equivalence: for `p_scale`, `built_aes$x$value`
contains the log-transformed numerics, while `spec_aes()` still shows `"displ"`.
The "transformed" category from the spec/build comparison (quantity in spec with
a value that differs in build) is exactly this case.

**Proposed rule** `.rule_scale_transform_to_mapping` at mode `"visual"`:

1. For each continuous scale with a non-identity transform (from `spec_scales()`),
   locate the aesthetic it controls.
2. Build the equivalent mapping expression (e.g. `scale_x_log10` → `"log10(x_var)"`).
3. Replace the plain variable string with the transformed expression in `spec$mapping`
   and `spec$aes_long`, and record the scale's transform as having been absorbed.
4. The inverse direction (mapping expression → scale) requires expression parsing;
   limit Phase 1 to the scale → mapping direction only.

**Complexity note**: only a small set of transforms has a well-defined inverse
mapping expression (`log10`, `log`, `sqrt`, `exp`). Arbitrary `scales::trans_new()`
transforms cannot be handled without user-supplied mapping templates. Phase 1
should handle the four common cases and leave the rest uncanonicalised.

**Related — variable-name mismatch in pre-computed data**: a separate issue arises
when two pre-computed plots use different column names for the same quantity (e.g.
`count(species)` → column `n`; `count(species, name = "count")` → column `count`).
The rendered bar heights are identical but `equiv_aes()` fails (`y = n` vs
`y = count`). Visual equivalence holds only when both plots have explicitly set
the same axis label (via `labs(y = ...)` or `scale_y_continuous(name = ...)`);
without matching labels, `equiv_labels()` reports a mismatch. There is no
spec-level rule that can resolve column-name differences without accessing the
actual data — this is inherently a visual or data-dependent check.

---

## 6. New spec dimensions: theme and guides

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

## 7. CRAN / package hygiene

### 6.1 `examples/` directory ✓ done

Content moved to `inst/examples/`.  Bare-code scripts without `test_that()`
assertions have been progressively deleted as their patterns were subsumed into
`tests/testthat/`.

### 6.2 Spatial / non-CRAN data (`geodaData`, `rnaturalearth`) — updated

`ncovr_layers_global_local.R` has been deleted.  The multi-dataset `geom_sf`
pattern (layer-order equivalence with and without region overlap) is now stubbed
in `inst/examples/2data.R`.  Any vignette coverage of this pattern must use
`eval = FALSE` blocks until a self-contained `sf` fixture is embedded in
`inst/testdata/`.

### 6.3 `.gitignore` / backup files ✓ done

Editor backup files (`*.R~`) removed from `inst/examples/`.
