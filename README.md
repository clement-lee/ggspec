
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggspec

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/clement-lee/ggspec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/clement-lee/ggspec/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/clement-lee/ggspec/graph/badge.svg)](https://app.codecov.io/gh/clement-lee/ggspec)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggspec)](https://CRAN.R-project.org/package=ggspec)
<!-- badges: end -->

`ggspec` extracts the full declarative specification of a `ggplot2`
object — layers, aesthetic mappings, scales, facets, coordinate system,
and labels — as tidy data frames. A second tier of functions enables
structural comparison of two ggplot objects, supporting automated plot
testing, auditing, and framework-agnostic grading workflows.

## Motivation

Different large-language models and AI coding assistants generate
syntactically different code for the same visualisation task. One AI
might write `geom_bar(aes(x = species))` on raw data; another might
write `count(species) |> ... geom_col(aes(x = species, y = n))`. Both
produce the same chart, but naive string or AST comparison would flag
them as different. `ggspec` provides a principled hierarchy of
equivalence checks — from strict spec equality through structural
canonicalisation to rendered-output comparison — so that equivalent
plots are recognised as equivalent regardless of which syntactic path an
AI (or human student) took to produce them.

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("clement-lee/ggspec")
```

## Usage

### Extracting a spec

``` r
library(ggspec)
library(ggplot2)
#> 
#> Attaching package: 'ggplot2'
#> The following object is masked from 'package:ggspec':
#> 
#>     is_ggplot

p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~drv) +
  labs(title = "Engine displacement vs highway MPG")

spec_layers(p)
#> # A tibble: 3 × 8
#>   layer geom   stat     position mapping   params           inherit_aes data_id
#>   <int> <chr>  <chr>    <chr>    <list>    <list>           <lgl>         <int>
#> 1     0 <NA>   <NA>     <NA>     <chr [2]> <list [0]>       NA                1
#> 2     1 point  identity identity <chr [3]> <named list [2]> TRUE             NA
#> 3     2 smooth smooth   identity <chr [2]> <named list [7]> TRUE             NA
spec_aes(p)
#> # A tibble: 7 × 5
#>   layer geom   aesthetic variable source
#>   <int> <chr>  <chr>     <chr>    <chr> 
#> 1     0 <NA>   x         displ    global
#> 2     0 <NA>   y         hwy      global
#> 3     1 point  x         displ    global
#> 4     1 point  y         hwy      global
#> 5     1 point  colour    class    local 
#> 6     2 smooth x         displ    global
#> 7     2 smooth y         hwy      global
```

### Comparing two plots

``` r
ref <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  facet_wrap(~drv)

obs_correct <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  facet_wrap(~drv)

obs_wrong <- ggplot(mpg, aes(displ, hwy)) +
  geom_smooth() +
  facet_wrap(~cyl)

equiv_plot(ref, obs_correct)
#> [PASS mode=strict] 6/6 checks passed
#>   Detail:
#> # A tibble: 9 × 12
#>   check  source layer geom  stat    position aesthetic variable status label_ref
#>   <chr>  <chr>  <int> <chr> <chr>   <chr>    <chr>     <chr>    <chr>  <chr>    
#> 1 layers ref        0 <NA>  <NA>    <NA>     <NA>      <NA>     <NA>   <NA>     
#> 2 layers ref        1 point identi… identity <NA>      <NA>     <NA>   <NA>     
#> 3 layers obs        0 <NA>  <NA>    <NA>     <NA>      <NA>     <NA>   <NA>     
#> 4 layers obs        1 point identi… identity <NA>      <NA>     <NA>   <NA>     
#> 5 aes    global     0 <NA>  <NA>    <NA>     x         displ    match  <NA>     
#> 6 aes    global     0 <NA>  <NA>    <NA>     y         hwy      match  <NA>     
#> 7 aes    global     1 point <NA>    <NA>     x         displ    match  <NA>     
#> 8 aes    global     1 point <NA>    <NA>     y         hwy      match  <NA>     
#> 9 aes    local      1 point <NA>    <NA>     colour    class    match  <NA>     
#> # ℹ 2 more variables: label_obs <chr>, match <list>
equiv_plot(ref, obs_wrong)
#> [FAIL mode=strict] 3/6 checks passed: Missing geom(s): point.; Aesthetic mapping issue(s): colour->class (layer 1).; Facet mismatch: cols: 'drv' vs 'cyl'
#>   Detail:
#> # A tibble: 9 × 12
#>   check  source layer geom   stat   position aesthetic variable status label_ref
#>   <chr>  <chr>  <int> <chr>  <chr>  <chr>    <chr>     <chr>    <chr>  <chr>    
#> 1 layers ref        0 <NA>   <NA>   <NA>     <NA>      <NA>     <NA>   <NA>     
#> 2 layers ref        1 point  ident… identity <NA>      <NA>     <NA>   <NA>     
#> 3 layers obs        0 <NA>   <NA>   <NA>     <NA>      <NA>     <NA>   <NA>     
#> 4 layers obs        1 smooth smooth identity <NA>      <NA>     <NA>   <NA>     
#> 5 aes    local      1 point  <NA>   <NA>     colour    class    missi… <NA>     
#> 6 aes    global     0 <NA>   <NA>   <NA>     x         displ    match  <NA>     
#> 7 aes    global     0 <NA>   <NA>   <NA>     y         hwy      match  <NA>     
#> 8 aes    global     1 point  <NA>   <NA>     x         displ    match  <NA>     
#> 9 aes    global     1 point  <NA>   <NA>     y         hwy      match  <NA>     
#> # ℹ 2 more variables: label_obs <chr>, match <list>
```

## Comparison modes

`ggspec` recognises four levels of plot equivalence or similarity,
ordered from most to least restrictive.

### Strict equivalence

Two plots are **strictly equivalent** when their specifications are
identical with no canonicalisation applied: same layer order, same
data/mapping placement, same geom names, same random seed for stochastic
elements.

``` r
compare_plots(p1, p2, mode = "strict")
```

### Structural equivalence

Two plots are **structurally equivalent** when their specifications are
identical after canonicalisation via `canon()`. The canonical form is
computed by a term rewriting system (TRS) that applies a fixed set of
confluent rewrite rules:

  - **fold\_global**: resolves the ambiguity of placing data/mapping at
    the global level vs per-layer — both forms normalise to the same
    spec.
  - **geom\_col\_to\_bar**: `geom_col()` is a shorthand for
    `geom_bar(stat = "identity")`; the canonical form always uses the
    latter.
  - **layer\_order**: non-spanning geoms are sorted alphabetically, so
    layer order does not affect structural equivalence.

<!-- end list -->

``` r
compare_plots(p1, p2, mode = "structural")  # default
```

### Visual equivalence

Two plots are **visually equivalent** when they produce identical
rendered output. This pathway uses `ggplot_build()` to evaluate plots
semantically rather than comparing their specs, so it can detect
equivalences that structural comparison cannot:

  - `geom_bar()` on raw data vs `geom_col()` on pre-counted data (same
    bars, different specs).
  - `coord_flip()` vs swapped aesthetics (same visual output, different
    coordinate systems).
  - `scale_fill_*(name = "v")` vs `labs(fill = "v")` (same legend label,
    different spec location).

<!-- end list -->

``` r
compare_plots(p1, p2, mode = "visual")
```

**Design constraint**: visual equivalence calls `ggplot_build()` on both
plots. This means (a) both plots must be buildable with their data
accessible in the session, (b) it is slower than structural comparison,
and (c) it is output-based — it does not verify that the plots were
derived from the same source data. Two plots backed by different
datasets that happen to produce the same rendered output will pass
visual equivalence. Use structural mode when data provenance must be
verified.

### Conceptual similarity

Two plots are **conceptually similar** when they communicate the same
information using potentially different visual encodings. Unlike the
equivalence modes above, conceptual similarity is not a strict
mathematical equivalence relation; each claim is qualified by a WHEN
condition:

| Claim                                      | WHEN                               |
| ------------------------------------------ | ---------------------------------- |
| boxplot ≡ violin ≡ jitter                  | 1 continuous + 1 discrete variable |
| density ≡ histogram ≡ freqpoly ≡ dotplot   | 1 continuous variable              |
| `geom_count` ≡ `geom_point(aes(size = n))` | 2 discrete variables, joint counts |

``` r
compare_plots(p1, p2, mode = "conceptual")
```

### Enriching a spec with build-derived defaults

`enrich_spec()` uses `ggplot_build()` to identify which parameters and
aesthetics were explicitly set by the user versus filled in by ggplot2:

``` r
es <- enrich_spec(p)
#> `geom_smooth()` using formula = 'y ~ x'

# Non-aesthetic parameters with explicit flag
es$params_tbl[[1]]
#> # A tibble: 0 × 4
#> # ℹ 4 variables: param <chr>, value <list>, explicit <lgl>, source <chr>

# Aesthetics resolved by ggplot2, with explicit flag
es$built_aes[[1]]
#> # A tibble: 0 × 3
#> # ℹ 3 variables: aesthetic <chr>, value <list>, explicit <lgl>
```

## Key functions

| Tier       | Function               | What it returns                         |
| ---------- | ---------------------- | --------------------------------------- |
| Extraction | `spec_layers()`        | One row per layer                       |
| Extraction | `spec_aes()`           | One row per layer × aesthetic           |
| Extraction | `spec_scales()`        | One row per scale                       |
| Extraction | `spec_facets()`        | Facet type and variables                |
| Extraction | `spec_labels()`        | One row per label                       |
| Extraction | `spec_coord()`         | Coordinate system                       |
| Extraction | `enrich_spec()`        | spec\_layers + default/explicit flags   |
| Comparison | `equiv_plot()`         | All checks in one call (strict)         |
| Comparison | `equiv_layers()`       | Geom and stat per layer                 |
| Comparison | `equiv_aes()`          | Aesthetic mappings                      |
| Comparison | `compare_plots()`      | Four-mode comparison entry point        |
| Comparison | `compare_visual()`     | Visual equivalence via `ggplot_build()` |
| Comparison | `compare_conceptual()` | Conceptual similarity detectors         |
| Comparison | `equiv_rendered()`     | Rendered layer data comparison          |
| Check      | `check_plot()`         | Framework-agnostic assertion            |
| Check      | `expect_equiv_plot()`  | testthat expectation                    |

## Related packages

  - **[ggcheck](https://github.com/rstudio/ggcheck)** — designed for
    `learnr`/`gradethis` pipelines; returns ad-hoc objects. `ggspec`
    returns rectangular, pipeable tibbles and has no grading framework
    dependency.

## License

MIT
