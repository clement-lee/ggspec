
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
#> # A tibble: 2 × 8
#>   layer_idx geom   stat    position mapping params       inherit_aes data_source
#>       <int> <chr>  <chr>   <chr>    <list>  <list>       <lgl>       <chr>      
#> 1         1 point  identi… identity <chr>   <named list> TRUE        global     
#> 2         2 smooth smooth  identity <chr>   <named list> TRUE        global
spec_aes(p)
#> # A tibble: 5 × 5
#>   layer_idx geom   aesthetic variable source
#>       <int> <chr>  <chr>     <chr>    <chr> 
#> 1         1 point  x         displ    global
#> 2         1 point  y         hwy      global
#> 3         1 point  colour    class    local 
#> 4         2 smooth x         displ    global
#> 5         2 smooth y         hwy      global
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
#> [PASS] 6/6 checks passed
#>   Detail:
#> # A tibble: 5 × 12
#>   check  source layer_idx geom  stat     position aesthetic variable status
#>   <chr>  <chr>      <int> <chr> <chr>    <chr>    <chr>     <chr>    <chr> 
#> 1 layers ref            1 point identity identity <NA>      <NA>     <NA>  
#> 2 layers obs            1 point identity identity <NA>      <NA>     <NA>  
#> 3 aes    global         1 point <NA>     <NA>     x         displ    match 
#> 4 aes    global         1 point <NA>     <NA>     y         hwy      match 
#> 5 aes    local          1 point <NA>     <NA>     colour    class    match 
#> # ℹ 3 more variables: label_ref <chr>, label_obs <chr>, match <list>
equiv_plot(ref, obs_wrong)
#> [FAIL] 3/6 checks passed: Missing geom(s): point.; Aesthetic mapping issue(s): colour->class (layer 1).; Facet mismatch: cols: 'drv' vs 'cyl'
#>   Detail:
#> # A tibble: 5 × 12
#>   check  source layer_idx geom   stat     position aesthetic variable status 
#>   <chr>  <chr>      <int> <chr>  <chr>    <chr>    <chr>     <chr>    <chr>  
#> 1 layers ref            1 point  identity identity <NA>      <NA>     <NA>   
#> 2 layers obs            1 smooth smooth   identity <NA>      <NA>     <NA>   
#> 3 aes    local          1 point  <NA>     <NA>     colour    class    missing
#> 4 aes    global         1 point  <NA>     <NA>     x         displ    match  
#> 5 aes    global         1 point  <NA>     <NA>     y         hwy      match  
#> # ℹ 3 more variables: label_ref <chr>, label_obs <chr>, match <list>
```

### Enriching a spec with build-derived defaults

`enrich_spec()` uses `ggplot_build()` to identify which parameters and
aesthetics were explicitly set by the user versus filled in by ggplot2:

``` r
es <- enrich_spec(p)
#> `geom_smooth()` using formula = 'y ~ x'

# Non-aesthetic parameters with explicit flag
es$params_tbl[[1]]
#> # A tibble: 1 × 4
#>   param value     explicit source
#>   <chr> <list>    <lgl>    <chr> 
#> 1 na.rm <lgl [1]> FALSE    geom

# Aesthetics resolved by ggplot2, with explicit flag
es$built_aes[[1]]
#> # A tibble: 9 × 3
#>   aesthetic value       explicit
#>   <chr>     <list>      <lgl>   
#> 1 colour    <chr [234]> TRUE    
#> 2 x         <dbl [234]> TRUE    
#> 3 y         <dbl [234]> TRUE    
#> 4 group     <int [234]> FALSE   
#> 5 shape     <dbl [1]>   FALSE   
#> 6 fill      <lgl [1]>   FALSE   
#> 7 size      <dbl [1]>   FALSE   
#> 8 alpha     <lgl [1]>   FALSE   
#> 9 stroke    <dbl [1]>   FALSE
```

## Key functions

| Tier       | Function              | What it returns                       |
| ---------- | --------------------- | ------------------------------------- |
| Extraction | `spec_layers()`       | One row per layer                     |
| Extraction | `spec_aes()`          | One row per layer × aesthetic         |
| Extraction | `spec_scales()`       | One row per scale                     |
| Extraction | `spec_facets()`       | Facet type and variables              |
| Extraction | `spec_labels()`       | One row per label                     |
| Extraction | `spec_coord()`        | Coordinate system                     |
| Extraction | `enrich_spec()`       | spec\_layers + default/explicit flags |
| Comparison | `equiv_plot()`        | All checks in one call                |
| Comparison | `equiv_layers()`      | Geom and stat per layer               |
| Comparison | `equiv_aes()`         | Aesthetic mappings                    |
| Comparison | `compare_plots()`     | Canonicalise then compare             |
| Check      | `check_plot()`        | Framework-agnostic assertion          |
| Check      | `expect_equiv_plot()` | testthat expectation                  |

## Related packages

  - **[ggcheck](https://github.com/rstudio/ggcheck)** — designed for
    `learnr`/`gradethis` pipelines; returns ad-hoc objects. `ggspec`
    returns rectangular, pipeable tibbles and has no grading framework
    dependency.

## License

MIT
