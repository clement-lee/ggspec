## Rule: density, dotplot, freqpoly, histogram, area(stat = "bin") are all conceptually equivalent WHEN visualising the distribution of 1 continuous variable.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("geom_density and geom_dotplot are strictly, structually, and visually not equivalent, but conceptually equivalent", {
  p1 <- make_density_plot()
  p2 <- make_dotplot_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## same for geom_density and geom_freqpoly
## same for geom_density and geom_histogram
## same for geom_density and geom_area(stat = "bin")
## same for geom_dotplot and geom_freqpoly
## same for geom_dotplot and geom_histogram
## same for geom_dotplot and geom_area(stat = "bin")
## same for geom_freqpoly and geom_histogram
## same for geom_freqpoly and geom_area(stat = "bin")
## same for geom_histogram and geom_area(stat = "bin")
