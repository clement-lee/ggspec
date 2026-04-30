## Rule: boxplot, violin, jitter are all conceptually equivalent WHEN visualising the distribution of 1 continuous variable by 1 discrete variable.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("geom_boxplot and geom_violin are strictly, structually, and visually not equivalent, but conceputally similar", {
  p1 <- make_boxplot_plot()
  p2 <- make_violin_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

test_that("geom_boxplot and geom_jitter are strictly, structually, and visually not equivalent, but conceputally similar", {
  p1 <- make_boxplot_plot()
  p2 <- make_jitter_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

test_that("geom_violin and geom_jitter are strictly, structually, and visually not equivalent, but conceputally similar", {
  p1 <- make_violin_plot()
  p2 <- make_jitter_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})


