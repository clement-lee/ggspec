## Rule: geom_count & geom_jitter are conceptually similar WHEN plotting the joint distribution of 2 discrete variables

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("For 2 discrete variables, geom_count and geom_jitter are strictly, structurally and visually not equivalent, but conceptually equivalent", {
  p1 <- make_2d_count_plot()
  p2 <- make_jitter_2d_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})
