## Variables:
## To visualise:

## Description: two data sources in different order are conceptually & structurally equivalent
## Equivalent according to the following modes?
## strict: false
## structural: true
## visual: false
## conceptual: true

## Examples
## multivariate data
test_that("Cars93 overlaying mpg and mpg overlaying Cars93 are strictly and visually not equivalent, but structurally and conceptually equivalent", {
  p1 <- plot_mpg_then_Cars93()
  p2 <- plot_Cars93_then_mpg()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## sf data
