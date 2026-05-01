## Rule: geom_point & geom_line are strictly, structurally, visually, and conceptually not equivalent WHEN visualising the relationship between 2 numeric variables (regardless of if one of them has a natural ordering, or if the data are ordered)

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: false

## Examples
test_that("geom_point & geom_line are strictly, structurally, visually, and conceptually not equivalent WHEN visualising the relationship between 2 numeric variables", {
  p1 <- make_point_plot()
  p2 <- make_line_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})
