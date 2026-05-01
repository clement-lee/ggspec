## Rule: geom_point & geom_path are strictly, structurally, visually, and conceptually not equivalent WHEN visualising the relationship between 2 numeric variables, and there's no natural ordering in the data

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: false

## Examples
test_that("geom_point & geom_path are strictly, structurally, visually, and conceptually not equivalent when visualising the relationship between 2 numeric variables and there's no natural ordering in the data", {
  p1 <- make_point_plot()
  p2 <- make_path_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## Rule: geom_point & geom_path are strictly, structurally and visually not equivalent, but conceptually similar, WHEN visualising the relationship between 2 numeric variables, and the data are sorted according to an ordering (for which the meaningfulness is down to user judgment), that doesn't necessarily need to be one of the 2 variables

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("geom_point & geom_path are strictly, structurally, and visually not equivalent but conceptually similar when visualising the relationship between 2 numeric variables, and the data are sorted according to an ordering", {
  p1 <- make_point_notime_plot()
  p2 <- make_path_notime_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict", ordered = TRUE))) # data ordered by some variable, which doesn't necessarily need to be one of the 2 variables visualised
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural", ordered = TRUE)))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual", ordered = TRUE)))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual", ordered = TRUE)))
})
