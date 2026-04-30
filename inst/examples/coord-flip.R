## Rule: mapping a variable to x axis and mapping the same variable to y axis with coord_flip(), are strictly and structurally not equivalent, but visually and conceptually equivalent.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("Having a variable on x-axis, or y-axis with flipped coordinates, are visually equivalent", {
  p1 <- make_bar_plot()
  p2 <- make_bar_y_flip_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## Rule: mapping variables 1 and 2 to x and y axes, respectively, and mapping variables 1 and 2 to y and x axes, respectively, with coord_flip(), are strictly and structurally not equivalent, but visually and conceptually equivalent.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("Having two variable on x and y axies, or the other way round with flipped coordinates, are visually equivalent", {
  p1 <- make_boxplot_plot()
  p2 <- make_boxplot_yx_flip_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})
