## Rule: mapping a variable to x axis and mapping the same variable to y axis, are only conceptually equivalent WHEN all other things are held constanct.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("Having a variable on x-axis or y-axis are conceptually equivalent", {
  p1 <- make_bar_plot()
  p2 <- make_bar_y_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## Rule: mapping variables 1 and 2 to x and y axes, respectively, and mapping variables 1 and 2 to y and x axes, respectively, are only conceptually equivalent.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("Having two variables on x and y axes and the other way round are conceptually equivalent", {
  p1 <- make_boxplot_plot()
  p2 <- make_boxplot_yx_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

