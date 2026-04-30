## Rule: 2 geoms in different orders are structurally and visually equivalent WHEN neither of them is spanning

## Equivalent according to following modes?
## strict: false
## structural: true
## visual: true
## conceptual: true

## Examples
test_that("geom_point + geom_line is structurally and visually equivalent to geom_line + geom_point", {
  p1 <- plot_point_then_line()
  p2 <- plot_line_then_point()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## Rule: 2 geoms in different orders are structurally but not visually equivalent WHEN at least one of them is spanning

## Equivalent according to following modes?
## strict: false
## structural: true
## visual: false
## conceptual: true

## Examples
test_that("geom_point + geom_smooth is structurally but not visually equivalent to geom_smooth + geom_point", {
  p1 <- plot_point_then_smooth()
  p2 <- plot_smooth_then_point()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})
