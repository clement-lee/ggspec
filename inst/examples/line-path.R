## Rule: geom_path & geom_line are visually equivalent WHEN the data are sorted (by the x variable)

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("geom_path and geom_line are strictly and structurally not equivalent, but visually and conceptually equivalent when the data are sorted by the x variable.", {
  p1 <- make_path_time_plot()
  p2 <- make_line_time_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
  expect_identical(p1$data |> arrange(date), p1$data)
})

## Rule: geom_path & geom_line are strictly, structurally, visually, and conceptually not equivalent WHEN the data are not sorted by the x variable.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: false

## Examples
test_that("geom_path and geom_line are strictly, structurally, visually, and conceptually not equivalent when the data are not sorted by the x variable.", {
  p1 <- make_path_notime_plot()
  p2 <- make_line_notime_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
  expect_false(identical(p1$data |> arrange(psavert), p1$data))
})
