## Rule: geom_jitter, geom_point(position = "jitter"), and geom_point(position = position_jitter()) are strictly equivalent WHEN the same random seed is used.

## Equivalent according to following modes?
## strict: true
## structural: true
## visual: true
## conceptual: true

## Examples
test_that("geom_jitter and geom_point(position = 'jitter') are strictly, structurally, visually and conceptually equivalent.", {
  p1 <- make_jitter_plot()
  p2 <- make_point_jitter_plot()
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

test_that("geom_jitter and geom_point(position = position_jitter()) are strictly, structurally, visually and conceptually equivalent.", {
  p1 <- make_jitter_plot()
  p2 <- make_point_position_jitter_plot()
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

test_that("geom_point(position = 'jitter') and geom_point(position = position_jitter()) are strictly, structurally, visually and conceptually equivalent.", {
  p1 <- make_point_jitter_plot()
  p2 <- make_point_position_jitter_plot()
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

## Rule: geom_jitter, geom_point(position = "jitter"), and geom_point(position = position_jitter()) are strictly and visually not equivalent but structurally and conceptually equivalent WHEN different random seeds are used.

## Equivalent according to following modes?
## strict: false
## structural: true
## visual: false
## conceptual: true

## Examples
test_that("geom_jitter and geom_point(position = 'jitter') are structurally and conceptually equivalent WHEN different random seeds are used.", {
  p1 <- make_jitter_plot(478L)
  p2 <- make_point_jitter_plot(569L)
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

test_that("geom_jitter and geom_point(position = position_jitter()) are strictly, structurally, visually and conceptually equivalent.", {
  p1 <- make_jitter_plot(478L)
  p2 <- make_point_position_jitter_plot(569L)
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

test_that("geom_point(position = 'jitter') and geom_point(position = position_jitter()) are strictly, structurally, visually and conceptually equivalent.", {
  p1 <- make_point_jitter_plot(478L)
  p2 <- make_point_position_jitter_plot(569L)
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
})

