## Rule: uncounted geom_count, counted geom_col & counted geom_count(stat = "identity") are all visually equivalent WHEN plotting the joint distribution of 2 discrete variables

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("geom_count without precounting and geom_point with precounting are strictly and structurally not equivalent, but visually and conceptually equivalent", {
  p1 <- make_2d_count_plot()
  p2 <- make_2d_point_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_equal(p1$data |> count(species, island), p2$data)
})

test_that("geom_count without precounting and geom_count with precounting and stat = 'identity' are strictly and structurally not equivalent, but visually and conceptually equivalent", {
  p1 <- make_2d_count_plot()
  p2 <- make_2d_count_identity_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_equal(p1$data |> count(species, island), p2$data)
})

test_that("geom_point and geom_count with stat = 'identity' are strictly and structurally not equivalent, but visually and conceptually equivalent", {
  p1 <- make_2d_point_plot()
  p2 <- make_2d_count_identity_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_equal(p1$data, p2$data)
})
