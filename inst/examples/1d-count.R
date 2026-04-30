## Variables: 1 discrete
## To visualise: the counts

## Description: uncounted geom_bar, counted geom_col & counted geom_bar(stat = "identity") are all visually equivalent
## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("geom_bar without precounting and geom_col with precounting are strictly and structurally not equivalent, but visually and conceptually equivalent", {
  p1 <- make_bar_plot()
  p2 <- make_col_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_equal(p1$data |> count(species), p2$data)
})

test_that("geom_bar without precounting and geom_bar with precounting and stat = 'identity' are strictly and structurally not equivalent, but visually and conceptually equivalent", {
  p1 <- make_bar_plot()
  p2 <- make_bar_identity_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_equal(p1$data |> count(species), p2$data)
})

test_that("geom_col and geom_bar with stat = 'identity' are strictly and structurally not equivalent, but visually and conceptually equivalent", {
  p1 <- make_col_plot()
  p2 <- make_bar_identity_plot()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_equal(p1$data, p2$data)
})
