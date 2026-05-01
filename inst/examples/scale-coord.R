## Rule: using scale_*_continuous and coord_cartesian are strictly and structurally not equivalent, but visually and conceptually equivalent WHEN there is no clipping of the data.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("Using scale_*_continuous and coord_cartesian are visually and conceptually equivalent when there is no clipping of the data", {
  p1 <- make_point_plot() + scale_x_continuous(limits = c(160, 240))
  p2 <- make_point_plot() + coord_cartesian(xlim = c(160, 240))
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

## Rule: using scale_*_continuous and coord_cartesian are strictly, structurally, and visually not equivalent but conceptually equivalent WHEN there is clipping of the data.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
test_that("Using scale_*_continuous and coord_cartesian are conceptually equivalent when there is clipping of the data", {
  p1 <- make_point_plot() + scale_x_continuous(limits = c(180, 230))
  p2 <- make_point_plot() + coord_cartesian(xlim = c(180, 230))
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

## Rule: using scale_*_continuous and coord_cartesian are strictly and structurally not equivalent, but visually and conceptually equivalent WHEN there is clipping of the data for the former and pre-filtering for the latter.

## Equivalent according to following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("Using scale_*_continuous and coord_cartesian are visually and conceptually equivalent when there is clipping of the data for the former and pre-filtering for the latter", {
  p1 <- make_point_plot() + scale_x_continuous(limits = c(180, 230))
  p2 <-
    penguins |>
    filter(flipper_len >= 180, flipper_len <= 230) |>
    ggplot(aes(x = flipper_len, y = body_mass)) +
      geom_point()
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data |> filter(flipper_len >= 180, flipper_len <= 230), p2$data)
})
